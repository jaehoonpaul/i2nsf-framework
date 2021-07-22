#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <sys/poll.h>

#include <confd_lib.h>
#include <confd_events.h>
#include <confd_cdb.h>
#include <confd_logsyms.h>

#define OK(E) assert((E) == CONFD_OK)

static int notifsock, subsock;
static char *progname;

static int get_notifsock(struct addrinfo *addr)
{
    int sock;

    sock = socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
    if (sock < 0)
        return -1;
    if (confd_notifications_connect(sock,
                                    addr->ai_addr, addr->ai_addrlen,
                                    CONFD_NOTIF_UPGRADE_EVENT) != CONFD_OK) {
        close(sock);
        return -1;
    }
    return sock;
}

static int get_subsock(struct addrinfo *addr)
{
    int sock;

    sock = socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
    if (sock < 0)
        return -1;
    if (cdb_connect(sock, CDB_SUBSCRIPTION_SOCKET,
                    addr->ai_addr, addr->ai_addrlen) != CONFD_OK) {
        close(sock);
        return -1;
    }
    return sock;
}

static enum cdb_iter_ret iter(confd_hkeypath_t *kp,
                              enum cdb_iter_op op, confd_value_t *oldv,
                              confd_value_t *newv, void *state)
{
    char path[BUFSIZ], value[BUFSIZ];
    struct confd_cs_node *csp;

    if (op == MOP_VALUE_SET) {
        confd_pp_kpath(path, sizeof(path), kp);
        csp = confd_find_cs_node(kp, kp->len);
        confd_val2str(csp->info.type, newv, value, sizeof(value));
        fprintf(stderr, "%s got VALUE_SET %s -> %s\n", progname, path, value);
    }
    return ITER_RECURSE;
}

int main(int argc, char **argv)
{
    char confd_port[16];
    struct addrinfo hints;
    struct addrinfo *addr = NULL;
    int debuglevel = CONFD_TRACE;
    int i, c;
    int spoint;
    struct pollfd set[2];
    struct confd_notification notif;

    while ((c = getopt(argc, argv, "dtp")) != -1) {
        switch (c) {
        case 'd':
            debuglevel = CONFD_DEBUG;
            break;
        case 't':
            debuglevel = CONFD_TRACE;
            break;
        case 'p':               /* undocumented */
            debuglevel = CONFD_PROTO_TRACE;
            break;
        default:
            fprintf(stderr,
                    "Usage: %s [-dt]\n",
                    argv[0]);
            exit(1);
        }
    }

    /* set up for connection to ConfD */
    snprintf(confd_port, sizeof(confd_port), "%d", CONFD_PORT);
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = PF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    if ((i = getaddrinfo("127.0.0.1", confd_port, &hints, &addr)) != 0) {
        /* "Can't happen" */
        fprintf(stderr, "%s: Failed to get address for ConfD: %s\n",
                argv[0], gai_strerror(i));
        exit(1);
    }
    if ((progname = strrchr(argv[0], '/')) != NULL)
        progname++;
    else
        progname = argv[0];

    /* Init library and connect */
    confd_init(progname, stderr, debuglevel);
    OK(confd_load_schemas(addr->ai_addr, addr->ai_addrlen));
    if ((subsock = get_subsock(addr)) < 0)
        confd_fatal("Failed to connect subscription socket to ConfD\n");
    if ((notifsock = get_notifsock(addr)) < 0)
        confd_fatal("Failed to connect notification socket to ConfD\n");

    /* Subscribe to a leaf */
    OK(cdb_subscribe(subsock, 1, 0, &spoint, "/simple:simple/color"));
    OK(cdb_subscribe_done(subsock));

    /* main loop */
    while (1) {
        set[0].fd = subsock;
        set[0].events = POLLIN;
        set[0].revents = 0;
        set[1].fd = notifsock;
        set[1].events = POLLIN;
        set[1].revents = 0;

        if (poll(set, 2, -1) < 0) {
            confd_fatal("Poll failed");
        }

        /* Check for I/O */

        if (set[0].revents & POLLIN) { /* subsock */
            int spoint[1];
            int num_spoints;

            OK(cdb_read_subscription_socket(subsock, spoint, &num_spoints));
            OK(cdb_diff_iterate(subsock, spoint[0], iter, 0, NULL));
            OK(cdb_sync_subscription_socket(subsock, CDB_DONE_PRIORITY));
        }

        if (set[1].revents & POLLIN) { /* notifsock */
            OK(confd_read_notification(notifsock, &notif));
            if (notif.type == CONFD_NOTIF_UPGRADE_EVENT &&
                notif.n.upgrade.event == CONFD_UPGRADE_COMMITED) {
                fprintf(stderr, "%s got UPGRADE_COMMITED - reloading schemas\n",
                       progname);
                OK(confd_load_schemas(addr->ai_addr, addr->ai_addrlen));
            }
        }

    }
}
