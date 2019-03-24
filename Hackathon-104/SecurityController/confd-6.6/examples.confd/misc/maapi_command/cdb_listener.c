#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/poll.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdio.h>

#include "confd_lib.h"
#include "confd_cdb.h"

static void usage() {
    printf("cdb_listener [-t|-d|-T|-S] [-p <port>] [-P <path>]\n");
    exit(1);
}



static enum cdb_iter_ret iter(confd_hkeypath_t *kp,
                              enum cdb_iter_op op,
                              confd_value_t *oldv,
                              confd_value_t *newv,
                              void *state)
{
    char buf[BUFSIZ];

    confd_pp_kpath(buf, BUFSIZ, kp);
    switch (op) {
    case MOP_CREATED:
        printf ("Create: %s\n", buf);
        break;
    case MOP_DELETED:
        printf ("Delete: %s\n", buf);
        break;
    case MOP_MODIFIED: {
        printf("Modified: %s\n", buf);
        break;
    }
    case MOP_VALUE_SET: {
        char obuf[BUFSIZ];
        char nbuf[BUFSIZ];
        if (oldv) confd_pp_value(obuf, BUFSIZ, oldv);
        if (newv) confd_pp_value(nbuf, BUFSIZ, newv);
        printf("Value Set: %s --> (%s)\n",
               buf, nbuf);
        break;
    }
    case MOP_MOVED_AFTER:
        if (newv) {
            char vbuf[BUFSIZ];
            confd_value_t *vp = newv;
            printf("Moved after: %s -> ", buf);
            while (vp->type != C_NOEXISTS) {
                confd_pp_value(vbuf, BUFSIZ, vp);
                printf("%c%s", vp == newv ? '{' : ' ', vbuf);
                vp++;
            }
            printf("}\n");
        } else {
            printf("Moved first: %s\n", buf);
        }
        break;
    default:
        /* We should never get MOP_ATTR_SET */
        confd_fatal("Got unexpected op %d in cdb_diff_iterate()", op);
    }
    return ITER_RECURSE;
}



int main(int argc, char **argv)
{
    int c;
    int port = CONFD_PORT;
    int debuglevel = CONFD_DEBUG;
    struct sockaddr_in addr;
    int subsock;
    int status;
    int spoint;
    char *path = "/";

    while ((c = getopt(argc, argv, "tdTSp:P:")) != -1) {
        switch(c) {
        case 't':
            debuglevel = CONFD_TRACE;
            break;
        case 'd':
            debuglevel = CONFD_DEBUG;
            break;
        case 'T':
            debuglevel = CONFD_PROTO_TRACE;
            break;
        case 'S':
            debuglevel = CONFD_SILENT;
            break;
        case 'p':
            port = atoi(optarg);
            break;
        case 'P':
            path = optarg;
            break;
        default:
            usage();
        }
    }

    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);

    confd_init(argv[0], stderr, debuglevel);
    if (confd_load_schemas((struct sockaddr*)&addr,
                           sizeof (struct sockaddr_in)) != CONFD_OK)
        confd_fatal("Failed to load schemas from confd\n");

    if ((subsock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        confd_fatal("Failed to open socket\n");
    if (cdb_connect(subsock, CDB_SUBSCRIPTION_SOCKET, (struct sockaddr*)&addr,
                      sizeof (struct sockaddr_in)) < 0)
        confd_fatal("Failed to confd_connect() to confd \n");

    if ((status = cdb_subscribe(subsock, 3, 0, &spoint, path))
        != CONFD_OK) {
        fprintf(stderr, "Terminate: subscribe %d\n", status);
        exit(0);
    }
    if (cdb_subscribe_done(subsock) != CONFD_OK)
        confd_fatal("cdb_subscribe_done() failed");
    printf("CDB Subscription point = %d\n", spoint);

    while (1) {


        struct pollfd set[1];

        set[0].fd = subsock;
        set[0].events = POLLIN;
        set[0].revents = 0;


        if (poll(&set[0], 1, -1) < 0) {
            perror("Poll failed:");
            continue;
        }

        /* Check for I/O */
        if (set[0].revents & POLLIN) {
            int sub_points[1];
            int reslen;

            if ((status = cdb_read_subscription_socket(subsock,
                                                       &sub_points[0],
                                                       &reslen)) != CONFD_OK) {
                fprintf(stderr, "terminate sub_read: %d\n", status);
                exit(1);
            }
            if (reslen > 0) {
                printf("*** Config updated \n");
                cdb_diff_iterate(subsock, sub_points[0], iter,
                                 ITER_WANT_PREV, NULL);
            }

            if ((status = cdb_sync_subscription_socket(subsock,
                                                       CDB_DONE_PRIORITY))
                != CONFD_OK) {
                fprintf(stderr, "failed to sync subscription: %d\n", status);
                exit(1);
            }
        }
    }
}
