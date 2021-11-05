
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <sys/poll.h>
#include <ctype.h>

#include <confd_lib.h>
#include <confd_cdb.h>
#include "types.h"

#define OK(val) assert((val) == CONFD_OK)

static struct addrinfo *addr;

struct port {
    int shelf;
    int slot;
    int port;
};

static int get_cdbsock(struct addrinfo *addr)
{
    int sock;

    if ((sock =
         socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol)) > 0) {
        if (cdb_connect(sock, CDB_DATA_SOCKET,
                        addr->ai_addr, addr->ai_addrlen) == CONFD_OK &&
            cdb_start_session(sock, CDB_RUNNING) == CONFD_OK) {
            return sock;
        }
        close(sock);
    }
    return -1;
}

static void get_port(confd_value_t *v, struct port *p)
{
    int32_t i = CONFD_GET_INT32(v);

    p->shelf = (i >> 16) & 0xff;
    p->slot  = (i >> 8) & 0xff;
    p->port  = i & 0xff;
}

#if 0 /* not used */
static void set_port(struct port *p, confd_value_t *v)
{
    int32_t i = (p->shelf << 16) | (p->slot << 8) | p->port;

    CONFD_SET_INT32(v, i);
}
#endif

int main(int argc, char **argv)
{
    char confd_port[16];
    struct addrinfo hints;
    int debuglevel = CONFD_SILENT;
    int c;
    char *p, *dname;
    int i;
    int ncbs;
    struct confd_type_cbs *cbs;
    int sock;
    confd_value_t name, ip;
    struct port port;
    char *path = "/system/port";
    int nports;
    struct confd_cs_node *root, *node;
    struct confd_type *type;
    char buf[BUFSIZ];

    snprintf(confd_port, sizeof(confd_port), "%d", CONFD_PORT);
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = PF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    while ((c = getopt(argc, argv, "dtpc:")) != -1) {
        switch (c) {
        case 'd':
            debuglevel = CONFD_DEBUG;
            break;
        case 't':
            debuglevel = CONFD_TRACE;
            break;
        case 'p':
            debuglevel = CONFD_PROTO_TRACE;
            break;
        case 'c':
            if ((p = strchr(optarg, '/')) != NULL)
                *p++ = '\0';
            else
                p = confd_port;
            if (getaddrinfo(optarg, p, &hints, &addr) != 0) {
                if (p != confd_port) {
                    *--p = '/';
                    p = "/port";
                } else {
                    p = "";
                }
                fprintf(stderr, "%s: Invalid address%s: %s\n",
                        argv[0], p, optarg);
                exit(1);
            }
            break;
        default:
            fprintf(stderr,
                    "Usage: %s [-dtp] [-c address[/port]]\n",
                    argv[0]);
            exit(1);
        }
    }

    if (addr == NULL &&
        (i = getaddrinfo("127.0.0.1", confd_port, &hints, &addr)) != 0)
        /* "Can't happen" */
        confd_fatal("%s: Failed to get address for ConfD: %s\n",
                    argv[0], gai_strerror(i));
    if ((dname = strrchr(argv[0], '/')) != NULL)
        dname++;
    else
        dname = argv[0];

    /* Init library */
    confd_init(dname, stderr, debuglevel);
    OK(confd_load_schemas(addr->ai_addr, addr->ai_addrlen));

    /* Register types with library (using the same code that is loaded
       into ConfD) - needed for confd_val2str()/confd_str2val() */
    ncbs = confd_type_cb_init(&cbs);
    for (i = 0; i < ncbs; i++) {
        if (strcmp(cbs[i].typepoint, types__typepointid_port_type) == 0)
            OK(confd_register_ns_type(types__ns,"portType",cbs[i].type));
        else if (strcmp(cbs[i].typepoint, types__typepointid_ipv4_mask) == 0)
            OK(confd_register_ns_type(types__ns,"ipv4AddressMask",cbs[i].type));
        else if (strcmp(cbs[i].typepoint, types__typepointid_ipv6_mask) == 0)
            OK(confd_register_ns_type(types__ns,"ipv6AddressMask",cbs[i].type));
    }

    /* Connect to CDB and get # of configured instances */
    if ((sock = get_cdbsock(addr)) < 0)
        confd_fatal("Failed to connect to CDB\n");
    OK(cdb_set_namespace(sock, types__ns));
    nports = cdb_num_instances(sock, path);
    if (nports < 1)
        confd_fatal("No %s instances configured\n", path);

    for (i = 0; i < nports; i++) {

        /* Read the values from CDB */
        OK(cdb_get(sock, &name, "%s[%d]/name", path, i));
        OK(cdb_get(sock, &ip, "%s[%d]/ip", path, i));

        /* Extract the port value into a C struct and use to print path */
        get_port(&name, &port);
        printf("%s{%d/%d/%d}: ", path, port.shelf, port.slot, port.port);

        /* Use the libconfd functionality to
           convert the values to strings and print - */

        /* - find type from element in tree */
        root = confd_find_cs_root(types__ns);
        node = confd_cs_node_cd(root, "%s/name", path);
        confd_val2str(node->info.type, &name, buf, sizeof(buf));
        printf(" name = %s ", buf);

        /* - find type from namespace and type name */
        type = confd_find_ns_type(types__ns, "ipAddressMask");
        confd_val2str(type, &ip, buf, sizeof(buf));
        printf(" ip = %s\n", buf);

    }

    return 0;
}
