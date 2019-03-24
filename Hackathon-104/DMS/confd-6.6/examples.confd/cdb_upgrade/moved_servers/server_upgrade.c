/*
 * Copyright 2006 Tail-f Systems AB
 *
 * Use cdb api to read from old db and maapi api to write in new
 *
 */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/poll.h>

#include <confd_lib.h>
#include <confd_maapi.h>
#include <confd_cdb.h>

#include "servers2.h"


static char *progname;
static int  debug = CONFD_SILENT;

static struct sockaddr_in addr; /* Keeps address to confd daemon */


/* confd_init() must be called before calling this function */
/* ms and cs are assumed to be valid sockets */
static void upgrade(int ms, int cs)
{
    int th;
    int i, n;
    static struct in_addr admin_ip;

    cdb_connect(cs, CDB_READ_SOCKET, (struct sockaddr *)&addr, sizeof(addr));
    cdb_start_session(cs, CDB_RUNNING);
    cdb_set_namespace(cs, servers__ns);

    maapi_connect(ms, (struct sockaddr *)&addr, sizeof(addr));
    maapi_attach_init(ms, &th);
    maapi_set_namespace(ms, th, servers__ns);

    admin_ip.s_addr = htonl(0x0a000001); /* initialize to 10.0.0.1 */
    n = cdb_num_instances(cs, "/servers/server");
    for (i=0; i < n; i++) {
        char name[128];
        confd_value_t ip, port, aip;
        char *dst;

        /* read old database using cdb_* API */
        cdb_get_str(cs, name, 128, "/servers/server[%d]/name", i);
        cdb_get(cs, &ip,   "/servers/server[%d]/ip", i);
        cdb_get(cs, &port, "/servers/server[%d]/port", i);

        if ((CONFD_GET_UINT16(&port) == 80) || (strstr(name, "www") != NULL)) {
            dst = "/servers/www{%s}";
        } else {
            dst = "/servers/others{%s}";
        }
        /* now create entries in the new database using maapi */
        maapi_create(ms, th, dst, name);
        maapi_pushd(ms, th, dst, name);
        maapi_set_elem(ms, th, &ip, "ip");
        maapi_set_elem(ms, th, &port, "port");
        CONFD_SET_IPV4(&aip, admin_ip);
        maapi_set_elem(ms, th, &aip, "adminIP");
        admin_ip.s_addr = htonl(ntohl(admin_ip.s_addr) + 1);
        maapi_popd(ms, th);
    }

    cdb_end_session(cs);
    cdb_close(cs);
}
/* END server_upgrade */


int main(int argc, char *argv[])
{
    char *confd_addr = "127.0.0.1";
    int confd_port = CONFD_PORT;
    int c, ms, cs;

    /* Setup progname (without path component) */
    if ((progname = strrchr(argv[0], (int)'/')) == NULL)
        progname = argv[0];
    else
        progname++;

    /* Parse command line */
    while ((c = getopt(argc, argv, "da:p:")) != EOF) {
        switch (c) {
        case 'd':
            debug++;
            break;
        case 'a':
            confd_addr = optarg;
            break;
        case 'p':
            confd_port = atoi(optarg);
            break;
        default:
            printf("huh?\n");
            exit(1);
        }
    }

    /* Initialize address to confd daemon */
    {
        addr.sin_addr.s_addr = inet_addr(confd_addr);
        addr.sin_family = AF_INET;
        addr.sin_port = htons(confd_port);
    }

    confd_init(progname, stdout, debug);

    if (((ms = socket(PF_INET, SOCK_STREAM, 0)) >= 0) &&
        ((cs = socket(PF_INET, SOCK_STREAM, 0)) >= 0)) {

        upgrade(ms, cs);

    }

    exit(0);
}
