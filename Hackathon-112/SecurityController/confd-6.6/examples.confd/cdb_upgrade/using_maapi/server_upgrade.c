/*
 * Copyright 2006 Tail-f Systems AB
 *
 * Use MAAPI to upgrade CDB
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


int main(int argc, char *argv[])
{
    char *confd_addr = "127.0.0.1";
    int confd_port = CONFD_PORT;
    int c, ms;
    struct maapi_cursor mc;
    int th;


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

    ms = socket(PF_INET, SOCK_STREAM, 0);

    maapi_connect(ms, (struct sockaddr *)&addr, sizeof(addr));
    maapi_attach_init(ms, &th);
    maapi_set_namespace(ms, th, servers__ns);

    /* Loop over servers and set default values for new elems */
    maapi_init_cursor(ms, th, &mc, "/servers/server");
    maapi_get_next(&mc);
    while (mc.n != 0) {
        maapi_set_elem2(ms, th,
                        "0.0.0.0", "/servers/server{%x}/proxy", &mc.keys[0]);
        maapi_set_elem2(ms, th,
                        "true", "/servers/server{%x}/enabled", &mc.keys[0]);
        maapi_get_next(&mc);
    }
    maapi_destroy_cursor(&mc);

    exit(0);
}
