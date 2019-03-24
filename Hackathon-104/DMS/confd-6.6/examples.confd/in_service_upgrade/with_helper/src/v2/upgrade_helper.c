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

static char *progname;
static int  debug = CONFD_SILENT;

static struct sockaddr_in addr; /* Keeps address to confd daemon */

#define OK(ret) assert((ret) == CONFD_OK)

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

    OK(maapi_connect(ms, (struct sockaddr *)&addr, sizeof(addr)));
    OK(maapi_load_schemas(ms));
    OK(maapi_attach_init(ms, &th));
    OK(maapi_cd(ms, th, "/servers:servers"));

    /* Loop over servers and set "default" values for new elems */
    OK(maapi_init_cursor(ms, th, &mc, "server"));
    OK(maapi_get_next(&mc));
    while (mc.n != 0) {
        OK(maapi_set_elem2(ms, th, "0.0.0.0", "server{%x}/proxy", &mc.keys[0]));
        OK(maapi_set_elem2(ms, th, "true", "server{%x}/enabled", &mc.keys[0]));
        OK(maapi_get_next(&mc));
    }
    maapi_destroy_cursor(&mc);

    exit(0);
}
