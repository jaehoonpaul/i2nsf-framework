/*
 * Copyright 2009 Tail-f Systems AB
 *
 * Permission to use this code as a starting point hereby granted
 *
 * Utility program to trigger CDB subscribers. Will trigger all
 * subscribers if invoked without any arguments, or list all
 * subscription id's to trigger them.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/socket.h>

#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/un.h>

#include <assert.h>

#include <confd_lib.h>
#include <confd_cdb.h>


static char *progname;
static enum confd_debug_level debug = CONFD_SILENT;
static FILE *debugf = NULL;
static struct sockaddr_in addr; /* Keeps address to confd daemon */


#define OK(E) assert((E) == CONFD_OK)

int main(int argc, char *argv[])
{
    char *confd_addr = "127.0.0.1";
    int confd_port = CONFD_PORT;
    int c;
    int *subids = NULL;
    int nsubids = 0;

    /* Setup progname (without path component) */
    if ((progname = strrchr(argv[0], (int)'/')) == NULL)
        progname = argv[0];
    else
        progname++;

    {
        char *ptmp = getenv("CONFD_IPC_PORT");
        if (ptmp) {
            confd_port = atoi(ptmp);
        } else {
            confd_port = CONFD_PORT;
        }
    }

    /* Parse command line */
    while ((c = getopt(argc, argv, "da:p:i")) != EOF) {
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
        case 'i':
            printf("%d\n", getpid());
            fflush(stdout);
            break;
        default:
            exit(1);
        }
    }
    argc -= optind;
    argv += optind;

    if (argc > 0) {
        int i;

        nsubids = argc;
        subids = malloc(nsubids * sizeof(*subids));
        assert(subids != NULL);
        for (i=0; i<nsubids; i++) {
            subids[i] = atoi(argv[i]);
        }
    }

    /* Initialize address to confd daemon */
    {
        struct in_addr in;
        if (inet_pton(AF_INET, confd_addr, &in) == 1) {
            addr.sin_family = AF_INET;
            addr.sin_addr.s_addr = in.s_addr;
            addr.sin_port = htons(confd_port);
        } else {
            fprintf(stderr, "unparsable adress: %s\n", confd_addr);
            exit(1);
        }
    }

    /* always save trace output somewhere */
    if (debug == CONFD_SILENT) {
        char fname[255];
        char *suffix = getenv("CDB_SET_FILE_SUFFIX");
        if (confd_port == CONFD_PORT) {
            snprintf(fname, sizeof(fname), "_tmp_%s", progname);
        } else {
            snprintf(fname, sizeof(fname), "_tmp_%s.%d", progname, confd_port);
        }
        if (suffix) {
            char tmpstr[16];
            if (strcmp(suffix, "pid") == 0) {
                snprintf(tmpstr, sizeof(tmpstr), "%d", (int)getpid());
                suffix = tmpstr;
            }
            strncat(fname, suffix, sizeof(fname) - strlen(fname) - 1);
        }
        debugf = fopen(fname, "w");
        debug = CONFD_TRACE;
    } else {
        debugf = stderr;
    }

    confd_init(progname, debugf, debug);

    {
        int s;

        assert((s = socket(PF_INET, SOCK_STREAM, 0)) >= 0);
        OK(cdb_connect(s, CDB_DATA_SOCKET,
                       (struct sockaddr *)&addr, sizeof(addr)));

        fprintf(debugf, "calling cdb_wait_start()\n");
        cdb_wait_start(s);      /* Make sure phase1 is complete */

        /* blocks until all clients have ack:ed */
        OK(cdb_trigger_subscriptions(s, subids, nsubids));

        cdb_close(s);
    }

    if (subids) free(subids);

    exit(0);
}
