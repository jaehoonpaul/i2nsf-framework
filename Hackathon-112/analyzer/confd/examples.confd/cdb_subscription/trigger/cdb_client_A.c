/*
 * Copyright 2009 Tail-f Systems AB
 *
 * Permission to use this code as a starting point hereby granted
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <sys/poll.h>

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
static FILE *outf;
static struct sockaddr_in addr; /* Keeps address to confd daemon */


#define OK(E) assert((E) == CONFD_OK)

/* Simplest possible client: read configuration of interaces and
 * enable and configure mac-addresses on all enabled
 * interfaces. Disable disabled interfaces.
 */
static void setup_interfaces(int ds)
{
    int i, nifc;

    OK(cdb_start_session(ds, CDB_RUNNING));

    nifc = cdb_num_instances(ds, "/example:sys/ifc");
    for (i=0; i<nifc; i++) {
        char if_name[32];
        char if_mac[24];
        int enabled;
        OK(cdb_pushd(ds, "/example:sys/ifc[%d]", i));
        OK(cdb_get_str(ds, if_name, sizeof(if_name), "name"));
        OK(cdb_get_bool(ds, &enabled, "enabled"));
        if (cdb_exists(ds, "hw/mac")) {
            OK(cdb_get_str(ds, if_mac, sizeof(if_mac), "hw/mac"));
        } else {
            /* Not configured, use (made-up) "builtin" */
            snprintf(if_mac, sizeof(if_mac),
                     "00:11:22:33:44:%02x", i);
        }
        fprintf(debugf, "%s: ifc[%d: %s] %s (%s)\n",
                progname, i, if_name, if_mac,
                enabled ? "enabled" : "disabled");
        if (enabled) {
            fprintf(outf, "%s: WRITE   IFC %s MAC: %s\n",
                    progname, if_name, if_mac);
            fprintf(outf, "%s: ENABLE  IFC %s\n", progname, if_name);
        } else {
            fprintf(outf, "%s: CLEAR   IFC %s MAC\n", progname, if_name);
            fprintf(outf, "%s: DISABLE IFC %s\n", progname, if_name);
        }
        OK(cdb_popd(ds));
    }

    OK(cdb_end_session(ds));
}



int main(int argc, char *argv[])
{
    char *confd_addr = "127.0.0.1";
    int confd_port = CONFD_PORT;
    int c;
    char *outfname = NULL;

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
    while ((c = getopt(argc, argv, "da:p:io:")) != EOF) {
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
        case 'o':
            outfname = optarg;
            break;
        default:
            exit(1);
        }
    }
    argc -= optind;
    argv += optind;

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

    outf = stdout;

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
        if ((debugf = fopen(fname, "w")) == NULL) {
            perror("couldn't open logfile");
            exit(1);
        }
        debug = CONFD_TRACE;
    } else {
        debugf = stderr;
    }

    if (outfname) {
        // char outfname[128];
        // snprintf(outfname, sizeof(outfname), "%s.output", progname);
        if ((outf = fopen(outfname, "w")) == NULL) {
            perror("couldn't open output file");
            exit(1);
        }
    }

    /* set stdout and debugf to unbuffered */
    setvbuf(outf, NULL, _IONBF, 0);
    setvbuf(debugf, NULL, _IONBF, 0);

    confd_init(progname, debugf, debug);
    OK(confd_load_schemas((struct sockaddr *)&addr, sizeof(addr)));

    {
        char *subpath = "/example:sys/ifc";
        int subid;
        int ss, ds;

        assert((ss = socket(PF_INET, SOCK_STREAM, 0)) >= 0);
        assert((ds = socket(PF_INET, SOCK_STREAM, 0)) >= 0);
        OK(cdb_connect(ss, CDB_SUBSCRIPTION_SOCKET,
                       (struct sockaddr *)&addr, sizeof(addr)));
        OK(cdb_connect(ds, CDB_DATA_SOCKET,
                       (struct sockaddr *)&addr, sizeof(addr)));

        OK(cdb_subscribe(ss, 150, 0, &subid, subpath));
        OK(cdb_subscribe_done(ss));

        fprintf(outf, "%s: started\n", progname);

        /* everything setup, fork and off we go */
        if (fork()) {
            /* parent */
            _exit(0);
        }

        for (;;) {
            struct pollfd fdset;

            fdset.fd = ss;
            fdset.events = POLLIN;
            fdset.revents = 0;

            if (poll(&fdset, 1, -1) < 0) {
                perror("poll() failed:");
                continue;
            }

            if (fdset.revents) {
                int r, n, subids[1];

                if ((r = cdb_read_subscription_socket(ss, subids, &n)) ==
                    CONFD_EOF) {
                    /* ConfD closed socket, take appropriate action... */
                    fprintf(outf, "%s: ConfD closed, exiting\n", progname);
                    exit(0);
                }
                if (r != CONFD_OK) {
                    confd_fatal("Error on ConfD socket: %s (%d): %s\n",
                                confd_strerror(confd_errno), confd_errno,
                                confd_lasterr());
                    exit(1);
                }

                assert(subids[0] == subid);

                fprintf(outf,
                        "\n%s: ======== subid %d triggered\n",
                        progname, subid);

                setup_interfaces(ds);

                OK(cdb_sync_subscription_socket(ss, CDB_DONE_PRIORITY));
                fprintf(outf, "\n%s: ======== "
                        "subscription notification acknowledged\n", progname);
            }
        }

        cdb_close(ss);
        cdb_close(ds);

    }


    exit(0);
}
