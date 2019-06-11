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

#include "example.h"


static char *progname;
static enum confd_debug_level debug = CONFD_SILENT;
static FILE *debugf = NULL;
static FILE *outf;
static struct sockaddr_in addr; /* Keeps address to confd daemon */


#define OK(E) assert((E) == CONFD_OK)


struct iter_state {
    int ds;         /* A CDB socket connected to RUNNING */
    int ps;         /* A CDB socket connected to PRE_COMMIT_RUNNING */
    char *subpath;
};

static enum cdb_iter_ret sub_iter(confd_hkeypath_t *kp,
                                      enum cdb_iter_op op,
                                      confd_value_t *oldv,
                                      confd_value_t *newv,
                                      void *state)
{
    struct iter_state *is = (struct iter_state *)state;
    int ds = is->ds;
    char pathstr[128];

    confd_pp_kpath(pathstr, sizeof(pathstr), kp);

    switch (op) {
    case MOP_CREATED:
        fprintf(debugf, "%s: %s created\n", progname, pathstr);
        if (CONFD_GET_XMLTAG(&kp->v[1][0]) == example_ip) {
            /* An IP address was created */

            /* keypath is /sys/ifc{$name}/ip{$ip} */
            /*              4   3    2    1   0   */
            confd_value_t *ip = &kp->v[0][0];
            confd_value_t *name = &kp->v[2][0];
            char ipstr[32], namestr[32], bcaststr[32];
            int prefixlen, enabled;

            confd_pp_value(ipstr, sizeof(ipstr), ip);
            confd_pp_value(namestr, sizeof(namestr), name);

            /* Check if ifc is enabled */
            OK(cdb_get_bool(ds, &enabled,
                            "/example:sys/ifc{%x}/enabled", name));

            /* Now use ds to read prefix length and mask */
            OK(cdb_pushd(ds, "%h", kp));
            OK(cdb_get_int32(ds, &prefixlen, "prefix-length"));
            bcaststr[0] = '\000';
            if (cdb_exists(ds, "broadcast")) {
                confd_value_t bcast;
                OK(cdb_get(ds, &bcast, "broadcast"));
                confd_pp_value(bcaststr, sizeof(bcaststr), &bcast);
            }
            OK(cdb_popd(ds));
            fprintf(outf, "%s: %s IP IFC %s: %s/%d %s\n", progname,
                    enabled ? "SET  " : "SET* ",
                    namestr, ipstr, prefixlen, bcaststr);
            return ITER_CONTINUE;

        }
        return ITER_RECURSE;
        break;
    case MOP_DELETED:
        fprintf(debugf, "%s: %s deleted\n", progname, pathstr);
        return ITER_CONTINUE;
        break;
    case MOP_MODIFIED:
        fprintf(debugf, "%s: %s modified\n", progname, pathstr);
        return ITER_RECURSE;
        break;
    case MOP_VALUE_SET:
        fprintf(debugf, "%s: %s set\n", progname, pathstr);
        return ITER_CONTINUE;
        break;
    default:
        /* We should never get MOP_MOVED_AFTER or MOP_ATTR_SET */
        fprintf(debugf, "%s: %s unexpected op %d!\n", progname, pathstr, op);
        return ITER_RECURSE;
        break;
    }
}


static enum cdb_iter_ret handle_delete(confd_hkeypath_t *kp,
                                       enum cdb_iter_op op,
                                       confd_value_t *oldv,
                                       confd_value_t *newv,
                                       void *state)
{
    struct iter_state *is = (struct iter_state *)state;
    int ps = is->ps;

    char pathstr[128];
    confd_pp_kpath(pathstr, sizeof(pathstr), kp);

    switch (op) {
    case MOP_DELETED:
        fprintf(debugf, "%s: %s deleted\n", progname, pathstr);
        switch (CONFD_GET_XMLTAG(&kp->v[1][0])) {
        case example_ip: {
            /* An IP address was deleted */
            confd_value_t *ip = &kp->v[0][0];
            confd_value_t *name = &kp->v[2][0];
            char ipstr[32], namestr[32];
            confd_pp_value(ipstr, sizeof(ipstr), ip);
            confd_pp_value(namestr, sizeof(namestr), name);

            fprintf(outf,
                    "%s: DELETE IP IFC %s: %s\n", progname, ipstr, namestr);
        }
            break;
        case example_ifc: {
            /* A whole interface was deleted */
            int i, n;
            confd_value_t *name = &kp->v[0][0];
            char namestr[32];

            confd_pp_value(namestr, sizeof(namestr), name);

            /* Use PRE_COMMIT_RUNNING to read out deleted IP addresses */
            assert(ps >= 0);
            OK(cdb_pushd(ps, "%h", kp));
            n = cdb_num_instances(ps, "ip");
            for (i=0; i<n; i++) {
                confd_value_t ip;
                char ipstr[32];
                cdb_get(ps, &ip, "ip[%d]/address", i);
                confd_pp_value(ipstr, sizeof(ipstr), &ip);

                fprintf(outf,
                        "%s: DELETE IP IFC %s: %s\n", progname, ipstr, namestr);
            }
            OK(cdb_popd(ps));
        }
            break;
        }
        return ITER_CONTINUE;
        break;
    case MOP_CREATED:
        return ITER_CONTINUE;
        break;
    default:
        return ITER_RECURSE;
        break;
    }
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
        int subid1, subid2;
        int ps, ss, ds;

        assert((ss = socket(PF_INET, SOCK_STREAM, 0)) >= 0);
        assert((ds = socket(PF_INET, SOCK_STREAM, 0)) >= 0);
        assert((ps = socket(PF_INET, SOCK_STREAM, 0)) >= 0);
        OK(cdb_connect(ss, CDB_SUBSCRIPTION_SOCKET,
                       (struct sockaddr *)&addr, sizeof(addr)));
        OK(cdb_connect(ds, CDB_DATA_SOCKET,
                       (struct sockaddr *)&addr, sizeof(addr)));
        OK(cdb_connect(ps, CDB_DATA_SOCKET,
                       (struct sockaddr *)&addr, sizeof(addr)));

        OK(cdb_subscribe(ss, 100, 0, &subid1, subpath));
        OK(cdb_subscribe(ss, 200, 0, &subid2, subpath));
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
                int r, i, n, subids[2];
                struct iter_state is;

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

                is.subpath = subpath;
                OK(cdb_start_session(ds, CDB_RUNNING));
                is.ds = ds;
                if (cdb_start_session(ps, CDB_PRE_COMMIT_RUNNING) ==
                    CONFD_OK) {
                    is.ps = ps;
                } else {
                    /* For synthetic subscription triggers there is no prev */
                    assert(confd_errno == CONFD_ERR_NOEXISTS);
                    is.ps = -1;
                }

                for (i=0; i<n; i++) {
                    fprintf(outf, "\n%s: ======== subid %d triggered\n",
                            progname, subids[i]);
                    if (debugf != stderr) {
                        fprintf(debugf, "%s: ======== subid %d triggered\n",
                                progname, subids[i]);
                    }

                    if (subids[i] == subid1) {
                        OK(cdb_diff_iterate(ss, subids[i], handle_delete,
                                            0, &is));
                    } else if (subids[i] == subid2) {
                        OK(cdb_diff_iterate(ss, subids[i], sub_iter,
                                            ITER_WANT_PREV, &is));
                    }
                }

                OK(cdb_end_session(ds));
                if (is.ps >= 0) { OK(cdb_end_session(ps)); }

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
