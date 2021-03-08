/*
 * A small utility program interfacing the load_config() / save_config() maapi
 * functions.
 *
 * Copyright 2008 Tail-f Systems
 *
 * Permission to use this code as a starting point hereby granted
 *
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/param.h>
#include <sys/time.h>
#include <errno.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/un.h>
#include <sys/poll.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdarg.h>
#include <fcntl.h>

#include <confd.h>
#include <confd_maapi.h>
#include <confd_cdb.h>

/* #define EXTERNAL_IPC */ /* Uncomment this to provide support for user-
                            * defined IPC towards the ConfD/NCS daemon.
                            * The CONFD_IPC_EXTADDR and CONFD_IPC_EXTSOPATH
                            * - or NCS_IPC_EXTADDR and NCS_IPC_EXTSOPATH -
                            * environment variables can then be used to
                            * request a connection using this IPC mechanism,
                            * see the deployment chapter in the User Guide.
                            * Requires that the shared object provides the
                            * getaddrinfo() and socket() IPC callbacks.
                            * Note, on Linux this requires that -ldl is
                            * added to the LIBS definition in the Makefile.
                            */

#ifdef EXTERNAL_IPC
#include <dlfcn.h>
#include "ipc_drv.h"
#endif

#define ERR(s, doexit) do {                              \
        fatal(0, doexit, "%d: %s failed: %s (%d): %s",   \
              __LINE__, (s),                             \
              confd_strerror(confd_errno),               \
              confd_errno, confd_lasterr());             \
    } while (0)

#define OK(E) do {                                      \
        if ((E) != CONFD_OK) {                          \
            ERR(#E, 1);                                 \
        }                                               \
    } while (0)


#ifdef NCS
#define SERVER "NCS"
#define PORT NCS_PORT
#define IPC_ADDR "NCS_IPC_ADDR"
#define IPC_PORT "NCS_IPC_PORT"
#define IPC_EXTADDR "NCS_IPC_EXTADDR"
#define IPC_EXTSOPATH "NCS_IPC_EXTSOPATH"
#define MAAPI_THANDLE "NCS_MAAPI_THANDLE"
#define MAAPI_USID "NCS_MAAPI_USID"
#else
#define SERVER "ConfD"
#define PORT CONFD_PORT
#define IPC_ADDR "CONFD_IPC_ADDR"
#define IPC_PORT "CONFD_IPC_PORT"
#define IPC_EXTADDR "CONFD_IPC_EXTADDR"
#define IPC_EXTSOPATH "CONFD_IPC_EXTSOPATH"
#define MAAPI_THANDLE "CONFD_MAAPI_THANDLE"
#define MAAPI_USID "CONFD_MAAPI_USID"
#endif

/* global variables */
static char *progname = NULL;
static int debuglevel = CONFD_SILENT;
static int sock, tid;
static int new_session_started = 0;
static int attached = 0;
static int via_candidate = 0;
static enum confd_dbname ldb = CONFD_RUNNING, sdb = CONFD_RUNNING;
static int family, type, protocol;
static struct sockaddr *addr;
static socklen_t addrlen;
#ifdef EXTERNAL_IPC
static struct confd_ext_ipc_cbs *ecbs;
#endif
static char cwd[MAXPATHLEN];

static void usage()
{
    fprintf(
        stderr,
        "A utility that saves and loads the running configuration, usage: \n"
        "    %s [options] [filename]\n"
        "    %s -l [options] [filename...]\n"
        "    %s -C [-R] [filename...]\n",
        progname, progname, progname);
    fprintf(
        stderr,
        " Valid options are (for further details, see the manpage):\n");
    fprintf(
        stderr,
        "    -d            debug flag\n"
        "    -l            load config into " SERVER " [default is to save]\n"
#ifdef NCS
        "    -j            disable NCS Fastmap during load\n"
        "    -n            no networking, just apply the data to NCS\n"
        "    -M            include NCS service-meta-data attributes\n"
#endif
        "    -F x|p|o|j|c|i format (one of x(ml), p(retty xml), json or\n"
        "                  j, c, i (-style cli)) [default is xml]\n"
        "    -W            include default values\n"
        "    -S            include default values as comments\n"
        "    -m            when loading config, merge"
        " [default is delete and replace]\n"
        "    -r            when loading config, replace"
        " [default is delete and replace]\n"
        "    -H            hide all hidden nodes"
        " [default depends on transaction]\n"
        "    -U            unhide all hidden nodes"
        " [default depends on transaction]\n"
        "    -a            when loading 'c' or 'i' config, commit"
        " after each line\n"
        "    -e            when loading, do not abort on errors\n"
        "    -p  <path>    when saving the path to save\n"
        "                  when loading this path will first be deleted\n"
        "    -N            when saving, do Not include parents to the path\n"
        "    -P  <xpath>   when saving apply this xpath filter\n"
        "    -D            do maapi_delete_all(MAAPI_DEL_ALL) before loading\n"
        "    -o            when saving include operational data\n"
        "                  when loading ignore operational data\n"
        "    -u  <user>    use this user\n"
        "    -g  <group>   use this group (may be repeated)\n"
        "    -c  <context> use this context [default is 'system']\n"
        "    -i            attach to init session instead of"
        " starting new session\n"
        "    -s            start transaction towards startup"
        " [default is running]\n"
        "    -O            when saving, include only operational data,"
        " not config\n"
        "                  when loading, start operational transaction (load"
        " oper\n"
        "                  data via a transaction instead of via CDB)\n"
        "    -t            measure how long the requested command takes\n"
        "\n"
        "    -C            load filename into CDB operational\n"
        "    -R            generate CDB operational subscription notifications"
        "\n"
        );
}

static void fatal(int err, int doexit, char *fmt, ...)
{
    va_list ap;

    fprintf(stderr, "%s: ", progname);
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    if (err != 0)
        fprintf(stderr, ": %s", strerror(err));
    fprintf(stderr, "\n");
    if (doexit)
        exit(doexit);
}

void get_daemon_addr(char *addrstr, int port)
{
    static struct sockaddr_in in_addr;
    static struct sockaddr_in6 in6_addr;
    static struct sockaddr_un un_addr;
    char *daemon_addr = "127.0.0.1";
    int daemon_port = PORT;
    char *etmp;
#ifdef EXTERNAL_IPC
    char *path;
    void *handle;
    confd_ext_ipc_init_func_t *ext_init_ops;
    char *errstr = NULL;
#endif

    if (addrstr != NULL) {
        daemon_addr = addrstr;
    } else if ((etmp = getenv(IPC_ADDR)) != NULL) {
        daemon_addr = etmp;
    } else if ((etmp = getenv(IPC_EXTADDR)) != NULL) {
        daemon_addr = etmp;
#ifdef EXTERNAL_IPC
        if ((path = getenv(IPC_EXTSOPATH)) != NULL) {
            if ((handle = dlopen(path, RTLD_LOCAL|RTLD_LAZY)) == NULL) {
                fprintf(stderr, "Failed to load %s\n", path);
            } else {
                if ((ext_init_ops = (confd_ext_ipc_init_func_t *)
                     dlsym(handle, "confd_ext_ipc_init")) == NULL ||
                    ((ecbs = (*ext_init_ops)()) == NULL) ||
                    ecbs->getaddrinfo == NULL || ecbs->socket == NULL) {
                    fprintf(stderr,
                            "Failed to find getaddrinfo()/socket() in %s\n",
                            path);
                } else {
                    if (ecbs->getaddrinfo(daemon_addr,
                                          &family, &type, &protocol,
                                          &addr, &addrlen, &errstr) != 0) {
                        fprintf(stderr, "getaddrinfo() in %s failed", path);
                        if (errstr != NULL) {
                            fprintf(stderr, ": %s\n", errstr);
                            free(errstr);
                        } else {
                            fprintf(stderr, "\n");
                        }
                    } else {
                        return;
                    }
                }
            }
            ecbs = NULL;
            fprintf(stderr, "Ignoring environment " IPC_EXTSOPATH "\n");
        }
#endif
    }
    if (port != 0) {
        daemon_port = port;
    } else if ((etmp = getenv(IPC_PORT)) != NULL) {
        daemon_port = atoi(etmp);
    }

    memset(&in_addr, '\0', sizeof(in_addr));
    memset(&in6_addr, '\0', sizeof(in6_addr));
    memset(&un_addr, '\0', sizeof(un_addr));
    type = SOCK_STREAM;
    protocol = 0;
    if (inet_pton(AF_INET, daemon_addr, &in_addr.sin_addr) == 1) {
        family = PF_INET;
        in_addr.sin_family = AF_INET;
        in_addr.sin_port = htons(daemon_port);
        addr = (struct sockaddr *)&in_addr;
        addrlen = sizeof(in_addr);
    } else if (inet_pton(AF_INET6, daemon_addr, &in6_addr.sin6_addr) == 1) {
        family = PF_INET6;
        in6_addr.sin6_family = AF_INET6;
        in6_addr.sin6_port = htons(daemon_port);
        addr = (struct sockaddr *)&in6_addr;
        addrlen = sizeof(in6_addr);
    } else {
        family = PF_UNIX;
        un_addr.sun_family = AF_UNIX;
        snprintf(un_addr.sun_path, sizeof(un_addr.sun_path),
                 "%s", daemon_addr);
        addr = (struct sockaddr *)&un_addr;
        addrlen = sizeof(un_addr);
    }
}

int get_socket()
{
#ifdef EXTERNAL_IPC
    char *errstr;

    if (ecbs != NULL)
        return ecbs->socket(family, type, protocol, &errstr);
    else
#endif
        return socket(family, type, protocol);
}

static int cnct(int minit, int load, char *user, char *ctxt,
                char **groups, int ngroups)
{
    struct confd_ip ip;
    int mode = load ? CONFD_READ_WRITE : CONFD_READ;
    enum confd_dbname mdb = load ? ldb : sdb;

    OK(maapi_connect(sock, addr, addrlen));

    ip.af = AF_INET;
    inet_pton(AF_INET, "127.0.0.1", &ip.ip.v4);

    if (minit) {
        if (debuglevel > CONFD_SILENT) {
            fprintf(stderr, "attaching to init session...\n");
        }
        OK(maapi_attach_init(sock, &tid));
        attached++;
    } else {
        char *usid_env = getenv(MAAPI_USID);
        char *th_env = getenv(MAAPI_THANDLE);

        if (usid_env && th_env) {
            int usid = atoi(usid_env);
            tid = atoi(th_env);
            if (debuglevel > CONFD_SILENT) {
                fprintf(stderr, "attaching to existing session: "
                        "usid=%d thandle=%d\n", usid, tid);
            }
            OK(maapi_attach2(sock, 0, usid, tid));
            attached++;
        } else {
            if (debuglevel > CONFD_SILENT) {
                int i;
                fprintf(stderr, "starting user session "
                        "ctxt=%s user=%s groups=[", ctxt, user);
                for (i=0; i<ngroups; i++) {
                    fprintf(stderr, "%s", groups[i]);
                    if (i < (ngroups-1)) { fputs(",", stderr); }
                }
                fputs("]\n", stderr);
            }
            OK(maapi_start_user_session(sock, user, ctxt,
                                        (const char **)groups, ngroups,
                                        &ip, CONFD_PROTO_TCP));
            new_session_started++;
            if ((tid = maapi_start_trans(sock, mdb, mode)) < 0 &&
                mdb == CONFD_RUNNING && mode == CONFD_READ_WRITE &&
                confd_errno == CONFD_ERR_NOT_WRITABLE) {
                /* assume running is writable-through-candidate */
                if ((tid = maapi_start_trans(sock, CONFD_CANDIDATE, mode)) >= 0)
                    via_candidate++;
            }
            if (tid < 0)
                ERR("maapi_start_trans()", 1);
        }
    }

    return CONFD_OK;
}

enum timer_cmd {
    TIMER_START,
    TIMER_STOP,
    TIMER_REPORT
};

static void lap_timer(int enabled, enum timer_cmd cmd)
{
    static struct timeval start, stop;
    static enum timer_cmd state = TIMER_START;

    if (!enabled) return;

    switch (cmd) {
    case TIMER_START:
        state = TIMER_START;
        gettimeofday(&start, NULL);
        break;
    case TIMER_STOP:
        if (state == TIMER_START) {
            gettimeofday(&stop, NULL);
            stop.tv_sec -= start.tv_sec;
            stop.tv_usec -= start.tv_usec;
            if (stop.tv_usec < 0) { stop.tv_sec--; stop.tv_usec += 1000000; }
            state = TIMER_STOP;
        }
        break;
    case TIMER_REPORT:
        if (state == TIMER_STOP) {
            fprintf(stderr, "elapsed time: %d.%03d s\n",
                    (int)stop.tv_sec, (int)(stop.tv_usec / 1000));
        }
        break;
    }
    return;
}


static char *save_stdin()
{
    char *buf = NULL;
    int len = 0;
    int buflen = 0;
    int r;

    do {
        if (buflen - len < BUFSIZ) {
            if ((buf = realloc(buf, len + BUFSIZ)) == NULL) {
                fatal(0, 1, "failed to allocate memory");
            }
            buflen = len + BUFSIZ;
        }
        if ((r = read(fileno(stdin), &buf[len], BUFSIZ - 1)) > 0) {
            len += r;
        }
    } while (r > 0);

    if (r < 0) {
        fatal(errno, 1, "failed to read from stdin");
    }
    buf[len] = '\0';
    return buf;
}


static char *abspath(char *path)
{
    static char abs[2*MAXPATHLEN];

    if (path[0] == '/')
        return path;
    snprintf(abs, sizeof(abs), "%s/%s", cwd, path);
    return abs;
}


int main(int argc, char **argv)
{
    int c;
    int config_format = MAAPI_CONFIG_XML;
    char *path = NULL;
    char *xpath = NULL;
    char *filename = NULL;
    int load_config = 0;
    int delete_config = 0;
    int attach_init = 0;
    char *username = "system";
    char *context = "system";
    char *groups[16];
    int ngroups = 0;
    int flags = 0;
    int timer = 0;
    int cdb_load = 0;
    int cdb_load_flags = 0;
    int disable_fastmap = 0;
    int no_networking=0;

    /* Setup progname (without path component) */
    if ((progname = strrchr(argv[0], (int)'/')) == NULL)
        progname = argv[0];
    else
        progname++;
    getcwd(cwd, sizeof(cwd));

    while ((c = getopt(argc, argv, "xnNdjF:CRWSp:P:f:lmrHUaeoMDiu:g:c:tsOh?"))
           != -1)
    {
        switch(c) {
        case 'x':
            flags |=  MAAPI_CONFIG_XML_LOAD_LAX;
            break;
        case 'd':
            debuglevel++;
            break;
        case 'C':
            cdb_load++;
            break;
        case 'R':
            cdb_load_flags = CDB_LOCK_REQUEST|CDB_LOCK_WAIT|CDB_LOCK_PARTIAL;
            break;
        case 'j':
            disable_fastmap = 1;
            break;
        case 'n':
            no_networking = 1;
            break;
        case 'F':
            switch (optarg[0]) {
            case 'x':
            case 'X':
                config_format = MAAPI_CONFIG_XML;
                break;
            case 'p':
            case 'P':
                config_format = MAAPI_CONFIG_XML_PRETTY;
                break;
            case 'n':
            case 'N':
            case 'o':
            case 'O':
                config_format = MAAPI_CONFIG_JSON;
                break;
            case 'j':
            case 'J':
                config_format = MAAPI_CONFIG_J;
                break;
            case 'c':
            case 'C':
                config_format = MAAPI_CONFIG_C;
                break;
            case 'i':
            case 'I':
                config_format = MAAPI_CONFIG_C_IOS;
                break;
            default:
                fatal(0, 1, "invalid format: %s (must be one of x, j, c, or i)",
                      optarg);
            }
            break;
        case 'W':
            flags |= MAAPI_CONFIG_WITH_DEFAULTS;
            break;
        case 'S':
            flags |= MAAPI_CONFIG_SHOW_DEFAULTS;
            break;
        case 'p':
            path = optarg;
            break;
        case 'P':
            xpath = optarg;
            break;
        case 's':
            ldb = sdb = CONFD_STARTUP;
            break;
        case 'O':
            ldb = CONFD_OPERATIONAL;
            flags |= MAAPI_CONFIG_OPER_ONLY;
            break;
        case 'f':
            filename = optarg;
            break;
        case 'l':
            load_config++;
            break;
        case 'm':
            flags |= MAAPI_CONFIG_MERGE;
            break;
        case 'r':
            flags |= MAAPI_CONFIG_REPLACE;
            break;
        case 'H':
            flags |= MAAPI_CONFIG_HIDE_ALL;
            break;
        case 'U':
            flags |= MAAPI_CONFIG_UNHIDE_ALL;
            break;
        case 'a':
            flags |= MAAPI_CONFIG_AUTOCOMMIT;
            break;
        case 'e':
            flags |= MAAPI_CONFIG_CONTINUE_ON_ERROR;
            break;
        case 'o':
            flags |= MAAPI_CONFIG_WITH_OPER;
            break;
        case 'M':
            flags |= MAAPI_CONFIG_WITH_SERVICE_META;
            break;
        case 'N':
            flags |= MAAPI_CONFIG_NO_PARENTS;
            break;
        case 'D':
            delete_config++;
            break;
        case 'i':
            attach_init++;
            break;
        case 'u':
            username = optarg;
            break;
        case 'c':
            context = optarg;
            break;
        case 'g':
            if (ngroups < (sizeof(groups)/sizeof(*groups))) {
                groups[ngroups] = optarg;
                ngroups++;
            } else {
                fatal(0, 1, "too many groups (max is %d)",
                      (int)(sizeof(groups)/sizeof(*groups)));
            }
            break;
        case 't':
            timer++;
            break;
        case 'h':
            usage();
            exit(0);
        case '?':
        default:
            usage();
            if (optopt == '?') {
                exit(0);
            } else {
                exit(1);
            }
        }
    }
    argc -= optind;
    argv += optind;

    if (argc == 0 && !filename)
        filename = "-";
    if (filename) {
        argc++;
        argv--;
        argv[0] = filename;
    }

    if (ngroups == 0) {
        groups[0] = username;
        ngroups = 1;
    }

    /* Initialize address to confd daemon */
    get_daemon_addr(NULL, 0);

    confd_init(progname, stderr, debuglevel);

    if ((sock = get_socket()) < 0 )
        fatal(errno, 1, "failed to open socket");

    if (cdb_load) {
        OK(cdb_connect(sock, CDB_DATA_SOCKET, addr, addrlen));
        OK(cdb_start_session2(sock, CDB_OPERATIONAL, cdb_load_flags));
    } else {
        OK(cnct(attach_init, load_config, username, context, groups, ngroups));
    }

    flags |= config_format;

    lap_timer(timer, TIMER_START);

    if (cdb_load) {

        for (; argc; argc--,argv++) {
            if (strcmp(argv[0], "-") == 0) {
                argv[0] = save_stdin();
                OK(cdb_load_str(sock, argv[0], 0));
                free(argv[0]);
            } else {
                OK(cdb_load_file(sock, abspath(argv[0]), 0));
            }
        }

        lap_timer(timer, TIMER_STOP);

        cdb_end_session(sock);
        cdb_close(sock);

    } else if (load_config) {
        if (delete_config) {
            OK(maapi_delete_all(sock, tid, MAAPI_DEL_ALL));
        }
        if (path) {
            OK(maapi_delete(sock, tid, path));
        }

        for (; argc; argc--,argv++) {
            if (strcmp(argv[0], "-") == 0) {
                int id, ssock, r;
                char buf[BUFSIZ];

                id = maapi_load_config_stream(sock, tid, flags);
                if (id <= 0) {
                    ERR("maapi_load_config_stream()", 1);
                }
                if ((ssock = get_socket()) < 0)
                    fatal(errno, 1, "failed to open socket");
                OK(confd_stream_connect(ssock, addr, addrlen, id, 0));
                while ((r = read(fileno(stdin), buf, sizeof(buf))) > 0) {
                    if (write(ssock, buf, r) != r) {
                        fatal(errno, 1, "failed to write to stream socket");
                    }
                }
                if (r < 0)
                    fatal(errno, 1, "failed to read from stdin");
                close(ssock);
                OK(maapi_load_config_stream_result(sock, id));
            } else {
                OK(maapi_load_config(sock, tid, flags, abspath(argv[0])));
            }
            if ((flags & (MAAPI_CONFIG_MERGE|MAAPI_CONFIG_REPLACE)) == 0)
                flags |= MAAPI_CONFIG_MERGE;
        }

        /* Don't apply for attached transaction
           - done by confd --start-phase1 for init transaction
           - otherwise up to the nb agent that started the transaction */
        if (!attached) {
            int aflags = 0;
            if (disable_fastmap) {
                aflags = MAAPI_COMMIT_NCS_NO_FASTMAP;
            }
            if (no_networking)
                aflags |= MAAPI_COMMIT_NCS_NO_NETWORKING;
            if (via_candidate) {
                OK(maapi_apply_trans_flags(sock, tid, 0, 0));
                OK(maapi_candidate_commit_persistent(sock, NULL));
            } else {
                OK(maapi_apply_trans_flags(sock, tid, 0, aflags));
            }
        }

        lap_timer(timer, TIMER_STOP);

        if (new_session_started) {
            maapi_end_user_session(sock);
        }
        maapi_close(sock);

    } else {   /* save */

        int id, ssock, r;
        FILE *f;
        char buf[BUFSIZ];

        if (strcmp(argv[0], "-") == 0) {
            f = stdout;
        } else {
            if ((f = fopen(argv[0], "w")) == NULL)
                fatal(errno, 1, "failed to open file %s", argv[0]);
        }
        if (xpath) {
            flags |= MAAPI_CONFIG_XPATH;
            path = xpath;
        }
        id = maapi_save_config(sock, tid, flags, path);
        if (id <= 0)
            ERR("maapi_save_config()", 1);
        if ((ssock = get_socket()) < 0 )
            fatal(errno, 1, "failed to open socket");
        OK(confd_stream_connect(ssock, addr, addrlen, id, 0));

        while ((r = read(ssock, buf, sizeof(buf))) > 0) {
            if (fwrite(buf, 1, r, f) != r)
                fatal(errno, 1, "failed to write output");
        }
        if (r < 0)
            fatal(errno, 1, "failed to read from stream socket");
        close(ssock);
        OK(maapi_save_config_result(sock, id));
        if (config_format == MAAPI_CONFIG_XML ||
            config_format == MAAPI_CONFIG_XML_PRETTY) {
            fprintf(f, "\n");
        }
        if (f != stdout) {
            if (fclose(f) != 0)
                fatal(errno, 1, "failed to write output");
        }

        lap_timer(timer, TIMER_STOP);

        if (new_session_started) {
            maapi_end_user_session(sock);
        }
        maapi_close(sock);
    }

    lap_timer(timer, TIMER_REPORT);

    exit(0);
}
