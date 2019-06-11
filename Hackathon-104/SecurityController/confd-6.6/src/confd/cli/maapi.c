/*    -*- C -*-
 *
 *  Copyright 2006 Tail-f Systems AB. All rights reserved.
 *
 *  This software is the confidential and proprietary
 *  information of Tail-f Systems AB.
 *
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/un.h>
#include <sys/poll.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdarg.h>
#include <getopt.h>

#include <stdio.h>

#include "confd_lib.h"
#include "confd_maapi.h"

/* This program is intended to be used from scripts run from the CLI
 * or as a Netconf action or extension.
 *
 * It will connect to an ongoing transaction and perform maapi function
 * on it. The target transaction is expected in the environment variables
 * CONFD_MAAPI_THANDLE.
 *
 * usage: maapi <op> <args>
 *
 * For example:
 *
 * maapi --get_keys /aaa:aaa/autentication/users/user
 *
 */

#ifdef NCS
#define PORT NCS_PORT
#define IPC_ADDR "NCS_IPC_ADDR"
#define IPC_PORT "NCS_IPC_PORT"
#define IPC_EXTADDR "NCS_IPC_EXTADDR"
#define IPC_EXTSOPATH "NCS_IPC_EXTSOPATH"
#define MAAPI_DEBUG "NCS_MAAPI_DEBUG"
#define MAAPI_THANDLE "NCS_MAAPI_THANDLE"
#define MAAPI_USID "NCS_MAAPI_USID"
#else
#define PORT CONFD_PORT
#define IPC_ADDR "CONFD_IPC_ADDR"
#define IPC_PORT "CONFD_IPC_PORT"
#define IPC_EXTADDR "CONFD_IPC_EXTADDR"
#define IPC_EXTSOPATH "CONFD_IPC_EXTSOPATH"
#define MAAPI_DEBUG "CONFD_MAAPI_DEBUG"
#define MAAPI_THANDLE "CONFD_MAAPI_THANDLE"
#define MAAPI_USID "CONFD_MAAPI_USID"
#endif

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

static void usage(char *cmd) {
    fprintf(stderr, "Usage: %s [options]\n", cmd);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  --get <paths>         get values for paths\n");
    fprintf(stderr, "  --exists <paths>      true if paths exists\n");
    fprintf(stderr, "  --keys <paths>        print instance keys for paths\n");
    fprintf(stderr, "  --delete <paths>      delete paths\n");
    fprintf(stderr, "  --create <paths>      create dynamic/optional "
            "element\n");
    fprintf(stderr, "  --insert              insert dynamic element\n");
    fprintf(stderr, "  --set <path1> <value1> <path2> <value2>\n");
    fprintf(stderr, "  --msg <to> <message> <from>\n");
    fprintf(stderr, "  --sysmsg <to> <message>\n");
    fprintf(stderr, "  --priomsg <to> <message>\n");
    fprintf(stderr, "  --xpath <expr>\n");
    fprintf(stderr, "  --readline\n");
    fprintf(stderr, "  --audit <user> <usid> <line>\n");
    fprintf(stderr, "  --xpath-must <expr>\n");
    fprintf(stderr, "    [--xpath-initial-path <path>]\n");
    fprintf(stderr, "  --save [saveoptions]\n");
    fprintf(stderr, "    saveoptions:\n");
    fprintf(stderr, "      --xml\n");
    fprintf(stderr, "      --curly\n");
#ifndef NCS
    fprintf(stderr, "      --cisco\n");
#endif
    fprintf(stderr, "      --with-defaults\n");
    fprintf(stderr, "      --show-defaults\n");
    fprintf(stderr, "  --roll                output rollback config\n");
    fprintf(stderr, "  --cliget <param>      get cli session param\n");
    fprintf(stderr, "  --cliset <p> <v>      set cli session param\n");
    fprintf(stderr, "  --cmd2path <cmd>      try to extract aaa path "
            "from cmd\n");
    fprintf(stderr, "  --cmd-path [pathopts] <path>\n");
    fprintf(stderr, "     cmdpathops:\n");
    fprintf(stderr, "       --is-delete\n");
    fprintf(stderr, "       --emit-parents\n");
    fprintf(stderr, "       --non-recursive\n");
    fprintf(stderr, "  --cmd-diff <path>\n");
    fprintf(stderr, "  --keypath-diff <path>\n");

    fprintf(stderr, "  --clicmd [cmdoptions] <str>\n");
    fprintf(stderr, "    cmdoptions:\n");
    fprintf(stderr, "      --get-io\n");
    fprintf(stderr, "      --no-fullpath\n");
    fprintf(stderr, "      --no-error\n");
    fprintf(stderr, "      --no-hidden\n");
    fprintf(stderr, "      --no-aaa\n");
    fprintf(stderr, "      --unhide <group>\n");
    fprintf(stderr, "  --loadcmds [loadcmdsoptions] <str>\n");
    fprintf(stderr, "    loadcmdsoptions:\n");
    fprintf(stderr, "      --chroot <path>\n");
    fprintf(stderr, "      --autocommit\n");
    fprintf(stderr, "      --curly\n");
    fprintf(stderr, "      --cisco (default)\n");
    fprintf(stderr, "      --continue-on-error\n");
    fprintf(stderr, "      --suppress-errors\n");
    fprintf(stderr, "  --loadfile [loadcmdsoptions] <str>\n");
    fprintf(stderr, "    loadcmdsoptions:\n");
    fprintf(stderr, "      --autocommit\n");
    fprintf(stderr, "      --continue-on-error\n");
    fprintf(stderr, "      --suppress-errors\n");
    fprintf(stderr, "  --copy-tree </from/path> </to/path>\n");
    fprintf(stderr, "  --revert              remove all changes\n");
    fprintf(stderr, "\n");
}

static struct option long_options[] = {
    {"get",     0, 0, 'g'},
    {"set",     0, 0, 's'},
    {"exists",  0, 0, 'e'},
    {"keys",    0, 0, 'k'},
    {"delete",  0, 0, 'd'},
    {"create",  0, 0, 'c'},
    {"insert",  0, 0, 'i'},
    {"verbose", 0, 0, 'v'},
    {"msg",     0, 0, 'm'},
    {"sysmsg",  0, 0, 'M'},
    {"priomsg",  0, 0, 'Q'},
    {"xpath",   1, 0, 'x'},
    {"readline",   0, 0, 'P'},
    {"xpath-must", 1, 0, 't'},
    {"xpath-initial-path", 1, 0, 'p'},
    {"cmd2path",  0, 0, 'T'},
    {"cmd-path",  0, 0, 'E'},
    {"cmd-diff",  0, 0, 'L'},
    {"keypath-diff",  0, 0, 'q'},
    {"clicmd",  0, 0, 'C'},
    {"cliget",  0, 0, 'G'},
    {"cliset",  0, 0, 'S'},
    {"no-hidden",  0, 0, 'H'},
    {"no-error",  0, 0, 'w'},
    {"no-aaa",  0, 0, 'o'},
    {"no-fullpath",  0, 0, 'F'},
    {"get-io",  0, 0, 'O'},
    {"unhide",  1, 0, 'U'},
    {"save",  0, 0, 'A'},
    {"roll",  0, 0, 'R'},
    {"xml",  0, 0, 'X'},
    {"curly",  0, 0, 'B'},
    {"is-delete",  0, 0, 'J'},
    {"emit-parents",  0, 0, 'K'},
    {"non-recursive",  0, 0, 'N'},
#ifndef NCS
    {"cisco",  0, 0, 'I'},
#endif
    {"with-defaults",  0, 0, 'W'},
    {"show-defaults",  0, 0, 'D'},
    {"loadcmds",  0, 0, 'l'},
    {"loadfile",  0, 0, 'n'},
    {"chroot",  1, 0, 'r'},
    {"autocommit",  0, 0, 'b'},
    {"continue-on-error",  0, 0, 'h'},
    {"suppress-errors",  0, 0, 'j'},
    {"merge", 0, 0, 'a'},
    {"audit", 0, 0, 'f'},
    {"copy-tree", 0, 0, 'u'},
    {"revert", 0, 0, 'z'},
    {0,         0, 0, 0}
};

#define GET_ELEM    'g'
#define SET_ELEM    's'
#define GET_KEYS    'k'
#define CREATE      'c'
#define EXISTS      'e'
#define DELETE      'd'
#define INSERT      'i'
#define MSG         'm'
#define SYSMSG      'M'
#define PRIOMSG     'Q'
#define XPATH_EXPR  'x'
#define XPATH_MUST  't'
#define XPATH_PATH  'p'
#define CLI_CMD     'C'
#define NO_FULLPATH 'F'
#define NO_HIDDEN   'H'
#define NO_ERROR    'w'
#define NO_AAA      'o'
#define GET_IO      'O'
#define UNHIDE      'U'
#define CLI_GET     'G'
#define CLI_SET     'S'
#define CMD2PATH    'T'
#define CMD_PATH    'E'
#define CMD_DIFF    'L'
#define KEYPATH_DIFF  'q'
#define SAVE        'A'
#define ROLL        'R'
#define XML         'X'
#define CURLY       'B'
#define IS_DELETE   'J'
#define EMIT_PARENTS 'K'
#define NON_RECURSIVE 'N'
#define CISCO       'I'
#define WITH_DEFAULTS 'W'
#define SHOW_DEFAULTS 'D'
#define LOADCMDS    'l'
#define LOADFILE    'n'
#define CHROOT      'r'
#define AUTOCOMMIT  'b'
#define CONTINUE_ON_ERROR  'h'
#define SUPPRESS_ERRORS  'j'
#define MERGE       'a'
#define VERBOSE     'v'
#define READLINE    'P'
#define AUDIT       'f'
#define COPY_TREE   'u'
#define REVERT      'z'

static int family, type, protocol;
static struct sockaddr *addr;
static socklen_t addrlen;
#ifdef EXTERNAL_IPC
static struct confd_ext_ipc_cbs *ecbs;
#endif

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

static void xtrace(char *str)
{
    printf("XTRACE: %s", str);
}

static void fatal_err(char *s)
{
    fprintf(stderr, "%s: %s - %s\n",
            s, confd_strerror(confd_errno), confd_lasterr());
    exit(1);
}

enum maapi_iter_ret keypath_iter(confd_hkeypath_t *kp,
                                 enum maapi_iter_op op,
                                 confd_value_t *oldv,
                                 confd_value_t *newv,
                                 void *state)
{
    char buf[BUFSIZ];
    char *opstr = "";
    if (op == MOP_CREATED)
        opstr = "created    ";
    else if (op == MOP_DELETED)
        opstr = "deleted    ";
    else if (op == MOP_MODIFIED)
        opstr = "modified   ";
    else if (op == MOP_VALUE_SET)
        opstr = "value set  ";
    else if (op == MOP_MOVED_AFTER)
        opstr = "moved after";
    else if (op == MOP_ATTR_SET)
        opstr = "attr set   ";

    confd_pp_kpath(buf, BUFSIZ, kp);
    fprintf(stdout, "%s: %s\n", opstr, buf);
    return ITER_RECURSE;
}

int main(int argc, char **argv)
{
    int msock;
    int usid = 0, thandle = 0, thandle_old = 0;
    int cmd = 0;
    int i;
    int res = CONFD_OK;
    int option_index;
    int verbose = 0;
    int debug_level = CONFD_SILENT;
    char *xpath_initial_path = "/";
    char *xpath_expr = NULL;
    char *usid_str, *thandle_str;
    int flags=0;
    int unhide=0;
    char **unhide_group;
    char chroot_path[256];
    int save_flags=0;
    int style_flags=0;
    int cmd_path_flags=0;
    int get_io=0;
    int load_flags=0;
    int report_error=1;

    unhide_group = (char **) malloc(sizeof(char *)*256);
    chroot_path[0] = '\0';

    /* argument processing first */

    while(1) {
        int c;

        c = getopt_long(argc, argv,
                        "abcdefghijklmnopqr:stuvwx"
                        "ABCDEFGHIJKLMNOPQRSU:VWX",
                        long_options, &option_index);

        if (c == -1) {
            if (!cmd) {
                usage(argv[0]);
                exit(2);
            }
            break;
        }

        switch(c) {
        case GET_ELEM:
        case SET_ELEM:
        case EXISTS:
        case GET_KEYS:
        case DELETE:
        case CREATE:
        case INSERT:
        case MSG:
        case SYSMSG:
        case PRIOMSG:
        case CLI_CMD:
        case CLI_GET:
        case CLI_SET:
        case COPY_TREE:
        case CMD2PATH:
        case CMD_PATH:
        case CMD_DIFF:
        case KEYPATH_DIFF:
        case SAVE:
        case ROLL:
        case READLINE:
        case LOADCMDS:
        case LOADFILE:
        case REVERT:
            if (cmd) {
                usage(argv[0]);
                exit(2);
            }
            cmd = c;
            break;
        case AUDIT:
            if (cmd) {
                usage(argv[0]);
                exit(2);
            }
            cmd = c;
            break;
        case XPATH_EXPR:
        case XPATH_MUST:
            if (cmd) {
                usage(argv[0]);
                exit(2);
            }
            cmd = c;
            if (c == XPATH_EXPR) {
                xpath_expr = optarg;
            } else {
                int tmplen = strlen(optarg) + strlen("boolean()") + 1;
                xpath_expr = malloc(tmplen);
                snprintf(xpath_expr, tmplen, "boolean(%s)", optarg);
            }
            break;
        case XPATH_PATH:
            if (optarg && (*optarg != '\000')) {
                xpath_initial_path = optarg;
            }
            break;
        case NO_HIDDEN:
            if (cmd != CLI_CMD) {
                usage(argv[0]);
                exit(2);
            }
            flags |= MAAPI_CMD_NO_HIDDEN;
            break;
        case NO_ERROR:
            if (cmd != CLI_CMD) {
                usage(argv[0]);
                exit(2);
            }
            report_error = 0;
            break;
        case NO_AAA:
            if (cmd != CLI_CMD) {
                usage(argv[0]);
                exit(2);
            }
            flags |= MAAPI_CMD_NO_AAA;
            break;
        case NO_FULLPATH:
            if (cmd != CLI_CMD) {
                usage(argv[0]);
                exit(2);
            }
            flags |= MAAPI_CMD_NO_FULLPATH;
            break;
        case GET_IO:
            if (cmd != CLI_CMD) {
                usage(argv[0]);
                exit(2);
            }
            get_io=1;
            break;
        case UNHIDE:
            if (cmd != CLI_CMD) {
                usage(argv[0]);
                exit(2);
            }
            if (unhide >= 255) {
                fprintf(stderr, "max 256 groups can be unhidden\n");
                exit(2);
            }
            unhide_group[unhide++] = strdup(optarg);
            break;
        case CHROOT:
            if (cmd != LOADCMDS) {
                usage(argv[0]);
                exit(2);
            }
            strncpy(chroot_path, optarg, sizeof(chroot_path)-1);
            break;
        case MERGE:
            if (cmd != LOADCMDS && cmd != LOADFILE) {
                usage(argv[0]);
                exit(2);
            }
            load_flags |= MAAPI_CONFIG_MERGE;
            break;
        case AUTOCOMMIT:
            if (cmd != LOADCMDS && cmd != LOADFILE) {
                usage(argv[0]);
                exit(2);
            }
            load_flags |= MAAPI_CONFIG_AUTOCOMMIT;
            break;
        case CONTINUE_ON_ERROR:
            if (cmd != LOADCMDS && cmd != LOADFILE) {
                usage(argv[0]);
                exit(2);
            }
            load_flags |= MAAPI_CONFIG_CONTINUE_ON_ERROR;
            break;
        case SUPPRESS_ERRORS:
            if (cmd != LOADCMDS && cmd != LOADFILE) {
                usage(argv[0]);
                exit(2);
            }
            load_flags |= MAAPI_CONFIG_SUPPRESS_ERRORS;
            break;
        case XML:
            style_flags |= MAAPI_CONFIG_XML;
            break;
        case CURLY:
            style_flags |= MAAPI_CONFIG_J;
            break;
        case CISCO:
            style_flags |= MAAPI_CONFIG_C;
            break;
        case WITH_DEFAULTS:
            save_flags |= MAAPI_CONFIG_WITH_DEFAULTS;
            break;
        case SHOW_DEFAULTS:
            save_flags |= MAAPI_CONFIG_SHOW_DEFAULTS;
            break;
        case IS_DELETE:
            cmd_path_flags |= MAAPI_FLAG_DELETE;
            break;
        case EMIT_PARENTS:
            cmd_path_flags |= MAAPI_FLAG_EMIT_PARENTS;
            break;
        case NON_RECURSIVE:
            cmd_path_flags |= MAAPI_FLAG_NON_RECURSIVE;
            break;
        case VERBOSE:
            verbose=1;
            break;
        default:
            usage(argv[0]);
            exit(2);
        }
    }

    if (cmd == 0) {
        usage(argv[0]);
        exit(2);
    }

    get_daemon_addr(NULL, 0);

    if (getenv(MAAPI_DEBUG))
        debug_level = CONFD_DEBUG;

    confd_init("maapi", stderr, debug_level);

    if (argc < 2) {
        usage(argv[0]);
        exit(2);
    }

    if ((msock = get_socket()) < 0 )
        confd_fatal("Failed to open socket\n");

    if (maapi_connect(msock, addr, addrlen) < 0)
        fatal_err("Failed to confd_connect() to confd");

    usid_str = getenv(MAAPI_USID);

    if (usid_str)
        usid = atoi(usid_str);
    else if (cmd != MSG && cmd != SYSMSG && cmd != PRIOMSG) {
        fprintf(stderr,
                "Error: no %s environment variable found\n", MAAPI_USID);
        exit(2);
    }


    thandle_str = getenv(MAAPI_THANDLE);
    if (thandle_str)
        thandle = atoi(thandle_str);
    else if (cmd != MSG && cmd != SYSMSG && cmd != PRIOMSG) {
        fprintf(stderr,
                "Error: no %s environment variable found\n", MAAPI_THANDLE);
        exit(2);
    }


    if (cmd != MSG && cmd != SYSMSG && cmd != PRIOMSG) {
        if ((maapi_attach2(msock, -1, usid, thandle)) != CONFD_OK)
            fatal_err("Failed to attach");
    }

    switch (cmd) {
    case SAVE:
        if (verbose) printf("save\n");
        if (style_flags == 0) {
            save_flags = save_flags | MAAPI_CONFIG_J;
        }
        else {
            save_flags = save_flags | style_flags;
        }
        if (optind == argc) {
            int id;
            int streamsock;
            char buf[1024];
            int n;
            int outfd = fileno(stdout);

            id = maapi_save_config(msock, thandle, save_flags, NULL);

            if (id == CONFD_ERR) {
                fatal_err("Failed to save path");
            }

            streamsock = get_socket();
            if (streamsock < 0)
                confd_fatal("Failed to allocate socket\n");
            if (confd_stream_connect(streamsock, addr, addrlen, id, 0) !=
                CONFD_OK) {
                fatal_err("Failed to save path");
            }

            n = read(streamsock, buf, 1024);

            while(n>0) {
                if (write(outfd, buf, n) < 0)
                    confd_fatal("Failed to write to stdout\n");
                n = read(streamsock, buf, 1024);
            }

            close(streamsock);
        }
        for(i = optind ; i < argc ; i++) {
            int id;
            int streamsock;
            char buf[1024];
            int n;
            int outfd = fileno(stdout);

            id = maapi_save_config(msock, thandle, save_flags, argv[i]);

            if (id == CONFD_ERR) {
                fatal_err("Failed to save path");
            }

            streamsock = get_socket();
            if (streamsock < 0)
                confd_fatal("Failed to allocate socket\n");
            if (confd_stream_connect(streamsock, addr, addrlen, id, 0) !=
                CONFD_OK) {
                fatal_err("Failed to save path");
            }

            n = read(streamsock, buf, 1024);

            while(n>0) {
                if (write(outfd, buf, n) < 0)
                    confd_fatal("Failed to write to stdout\n");
                n = read(streamsock, buf, 1024);
            }

            close(streamsock);
        }
        break;

    case ROLL:
        if (verbose) printf("rollback file\n");
        if (optind == argc) {
            int id;
            int streamsock;
            char buf[1024];
            int n;
            int outfd = fileno(stdout);

            id = maapi_roll_config(msock, thandle, NULL);

            if (id == CONFD_ERR) {
                fatal_err("Failed to roll path");
            }

            streamsock = get_socket();
            if (streamsock < 0)
                confd_fatal("Failed to allocate socket\n");
            if (confd_stream_connect(streamsock, addr, addrlen, id, 0) !=
                CONFD_OK) {
                fatal_err("Failed to roll path");
            }

            n = read(streamsock, buf, 1024);

            while(n>0) {
                if (write(outfd, buf, n) < 0)
                    confd_fatal("Failed to write to stdout\n");
                n = read(streamsock, buf, 1024);
            }

            close(streamsock);
        }
        for(i = optind ; i < argc ; i++) {
            int id;
            int streamsock;
            char buf[1024];
            int n;
            int outfd = fileno(stdout);

            id = maapi_roll_config(msock, thandle, argv[i]);

            if (id == CONFD_ERR) {
                fatal_err("Failed to roll path");
            }

            streamsock = get_socket();
            if (streamsock < 0)
                confd_fatal("Failed to allocate socket\n");
            if (confd_stream_connect(streamsock, addr, addrlen, id, 0) !=
                CONFD_OK) {
                fatal_err("Failed to roll path");
            }

            n = read(streamsock, buf, 1024);

            while(n>0) {
                if (write(outfd, buf, n) < 0)
                    confd_fatal("Failed to write to stdout\n");
                n = read(streamsock, buf, 1024);
            }

            close(streamsock);
        }
        break;

    case GET_ELEM:
        if (verbose) printf("get:\n");
        for(i = optind ; i < argc ; i++) {
            char buf[BUFSIZ];
            confd_value_t v;
            int n;

            if (verbose) printf("path = %s\n", argv[i]);

            if (maapi_get_elem(msock, thandle, &v, argv[i]) != CONFD_OK)
                fatal_err("Failed to read value");

            n = confd_pp_value(buf, BUFSIZ, &v);

            if (n < 0)
                confd_fatal("failed to format value\n");

            if (n > BUFSIZ)
                confd_fatal("value was truncated\n");

            if (printf("%s\n", buf) < 0)
                exit(2);

        }
        break;
    case SET_ELEM:
        if (verbose) printf("set:\n");
        for(i = optind ; i < argc ; i+=2) {
            /* check for <path> <value> */
            if (i+1 >= argc) {
                usage(argv[0]);
                exit(2);
            }

            if (verbose) printf("path = %s\n", argv[i]);
            if (verbose) printf("value = %s\n", argv[i+1]);

            res = maapi_set_elem2(msock, thandle, argv[i+1], argv[i]);

            if (res != CONFD_OK)
                fatal_err("Failed to set value");
        }
        break;
    case COPY_TREE:
        if (verbose) printf("copy-tree:\n");
        for(i = optind ; i < argc ; i+=2) {
            /* check for </from/path> </to/path> */
            if (i+1 >= argc) {
                usage(argv[0]);
                exit(2);
            }

            if (verbose) printf("from path = %s\n", argv[i]);
            if (verbose) printf("to path = %s\n", argv[i+1]);

            res = maapi_copy_tree(msock, thandle, argv[i], argv[i+1]);

            if (res != CONFD_OK)
                fatal_err("Failed to copy tree");
        }
        break;
    case REVERT:
        if (verbose) printf("revert:\n");

        res = maapi_revert(msock, thandle);

        if (res != CONFD_OK)
            fatal_err("Failed to revert changes");

        break;
    case AUDIT:
        if (verbose) printf("audit:\n");

        i = optind;

        res = maapi_cli_accounting(msock, argv[i], atoi(argv[i+1]), argv[i+2]);

        if (res != CONFD_OK)
            fatal_err("Error audit");

        break;
    case GET_KEYS:
        if (verbose) printf("keys:\n");
        for(i = optind ; res == CONFD_OK && i < argc ; i++) {
            struct maapi_cursor mc;

            if (verbose) printf("path = %s\n", argv[i]);

            if (maapi_init_cursor(msock, thandle, &mc, argv[i]) != CONFD_OK)
                fatal_err("Failed to init cursor");

            maapi_get_next(&mc);
            while(mc.n != 0) {
                int x;

                for(x=0 ; x < mc.n ; x++) {
                    char buf[BUFSIZ];
                    int n = confd_pp_value(buf, BUFSIZ, &mc.keys[x]);

                    if (n <= 0)
                        confd_fatal("Failed to format value\n");

                    if (n > BUFSIZ)
                        confd_fatal("Error: value was truncated\n");

                    if (x != 0) printf(" ");
                    if (printf("%s", buf) < 0)
                        exit(2);

                }
                printf("\n");
                maapi_get_next(&mc);
            }
            maapi_destroy_cursor(&mc);
        }
        break;
    case CREATE:
        if (verbose) printf("create:\n");
        for(i = optind ; res == CONFD_OK && i < argc ; i++) {
            res = maapi_create(msock, thandle, argv[i]);

            if (verbose) printf("path = %s\n", argv[i]);

            if (res != CONFD_OK)
                fatal_err("Failed to create elem");
        }
        break;
    case EXISTS:
        if (verbose) printf("exists:\n");
        for(i = optind ; res == CONFD_OK && i < argc ; i++) {
            res = maapi_exists(msock, thandle, argv[i]);

            if (verbose) printf("path = %s\n", argv[i]);

            if (res == 0)
                res = CONFD_ERR;
            else if (res != 1) {
                fatal_err("Exists check failed");
                res = CONFD_ERR;
            }
            else
                res = CONFD_OK;
        }
        break;
    case DELETE:
        if (verbose) printf("delete:\n");
        for(i = optind ; res == CONFD_OK && i < argc ; i++) {
            if (verbose) printf("path = %s\n", argv[i]);

            res = maapi_delete(msock, thandle, argv[i]);

            if (res != CONFD_OK)
                fatal_err("Failed to delete elem");
        }
        break;
    case INSERT:
        if (verbose) printf("insert:\n");

        for(i = optind ; res == CONFD_OK && i < argc ; i++) {
            res = maapi_insert(msock, thandle, argv[i]);

            if (verbose) printf("path = %s\n", argv[i]);

            if (res != CONFD_OK)
                fatal_err("Failed to create elem");
        }
        break;
    case MSG:
        if (verbose) printf("message:\n");
        if (optind+3 != argc) {
            usage(argv[0]);
            exit(2);
        }

        if (verbose) printf("to = %s\n", argv[optind]);
        if (verbose) printf("message = %s\n", argv[optind+1]);
        if (verbose) printf("sender = %s\n", argv[optind+2]);

        res = maapi_user_message(msock, argv[optind], argv[optind+1],
                                 argv[optind+2]);

        if (res != CONFD_OK)
            fatal_err("Failed to send message");
        break;
    case SYSMSG:
        if (verbose) printf("system message:\n");
        if (optind+2 != argc) {
            usage(argv[0]);
            exit(2);
        }

        if (verbose) printf("to = %s\n", argv[optind]);
        if (verbose) printf("message = %s\n", argv[optind+1]);

        res = maapi_sys_message(msock, argv[optind], argv[optind+1]);

        if (res != CONFD_OK)
            fatal_err("Failed to send system message");
        break;
    case PRIOMSG:
        if (verbose) printf("prio message:\n");
        if (optind+2 != argc) {
            usage(argv[0]);
            exit(2);
        }

        if (verbose) printf("to = %s\n", argv[optind]);
        if (verbose) printf("message = %s\n", argv[optind+1]);

        res = maapi_prio_message(msock, argv[optind], argv[optind+1]);

        if (res != CONFD_OK)
            fatal_err("Failed to send prio message");
        break;
    case XPATH_EXPR:
    case XPATH_MUST:
    {
        char *res_str = NULL;
        if (verbose) {
            printf("\nInitial path: %s"
                   "\nExpression:   %s\n", xpath_initial_path, xpath_expr);
        }

        res = maapi_xpath_eval_expr(msock, thandle, xpath_expr, &res_str,
                                    verbose ? xtrace : NULL,
                                    xpath_initial_path);

        if (res != CONFD_OK)
            confd_fatal("%s\n", confd_lasterr());

        if (verbose) printf("\nResult: ");

        printf("%s\n", res_str);
        free(res_str);

        if (verbose) printf("\n");
        break;
    }
    case CLI_CMD:

        if (get_io) {
            if (verbose) printf("cmd io:\n");
            /* only do output at this point */
            for(i = optind ; res == CONFD_OK && i < argc ; i++) {
                struct pollfd fds[2];
                struct pollfd *fdsptr = fds;
                int nfds;
                int id;
                int streamsock;
                char buf[1024];
                int n;
                int infd = fileno(stdin);
                int outfd = fileno(stdout);

                id = maapi_cli_cmd_io2(msock, usid, argv[i], strlen(argv[i]),
                                       flags, unhide_group,
                                       unhide);

                if (id == CONFD_ERR) {
                    if (report_error)
                        fatal_err("Failed to run cmd");
                    else
                        exit(1);
                }

                streamsock = get_socket();
                if (streamsock < 0) {
                    if (report_error)
                        confd_fatal("Failed to allocate socket\n");
                    else
                        exit(1);
                }
                if (confd_stream_connect(streamsock, addr, addrlen, id, 0) !=
                    CONFD_OK) {
                    if (report_error)
                        fatal_err("Failed to save path");
                    else
                        exit(1);
                }


                fds[0].fd = infd;
                fds[0].events = POLLIN;

                fds[1].fd = streamsock;
                fds[1].events = POLLIN;
                nfds = 2;

                while(1) {
                    poll(fdsptr, nfds, -1);

                    if (fds[0].revents & (POLLIN | POLLHUP)) {
                        char buf[1024];
                        int n;


                        n = read(fds[0].fd, buf, 1024);

                        if (n < 0) {
                            /* got read error on stdin */
                            confd_fatal("Failed to read from stdin\n");
                        }
                        else if (n == 0) {
                            /* end of file on stdin */
                            /* shutdown(fds[1].fd, SHUT_WR); */
                            /* wait for the server to close */
                            if (fdsptr == fds) {
                                fdsptr++;
                                nfds--;
                            }
                            fds[0].revents = 0;
                        }
                        else {
                            if (write(streamsock, buf, n) < 0) {
                                if (report_error)
                                    confd_fatal("Failed to write to maapi\n");
                                else
                                    exit(1);
                            }
                        }
                    }
                    else if (fds[0].revents) {
                        /* stdin closed */
                        /* shutdown(fds[1].fd, SHUT_WR); */
                        /* wait for the server to close */
                        if (fdsptr == fds) {
                            fdsptr++;
                            nfds--;
                        }
                        fds[0].revents = 0;
                    }

                    if (fds[1].revents & POLLIN) {
                        n = read(streamsock, buf, 1024);

                        if (n < 0) {
                            /* read error from server */
                            close(streamsock);
                            break;
                        }
                        else if (n == 0) {
                            close(streamsock);
                            break;
                        }
                        else {
                            if (write(outfd, buf, n) < 0) {
                                if (report_error)
                                    confd_fatal("Failed to write to stdout\n");
                                else
                                    exit(1);
                            }
                        }
                    }
                    else if (fds[1].revents) {
                        /* got error on socket, close */
                        close(streamsock);
                        break;
                    }
                }
                res = maapi_cli_cmd_io_result(msock, id);

                if (res != CONFD_OK) {
                    if (report_error)
                        fatal_err("Error executing command");
                    else
                        exit(1);
                }
            }
        }
        else {
            if (verbose) printf("cmd:\n");

            for(i = optind ; res == CONFD_OK && i < argc ; i++) {
                if (unhide) {
                    res = maapi_cli_cmd4(msock, usid, argv[i], strlen(argv[i]),
                                         flags, unhide_group, unhide);
                }
                else {
                    res = maapi_cli_cmd2(msock, usid, argv[i], strlen(argv[i]),
                                         flags);
                }

                if (verbose) printf("cmd = %s\n", argv[i]);

                if (res != CONFD_OK) {
                    if (report_error)
                        fatal_err("Error executing command");
                    else
                        exit(1);
                }
            }
        }
        break;
    case CLI_GET:
        if (verbose) printf("cliget:\n");

        for(i = optind ; res == CONFD_OK && i < argc ; i++) {
            char buf[BUFSIZ];

            buf[0] = '\0';

            res = maapi_cli_get(msock, usid, argv[i], buf, sizeof(buf));

            if (verbose) printf("cliget = %s\n", argv[i]);

            if (res != CONFD_OK)
                fatal_err("Error cliget");

            if (printf("%s\n", buf) < 0)
                exit(2);
        }
        break;
    case CLI_SET:
        if (verbose) printf("cliset:\n");

        for(i = optind ; res == CONFD_OK && i < argc ; i+=2) {
            /* check for <path> <value> */
            if (i+1 >= argc) {
                usage(argv[0]);
                exit(2);
            }

            if (verbose) printf("cliset %s %s\n", argv[i], argv[i+1]);

            res = maapi_cli_set(msock, usid, argv[i], argv[i+1]);

            if (res != CONFD_OK)
                fatal_err("Error cliset");
        }
        break;
    case CMD2PATH:
        if (verbose) printf("cmd2path:\n");

        for(i = optind ; res == CONFD_OK && i < argc ; i++) {
            char ns[1024];
            char path[1024];

            /* check for <path> */
            if (i >= argc) {
                usage(argv[0]);
                exit(2);
            }

            if (verbose) printf("cmd2path %s\n", argv[i]);

            res = maapi_cli_cmd_to_path(msock, argv[i], ns, 1024, path, 1024);

            if (res != CONFD_OK)
                fatal_err("Error cmd2path");

            printf("%s\n%s\n", ns, path);
        }
        break;
    case CMD_PATH:
        if (verbose) printf("cmd-path:\n");
        for(i = optind ; res == CONFD_OK && i < argc ; i++) {
            char buf[BUFSIZ];

            res = maapi_cli_path_cmd(msock, thandle, buf, sizeof(buf),
                                     cmd_path_flags, argv[i]);

            if (verbose) printf("path = %s\n", argv[i]);

            if (res != CONFD_OK)
                fatal_err("cmd-path failed");

            if (printf("%s", buf) < 0)
                exit(2);
        }
        break;
    case CMD_DIFF:
        if (verbose) printf("cmd-diff:\n");

        if ((thandle_old = maapi_start_trans(msock, CONFD_RUNNING,
                                             CONFD_READ)) < 0)
                fatal_err("cmd-diff failed");

        for(i = optind ; res == CONFD_OK && i < argc ; i++) {
            char buf[BUFSIZ];

            res = maapi_cli_diff_cmd(msock, thandle, thandle_old, buf,
                                     sizeof(buf),
                                     cmd_path_flags, argv[i]);

            if (verbose) printf("path = %s\n", argv[i]);

            if (res != CONFD_OK)
                fatal_err("cmd-diff failed");

            if (printf("%s", buf) < 0)
                exit(2);
        }
        break;
    case KEYPATH_DIFF:
        if (verbose) printf("keypath-diff:\n");

        if ((msock < 0) ||
            (maapi_load_schemas(msock) != CONFD_OK))
            fatal_err("Failed to load schema");

        if (verbose) printf("Schema loaded\n");

        if (maapi_attach2(msock, 0, usid, thandle) != CONFD_OK)
            confd_fatal("Failed to attach: %s\n", confd_lasterr());

        for(i = optind ; res == CONFD_OK && i < argc ; i++) {

            if (verbose) printf("Keypath = %s\n", argv[i]);

            if (maapi_keypath_diff_iterate(msock, thandle,
                                           keypath_iter, 0,
                                           NULL, argv[i]) != CONFD_OK)
                fatal_err("keypath-diff failed");

        }
        break;
    case LOADCMDS:
        if (style_flags == 0)
            style_flags = MAAPI_CONFIG_C;

        load_flags = load_flags | style_flags;

        for(i = optind ; res == CONFD_OK && i < argc ; i++) {
            if (verbose) printf("loadcmds: %s\n", argv[i]);
            res = maapi_load_config_cmds(msock, thandle,
                                         load_flags,
                                         argv[i], chroot_path);

            if (verbose) printf("chroot = %s\n", chroot_path);

            if (res != CONFD_OK)
                fatal_err("loadcmds failed");

        }
        break;
    case LOADFILE:
        if (style_flags == 0)
            style_flags = MAAPI_CONFIG_C;

        load_flags = load_flags | style_flags;

        for(i = optind ; res == CONFD_OK && i < argc ; i++) {
            if (verbose) printf("loadfile: %s\n", argv[i]);
            res = maapi_load_config(msock, thandle, load_flags, argv[i]);

            if (res != CONFD_OK)
                fatal_err("loadfile failed");

        }
        break;
    case READLINE: {
        char buf[BUFSIZ];
        if (verbose) printf("readline:\n");
        res = maapi_cli_prompt(msock, usid, "", 1, buf, BUFSIZ);
        if (res != CONFD_OK)
            fatal_err("Failed to readline");
        if (printf("%s", buf) < 0)
            exit(2);
        break;
    }
    }

    if (res == CONFD_OK)
        exit(0);
    else
        exit(1);
}

