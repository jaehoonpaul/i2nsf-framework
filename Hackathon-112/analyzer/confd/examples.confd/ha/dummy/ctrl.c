/*
 * Copyright 2005,2006,2007 Tail-f Systems AB
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

#include <assert.h>

#include <confd_lib.h>
#include <confd_ha.h>
#include <confd_cdb.h>

#ifndef max
#define max(a, b) ((a) > (b) ? (a) : (b))
#endif

static char *progname = "cdb_set";
static enum confd_debug_level debug = CONFD_SILENT;
static FILE *debugf = NULL;
static struct sockaddr_in addr; /* Keeps address to confd daemon */
static enum cdb_db_type db = CDB_RUNNING;

#define OK(E) assert((E) == CONFD_OK)


void fatal(char *str)
{
    fprintf(stderr, "%s: fatal: %s\n", progname, str);
    exit(1);
}

static void start_session(int s, enum cdb_db_type d)
{
    int retry = 5;
    while (retry) {
        if (cdb_start_session(s, db) == CONFD_OK)
            return;
        if (confd_errno == CONFD_ERR_LOCKED) {
            if (debugf) {
                fprintf(debugf, "start_session locked, retry %d\n", retry);
            }
            usleep(500000);
            retry--;
            continue;
        }
        fatal("start session failed");
    }
    fatal("start session failed (after repeated retries)");
}

static void do_cdb_get(char *argv[]) /* <key> */
{
    int cs; confd_value_t val; char tmpbuf[BUFSIZ];

    assert((cs = socket(PF_INET, SOCK_STREAM, 0)) >= 0);
    OK(cdb_connect(cs, CDB_DATA_SOCKET,
                   (struct sockaddr *)&addr, sizeof(addr)));
    start_session(cs, db);
    OK(cdb_get(cs, &val, argv[0]));
    confd_pp_value(tmpbuf, BUFSIZ, &val);
    printf("%s\n", tmpbuf);
    OK(cdb_end_session(cs));
    OK(cdb_close(cs));
}

static void do_cdb_set(char *argv[]) /* <key> <value> */
{
    int cs;
    assert((cs = socket(PF_INET, SOCK_STREAM, 0)) >= 0);
    OK(cdb_connect(cs, CDB_DATA_SOCKET,
                   (struct sockaddr *)&addr, sizeof(addr)));
    start_session(cs, db);
    OK(cdb_set_elem2(cs, argv[0], argv[1]));
    OK(cdb_end_session(cs));
    OK(cdb_close(cs));
}

static void do_cdb_create(char *argv[]) /* <key> */
{
    int cs;
    assert((cs = socket(PF_INET, SOCK_STREAM, 0)) >= 0);
    OK(cdb_connect(cs, CDB_DATA_SOCKET,
                   (struct sockaddr *)&addr, sizeof(addr)));
    start_session(cs, db);
    OK(cdb_create(cs, argv[0]));
    OK(cdb_end_session(cs));
    OK(cdb_close(cs));
}

static void do_sleep(char *argv[]) /* <seconds> */
{
    int sec = atoi(argv[0]);
    int cs;
    assert((cs = socket(PF_INET, SOCK_STREAM, 0)) >= 0);
    OK(cdb_connect(cs, CDB_DATA_SOCKET,
                   (struct sockaddr *)&addr, sizeof(addr)));
    start_session(cs, db);
    sleep(sec);
    OK(cdb_close(cs));
}


static void benone(char *argv[])
{
    int s;
    assert((s = socket(PF_INET, SOCK_STREAM, 0)) >= 0);
    OK(confd_ha_connect(s, (struct sockaddr *)&addr, sizeof(addr), progname));
    OK(confd_ha_benone(s));
    close(s);
}

static void bemaster(char *argv[]) /* <nodename> */
{
    int s;
    confd_value_t nodeid;
    CONFD_SET_BUF(&nodeid, (unsigned char*)argv[0], strlen(argv[0]));
    assert((s = socket(PF_INET, SOCK_STREAM, 0)) >= 0);
    OK(confd_ha_connect(s, (struct sockaddr *)&addr, sizeof(addr), progname));
    OK(confd_ha_bemaster(s, &nodeid));
    close(s);
}

static void beslave(char *argv[]) /* <nodename> <mastername> <masterip> */
{
    int s;
    confd_value_t nodeid;
    struct confd_ha_node m;
    char *master = argv[1]; char *master_ip = argv[2];
    CONFD_SET_BUF(&nodeid, (unsigned char*)argv[0], strlen(argv[0]));
    assert((s = socket(PF_INET, SOCK_STREAM, 0)) >= 0);
    OK(confd_ha_connect(s, (struct sockaddr *)&addr, sizeof(addr), progname));
    m.af = AF_INET;
    CONFD_SET_BUF(&m.nodeid, (unsigned char*)master, strlen(master));
    inet_pton(AF_INET, master_ip, &m.addr.ip4);
    OK(confd_ha_beslave(s, &nodeid, &m, 1));
    close(s);
}

static void dead_slave(char *argv[]) /* <slavename> */
{
    int s;
    confd_value_t nodeid;
    CONFD_SET_BUF(&nodeid, (unsigned char*)argv[0], strlen(argv[0]));
    assert((s = socket(PF_INET, SOCK_STREAM, 0)) >= 0);
    OK(confd_ha_connect(s, (struct sockaddr *)&addr, sizeof(addr), progname));
    OK(confd_ha_slave_dead(s, &nodeid));
    close(s);
}

static void ha_status(char *argv[])
{
    int s;
    struct confd_ha_status status;
    assert((s = socket(PF_INET, SOCK_STREAM, 0)) >= 0);
    OK(confd_ha_connect(s, (struct sockaddr *)&addr, sizeof(addr), progname));
    OK(confd_ha_get_status(s, &status));
    close(s);
    switch (status.state) {
    case CONFD_HA_STATE_NONE:
        printf("none\n");
        break;
    case CONFD_HA_STATE_SLAVE:
    {
        char str[128];
        assert(status.num_nodes == 1);
        memset(str, 0, sizeof(str));
        memcpy(str, CONFD_GET_BUFPTR(&status.nodes[0].nodeid),
               max((sizeof(str)-1),CONFD_GET_BUFSIZE(&status.nodes[0].nodeid)));
        printf("slave master: %s\n", str);
    }
    break;
    case CONFD_HA_STATE_MASTER:
    {
        int i;
        printf("master %d slaves", status.num_nodes);
        for (i=0; i<status.num_nodes; i++) {
            char str[128];
            memset(str, 0, sizeof(str));
            memcpy(str, CONFD_GET_BUFPTR(&status.nodes[i].nodeid),
                   max(127, CONFD_GET_BUFSIZE(&status.nodes[i].nodeid)));
            printf(" %s", str);
        }
        printf("\n");
    }
        break;
    default:
        printf("UNKNOWN?\n");
        break;
    }
}



int main(int argc, char *argv[])
{
    char *confd_addr = "127.0.0.1";
    int confd_port = CONFD_PORT;
    int c;

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
    while ((c = getopt(argc, argv, "da:p:or")) != EOF) {
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
        case 'r':
            db = CDB_RUNNING;
            break;
        case 'o':
            db = CDB_OPERATIONAL;
            break;
        default:
            printf("huh?\n");
            exit(1);
        }
    }
    argc -= optind;
    argv += optind;

    /* Initialize address to confd daemon */
    {
        addr.sin_addr.s_addr = inet_addr(confd_addr);
        addr.sin_family = AF_INET;
        addr.sin_port = htons(confd_port);
    }
    confd_init(progname, stderr, debug);

#ifdef EXTRA_DEBUG
    /* always save trace output somewhere */
    if (debug == CONFD_SILENT) {
        debugf = fopen("_tmp_debug", "w");
        debug = CONFD_TRACE;
        confd_set_debug(debug, debugf);
    } else
#endif

    struct cmd_t {
        char *cmd;
        void (*func)(char **);
        int nargs;
    } cmds[] = {{ "get", do_cdb_get, 1 },
                { "g", do_cdb_get, 1 },
                { "set", do_cdb_set, 2 },
                { "s", do_cdb_set, 2 },
                { "create", do_cdb_create, 1 },
                { "c", do_cdb_create, 1 },
                { "master", bemaster, 1 },
                { "slave", beslave, 3 },
                { "none", benone, 0 },
                { "dead_slave", dead_slave, 1 },
                { "ha_status", ha_status, 0 },
                { "sleep", do_sleep, 1 },
                { NULL, NULL, 0 }};

    while (argc > 0) {
        char *cmd = argv[0];
        struct cmd_t *cc;
        argc--;
        argv++;

        for (cc = cmds; cc->cmd != NULL; cc++) {
            if (strcmp(cmd, cc->cmd) == 0) {
                break;
            }
        }
        if (cc->cmd) {
            if (argc < cc->nargs) {
                fprintf(stderr, "to few arguments to cmd %s", cc->cmd);
                fatal("too few arguments");
            }
            cc->func(argv);
            argc -= cc->nargs;
            argv += cc->nargs;
        } else {
            fprintf(stderr, "unknown command: %s\n", cmd);
            fatal("unknown command");
        }
    }

    exit(0);
}
