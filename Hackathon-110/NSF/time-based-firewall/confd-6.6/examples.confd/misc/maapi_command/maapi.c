
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/poll.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdarg.h>


#include <stdio.h>

#include "confd_lib.h"
#include "confd_maapi.h"

#define OK(E) assert((E) == CONFD_OK)


/* global variables */
static int sock, tid;
static char *user = "admin";
static const char *groups[] = {"admin"};
static int debuglevel = CONFD_SILENT;
static char *context = "maapi";
static enum confd_dbname dbname = CONFD_RUNNING;


void pval(confd_value_t *v)
{
    char buf[BUFSIZ];
    confd_pp_value(buf, BUFSIZ, v);
    fprintf(stderr, "%s\n", buf);
}


static int cnct()
{

    struct sockaddr_in addr;
    struct confd_ip ip;

    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(4565);

    if ((sock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        confd_fatal("Failed to open socket\n");

    if (maapi_connect(sock, (struct sockaddr*)&addr,
                      sizeof (struct sockaddr_in)) < 0)
        confd_fatal("Failed to confd_connect() to confd \n");

    ip.af = AF_INET;
    inet_pton(AF_INET, "127.0.0.1", &ip.ip.v4);

    OK(maapi_start_user_session(sock, user, context, groups, 1,
                                &ip, CONFD_PROTO_TCP));

    if ((tid = maapi_start_trans(sock, dbname, CONFD_READ_WRITE)) < 0)
        confd_fatal("failed to start trans \n");
    return CONFD_OK;
}

static int err()
{
    fprintf(stderr, "ERROR: %d %s\n", confd_errno,
            confd_strerror(confd_errno));
    exit(1);
}


/* paths must be ns prefixed as in     */
/* ./maapi -D /aaa:aaa/authentication  */

static int usage()
{
    fprintf(stderr, "Usage: maapi <-e -path> |  <-p <path> [-v <strval>]\n");
    exit(1);
}


enum mop {
    M_NONE = 0,
    M_GET = 1,
    M_SET = 2,
    M_EXISTS = 3,
    M_DELETE = 4
};


int main(int argc, char **argv)
{
    int c;
    char *path = NULL;
    char *valstr = NULL;
    confd_value_t v;
    char buf[BUFSIZ];
    enum mop op = M_NONE;
    while ((c = getopt(argc, argv, "tdPSrcp:v:e:D:")) != -1) {
        switch(c) {
        case 'r':
            dbname = CONFD_RUNNING;
            break;
        case 't':
            debuglevel = CONFD_TRACE;
            break;
        case 'd':
            debuglevel = CONFD_DEBUG;
            break;
        case 'P':
            debuglevel = CONFD_PROTO_TRACE;
            break;
        case 'S':
            debuglevel = CONFD_SILENT;
            break;
        case 'c':
            dbname = CONFD_CANDIDATE;
            break;
        case 'D':
            op = M_DELETE;
            path = optarg;
            break;
        case 'p':
            op = M_GET;
            path = optarg;
            break;
        case 'v':
            op = M_SET;
            valstr = optarg;
            break;
        case 'e':
            op = M_EXISTS;
            path = optarg;
            break;
        default:
            usage();
        }
    }
    if (path == NULL)
        usage();
    confd_init("maapi", stderr, debuglevel);
    OK(cnct());
    if (op == M_GET) {
        if (maapi_get_elem(sock, tid, &v, path) != CONFD_OK)
            err();
        confd_pp_value(buf, BUFSIZ, &v);
        printf("%s\n", buf);
    }
    else if (op == M_SET) {
        if (valstr == NULL)
            usage();
        if (maapi_set_elem2(sock, tid, valstr, path) != CONFD_OK)
            err();
        if (maapi_apply_trans(sock, tid, 0) != CONFD_OK)
            err();
        printf("OK\n");
    }
    else if (op == M_EXISTS) {
        int eval;
        eval = maapi_exists(sock, tid, path) == 1;
        printf("%d\n", eval);
    }
    else if (op == M_DELETE) {
        int eval;
        eval = maapi_delete(sock, tid, path);
        printf("%d\n", eval);
        eval = maapi_apply_trans(sock, tid, 0);
        printf("%d\n", eval);
    }
    return 0;
}



