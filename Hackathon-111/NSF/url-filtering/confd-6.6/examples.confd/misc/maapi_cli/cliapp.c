
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

/* include generated ns file */
#include "scli.h"
#define OK(E) assert((E) == CONFD_OK)


/* global variables */
static int sock, tid;
static char *user = "admin";
static char *groups[] = {"admin"};
static int debuglevel = CONFD_DEBUG;
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
    OK(maapi_set_namespace(sock, tid, scli__ns));
    return CONFD_OK;
}


void reset()
{
    int s = system("./reset.sh");
    if (s != 0) {
        printf("failed to reset config \n");
        exit(1);
    }
    printf("Reset ok \n");
}

#define DELIM " \n\r"

static void runcli()
{
    char ibuf[BUFSIZ];
    struct maapi_cursor mc;
    int ival;
    char *tok;

    printf("#maapi-demo-cli "); fflush(stdout);

    while(fgets(ibuf, BUFSIZ, stdin) != NULL) {
        if ((tok = strtok(ibuf, DELIM)) == NULL) {
            printf("#maapi-demo-cli "); fflush(stdout);
            continue;
        }

        if (strncmp(tok, "show", 4) == 0) {
            if (maapi_get_str_elem(sock, tid, ibuf, BUFSIZ, "/root/foo") ==CONFD_OK)
                printf ("foo: %s\n", ibuf);
            else if (confd_errno == CONFD_ERR_NOEXISTS)
                printf ("foo: \n");
            else
                confd_fatal("What \n");

            if (maapi_get_int32_elem(sock, tid, &ival, "/root/bar") == CONFD_OK)
                printf ("bar: %d\n", ival);
            else if (confd_errno == CONFD_ERR_NOEXISTS)
                printf ("bar: \n");
            else
                confd_fatal("Whaat\n");

            maapi_init_cursor(sock, tid, &mc, "/root/servers/server");
            OK(maapi_get_next(&mc));
            while (mc.n != 0) {
                struct in_addr ip;
                uint16_t port;
                char tmpbuf[BUFSIZ];

                OK(maapi_get_ipv4_elem(sock, tid, &ip,
                                       "/root/servers/server{%x}/ip",
                                       &mc.keys[0]));
                OK(maapi_get_u_int16_elem(sock, tid, &port,
                                          "/root/servers/server{%x}/port",
                                          &mc.keys[0]));
                confd_pp_value(tmpbuf,BUFSIZ,&mc.keys[0]);
                printf ("server name=%s ip=%s port=%d\n",
                        tmpbuf, inet_ntoa(ip), port);
                OK(maapi_get_next(&mc));
            }
            maapi_destroy_cursor(&mc);
        }
        else if (strncmp(tok, "abort", 5) == 0) {
            OK(maapi_finish_trans(sock, tid));
            if ((tid = maapi_start_trans(sock, dbname, CONFD_READ_WRITE)) < 0)
                confd_fatal("failed to start trans \n");
            OK(maapi_set_namespace(sock, tid, scli__ns));
        }
        else if (strncmp(tok, "commit", 6) == 0) {
            OK(maapi_apply_trans(sock, tid, 0));
            OK(maapi_finish_trans(sock, tid));
            if ((tid = maapi_start_trans(sock, dbname, CONFD_READ_WRITE)) < 0)
                confd_fatal("failed to start trans \n");
            OK(maapi_set_namespace(sock, tid, scli__ns));
        }
        else if (strncmp(tok, "create", 6) == 0) {
            char *name; char *ipstr; char *portstr;
            if (((name = strtok(NULL, DELIM)) == NULL) ||
                ((ipstr = strtok(NULL, DELIM)) == NULL) ||
                ((portstr = strtok(NULL, DELIM)) == NULL)) {
                printf ("input error \n"); goto err;
            }
            if (maapi_create(sock, tid, "/root/servers/server{%s}", name) !=
                CONFD_OK) {
                printf ("error: %d %s \n ", confd_errno,confd_lasterr());
                goto err;
            }
            if (maapi_set_elem2(sock,tid,ipstr,"/root/servers/server{%s}/ip",name)
                != CONFD_OK) {
                printf ("error: %d %s \n ", confd_errno, confd_lasterr());
                goto err;
            }
            if (maapi_set_elem2(sock,tid,portstr,"/root/servers/server{%s}/port",
                                name)
                != CONFD_OK) {
                printf ("error: %d %s \n ", confd_errno, confd_lasterr());
                goto err;
            }
        }
        else if (strncmp(tok, "delete-config", 13) == 0) {
            if (maapi_delete_config(sock, dbname) != CONFD_OK)
                printf ("error: %d, %s\n", confd_errno,
                        confd_lasterr());
        }

        else if (strncmp(tok, "delete", 5) == 0) {
            char *name;
            if ((name= strtok(NULL, DELIM)) == NULL) {
                printf ("input error"); goto err;
            }
            if (maapi_delete(sock, tid, "/root/servers/server{%s}", name)
                != CONFD_OK) {
                printf ("error: %s \n ", confd_lasterr());
                goto err;
            }
        }
        else if (strncmp(tok, "candidate-reset", 15) == 0) {
            if (maapi_candidate_reset(sock) != CONFD_OK)
                printf ("error: %d, %s\n", confd_errno,
                        confd_lasterr());
        }

        else if (strncmp(tok, "validate-trans", 14) == 0) {
            if (maapi_validate_trans(sock, tid,1,1) == CONFD_OK) {
                printf ("ok \n");
            }
            else {
                printf ("nok: %s\n", confd_lasterr());
            }
        }
        else if (strcmp(tok, "candidate-confirmed-commit") == 0) {
            char *istr = strtok(NULL, DELIM);
            if (!istr) {printf("input error\n"); goto err;}
            if (maapi_candidate_confirmed_commit(sock, atoi(istr))!=CONFD_OK) {
                printf("error: %s\n", confd_lasterr());
            }
            else {
                printf("ok\n");
            }
        }
        else if (strcmp(tok, "candidate-commit") == 0) {
            if (maapi_candidate_commit(sock) !=CONFD_OK) {
                printf("error: %s\n", confd_lasterr());
            }
            else {
                printf("ok\n");
            }
        }
        else {
            printf("commands \n");
            printf("   show    - show current conf\n");
            printf("   help    - show this text\n");
            printf("   abort   - abort current trans \n");
            printf("   commit  - commit current trans\n");
            printf("   create name ip port  - create new server\n");
            printf("   delete name          - delete server\n");
            printf("   candidate-reset      - copy running into cand\n");
            printf("   validate-trans       - trans validate\n");
            printf("   delete-config        - delete config\n");
            printf("   candidate-commit     - copy cand to running | confirm\n");
            printf("   candidate-confirmed-commit secs \n");

            printf(" \n");
        }
          err:
        printf("#maapi-demo-cli "); fflush(stdout);
    }
}


int main(int argc, char **argv)
{
    int c;

    while ((c = getopt(argc, argv, "tdpsrc")) != -1) {
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
        case 'p':
            debuglevel = CONFD_PROTO_TRACE;
            break;
        case 's':
            debuglevel = CONFD_SILENT;
            break;
        case 'c':
            dbname = CONFD_CANDIDATE;
            break;

        }
    }
    printf ("dbname =%d\n", dbname);

    confd_init("cliapp", stderr, debuglevel);
    OK(cnct());
    runcli();
    return 0;
}



