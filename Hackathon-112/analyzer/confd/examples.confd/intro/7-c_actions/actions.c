/*********************************************************************
 * ConfD Actions intro example
 * Implements a couple of actions
 *
 * (C) 2007 Tail-f Systems
 * Permission to use this code as a starting point hereby granted
 *
 * See the README file for more information
 ********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <dirent.h>
#include <fcntl.h>
#include <errno.h>
#include <syslog.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <sys/poll.h>

#include <confd_lib.h>
#include <confd_dp.h>

#include "config.h"

/********************************************************************/

static int ctlsock, workersock;
static struct confd_daemon_ctx *dctx;

static int init_action(struct confd_user_info *uinfo);
static int do_action(struct confd_user_info *uinfo,
                     struct xml_tag *name,
                     confd_hkeypath_t *kp,
                     confd_tag_value_t *params,
                     int n);
static int abort_action(struct confd_user_info *uinfo);

static void main_loop(int do_phase0);

extern void fail(char *fmt, ...);

/********************************************************************/

int main(int argc, char **argv)
{
    struct sockaddr_in addr;
    int debuglevel = CONFD_TRACE;
    struct confd_action_cbs acb;

    /* Init library */
    confd_init("actions_daemon",stderr, debuglevel);

    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(CONFD_PORT);

    if (confd_load_schemas((struct sockaddr*)&addr,
                           sizeof (struct sockaddr_in)) != CONFD_OK)
        confd_fatal("Failed to load schemas from confd\n");
    if ((dctx = confd_init_daemon("actions_daemon")) == NULL)
        fail("Failed to initialize ConfD\n");

    if ((ctlsock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        confd_fatal("Failed to open ctlsocket\n");

    /* Create the first control socket, all requests to */
    /* create new transactions arrive here */

    if (confd_connect(dctx, ctlsock, CONTROL_SOCKET, (struct sockaddr*)&addr,
                      sizeof (struct sockaddr_in)) < 0)
        confd_fatal("Failed to confd_connect() to confd \n");


    /* Also establish a workersocket, this is the most simple */
    /* case where we have just one ctlsock and one workersock */

    if ((workersock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        confd_fatal("Failed to open workersocket\n");
    if (confd_connect(dctx, workersock, WORKER_SOCKET,(struct sockaddr*)&addr,
                      sizeof (struct sockaddr_in)) < 0)
        confd_fatal("Failed to confd_connect() to confd \n");

    /* register the action handler callback */
    memset(&acb, 0, sizeof(acb));
    strcpy(acb.actionpoint, "reboot-point");
    acb.init = init_action;
    acb.action = do_action;
    acb.abort = abort_action;

    if (confd_register_action_cbs(dctx, &acb) != CONFD_OK)
        fail("Couldn't register action callbacks");

    if (confd_register_done(dctx) != CONFD_OK)
        fail("Couldn't complete callback registration");

    main_loop(0);

    close(ctlsock);
    close(workersock);
    confd_release_daemon(dctx);
    return 0;
}

/* Main loop - receive and act on events from ConfD */
static void main_loop(int do_phase0)
{
    struct pollfd set[3];
    int ret;

    while (1) {

        set[0].fd = ctlsock;
        set[0].events = POLLIN;
        set[0].revents = 0;

        set[1].fd = workersock;
        set[1].events = POLLIN;
        set[1].revents = 0;

        if (poll(set, 2, -1) < 0) {
            fail("Poll failed");
        }

        /* Check for I/O */

        if (set[0].revents & POLLIN) { /* ctlsock */
            if ((ret = confd_fd_ready(dctx, ctlsock)) == CONFD_EOF) {
                fail("Control socket closed");
            } else if (ret == CONFD_ERR && confd_errno != CONFD_ERR_EXTERNAL) {
                fail("Error on control socket request: %s (%d): %s",
                     confd_strerror(confd_errno), confd_errno, confd_lasterr());
            }
        }

        if (set[1].revents & POLLIN) { /* workersock */
            if ((ret = confd_fd_ready(dctx, workersock)) == CONFD_EOF) {
                fail("Worker socket closed");
            } else if (ret == CONFD_ERR && confd_errno != CONFD_ERR_EXTERNAL) {
                fail("Error on worker socket request: %s (%d): %s",
                     confd_strerror(confd_errno), confd_errno, confd_lasterr());
            }
        }

    }
}

/********************************************************************/

static int init_action(struct confd_user_info *uinfo)
{
    int ret = CONFD_OK;

    printf("init_action called\n");
    confd_action_set_fd(uinfo, workersock);
    return ret;
}

static int abort_action(struct confd_user_info *uinfo) {
    fprintf(stderr, "Aborting outstanding action\n");
    /* We need to clean  up the worker socket by replying */
    confd_action_delayed_reply_error(uinfo, "aborted");
    return CONFD_OK;
}



/* This is the action callback function.  In this example, we have a
   single function for all four actions. */
static int do_action(struct confd_user_info *uinfo,
                     struct xml_tag *name,
                     confd_hkeypath_t *kp,
                     confd_tag_value_t *params,
                     int n)
{
    confd_tag_value_t reply[3];
    int i, k;
    char buf[BUFSIZ];
    char *p;

    printf("do_action called\n");

    for (i = 0; i < n; i++) {
        confd_pp_value(buf, sizeof(buf), CONFD_GET_TAG_VALUE(&params[i]));
        printf("param %2d: %9u:%-9u, %s\n", i, CONFD_GET_TAG_NS(&params[i]),
               CONFD_GET_TAG_TAG(&params[i]), buf);
    }

    switch (name->tag) {
    case config_reboot:
        printf("reboot\n");
        /* no params */
        break;
    case config_restart:
        printf("restart\n");
        /* params[0] is mode */
        p = CONFD_GET_CBUFPTR(CONFD_GET_TAG_VALUE(&params[0]));
        i = CONFD_GET_BUFSIZE(CONFD_GET_TAG_VALUE(&params[0]));
        strncpy(buf, p, i);
        /* if we get mode == error1, we reply with generic error */
        if (strcmp(buf, "error1") == 0) {
            return CONFD_ERR;
        }
        /* if we get mode == error2, we reply with specific error */
        if (strcmp(buf, "error2") == 0) {
            confd_action_seterr(uinfo, "myfail");
            return CONFD_ERR;
        }
        /* otherwise, we create a result string with mode-result-... */
        buf[i] = 0;
        strcat(buf, "-result");
        k = 1;
        while (k < n) {
            printf("k: %d, tag: %d\n", k, CONFD_GET_TAG_TAG(&params[k]));
            switch CONFD_GET_TAG_TAG(&params[k]) {
            case config_debug:
                strcat(buf, "-debug");
                break;
            case config_foo:
                strcat(buf, "-foo");
                if (k+1 < n) {
                    if (CONFD_GET_TAG_TAG(&params[k+1]) == config_debug)
                        strcat(buf, "-debug");
                    k++;
                }
                break;
            }
            k++;
        }
        i = 0;
        CONFD_SET_TAG_STR(&reply[i], config_time, buf); i++;
        confd_action_reply_values(uinfo, reply, i);
        break;
    case config_reset:
        printf("reset\n");
        p = CONFD_GET_CBUFPTR(CONFD_GET_TAG_VALUE(&params[0]));
        i = CONFD_GET_BUFSIZE(CONFD_GET_TAG_VALUE(&params[0]));
        strncpy(buf, p, i);
        buf[i] = 0;
        strcat(buf, "-result");
        i = 0;
        CONFD_SET_TAG_STR(&reply[i], config_time, buf); i++;
        confd_action_reply_values(uinfo, reply, i);
        break;
    case config_abort_test:
        return CONFD_DELAYED_RESPONSE;

    default:
        /* this happens only if we forget to update this code when the
           data model has changed. */
        printf("got bad operation\n");
        return CONFD_ERR;
    }

    return CONFD_OK;
}

void fail(char *fmt, ...)
{
    va_list ap;
    char buf[BUFSIZ];

    va_start(ap, fmt);
    snprintf(buf, sizeof(buf), "%s, exiting", fmt);
    vsyslog(LOG_ERR, buf, ap);
    va_end(ap);
    exit(1);
}

/********************************************************************/
