/*********************************************************************
 * ConfD threading intro example
 * Implements a data provider for operational data and action.
 *
 * (C) 2011 Tail-f Systems
 * Permission to use this code as a starting point hereby granted
 *
 * See the README file for more information
 ********************************************************************/

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/poll.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>
#include <time.h>
#include <pthread.h>
#include <signal.h>

#include <confd_lib.h>
#include <confd_dp.h>
#include <confd_maapi.h>
#include "model.h"

/********************************************************************/

/*
  This should be based on /confdConfig/sessionLimits/maxSessions
  in confd.conf
*/
#define MAX_SESSIONS 10

/*
  Max # of times we try to create_daemon() (i.e. connect to ConfD)
  - could be unlimited in production code, but we don't want this
  example code to keep running forever if we forget to 'make stop'
*/
#define MAX_ATTEMPTS 5

/* Per-thread data */
struct thread {
    struct confd_daemon_ctx *dctx;
    int sock;
    pthread_t thread;
    int state;
    struct confd_user_info *uinfo;   /* only for action workers */
};
#define STATE_NONE   0
#define STATE_ACTIVE 1
#define STATE_IDLE   2

/* The data for the daemon, managed via dctx->d_opaque */
struct daemon_data {
    struct sockaddr_in addr;
    struct confd_daemon_ctx *dctx;
    struct thread control;
    struct thread data_worker;
    struct thread action_worker[MAX_SESSIONS];
};

/* Dummy process info */
struct proc_info {
    pid_t pid;
    int cpu;
};
static struct proc_info proc[] = {
    {1, 37}, {5, 19}, {17, 42}
};
static int nprocs = sizeof(proc) / sizeof(proc[0]);

/********************************************************************/

/* The basic poll loop, run by all threads */
static void poll_loop(struct confd_daemon_ctx *dctx, int sock)
{
    struct pollfd set[1];
    int ret = CONFD_OK;

    while (ret == CONFD_OK) {
        set[0].fd = sock;
        set[0].events = POLLIN;
        set[0].revents = 0;

        if (poll(&set[0], 1, -1) < 0) {
            perror("Poll failed:");
            continue;
        }

        /* Check for I/O */
        if (set[0].revents & POLLIN) {
            if ((ret = confd_fd_ready(dctx, sock)) == CONFD_EOF) {
                fprintf(stderr, "Socket %d closed\n", sock);
            } else if (ret == CONFD_ERR && confd_errno != CONFD_ERR_EXTERNAL) {
                fprintf(stderr, "Error on request for socket %d: %s (%d): %s\n",
                        sock, confd_strerror(confd_errno),
                        confd_errno, confd_lasterr());
            } else {
                ret = CONFD_OK;
            }
        }
    }
}

/* pthread start routine */
static void *run_thread(void *arg)
{
    struct thread *thr = arg;

    poll_loop(thr->dctx, thr->sock);
    return NULL;
}

/* Create a worker socket/thread */
static int create_worker(struct daemon_data *dd,
                         struct thread *worker)
{
    if ((worker->sock = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
        fprintf(stderr, "Failed to open workersocket\n");
        return CONFD_ERR;
    }
    if (confd_connect(dd->dctx, worker->sock, WORKER_SOCKET,
                      (struct sockaddr*)&dd->addr,
                      sizeof (struct sockaddr_in)) < 0) {
        fprintf(stderr, "Failed to confd_connect() to confd\n");
        return CONFD_ERR;
    }
    worker->dctx = dd->dctx;
    worker->state = STATE_IDLE;
    worker->uinfo = NULL;
    if (pthread_create(&worker->thread, NULL, run_thread, worker) != 0) {
        fprintf(stderr, "Failed to create worker thread\n");
        close(worker->sock);
        return CONFD_ERR;
    }
    return CONFD_OK;
}

/********************************************************************/

static int init_trans(struct confd_trans_ctx *tctx)
{
    struct daemon_data *dd = tctx->dx->d_opaque;

    /* Always dispatch to our single data worker */
    confd_trans_set_fd(tctx, dd->data_worker.sock);
    return CONFD_OK;
}

static int finish_trans(struct confd_trans_ctx *tctx)
{
    return CONFD_OK;
}

/********************************************************************/

static int get_next(struct confd_trans_ctx *tctx,
                         confd_hkeypath_t *keypath,
                         long next)
{
    int ix;
    confd_value_t v;

    if (next == -1) {  /* first call */
        ix = 0;
    } else {
        ix = next;
    }
    if (ix < nprocs) {
        CONFD_SET_UINT32(&v, proc[ix].pid);
        confd_data_reply_next_key(tctx, &v, 1, ix + 1);
    } else {           /* end of list */
        confd_data_reply_next_key(tctx, NULL, -1, -1);
    }
    return CONFD_OK;
}

static int get_elem(struct confd_trans_ctx *tctx,
                    confd_hkeypath_t *keypath)
{
    pid_t pid = CONFD_GET_UINT32(&keypath->v[1][0]); /* the key */
    int ix;
    confd_value_t v;

    for (ix = 0; ix < nprocs; ix++) {
        if (proc[ix].pid == pid) {
            switch (CONFD_GET_XMLTAG(&(keypath->v[0][0]))) {
            case model_pid:
                CONFD_SET_UINT32(&v, proc[ix].pid);
                break;
            case model_cpu:
                CONFD_SET_UINT32(&v, proc[ix].cpu);
                break;
            default:
                return CONFD_ERR;
            }
            confd_data_reply_value(tctx, &v);
            return CONFD_OK;
        }
    }
    confd_data_reply_not_found(tctx);
    return CONFD_OK;
}

/********************************************************************/

static int init_action(struct confd_user_info *uinfo)
{
    struct confd_daemon_ctx *dctx = uinfo->actx.dx;
    struct daemon_data *dd = dctx->d_opaque;
    struct thread *worker;
    int i;

    /* record the time if this is the 'sleep' action */
    if (uinfo->actx.cb_opaque != NULL) {
        uinfo->actx.t_opaque = malloc(sizeof(time_t));
        time(uinfo->actx.t_opaque);
    }

    /* Find/create the action worker thread for this user session */
    /* We use unifo->u_opaque to keep track of the worker */
    if (uinfo->u_opaque != NULL) {
        worker = uinfo->u_opaque;
    } else {
        for (i = 0; i < MAX_SESSIONS; i++) {
            worker = &dd->action_worker[i];
            if (worker->state == STATE_IDLE || worker->state == STATE_NONE) {
                break;
            }
        }
        if (i >= MAX_SESSIONS ||    /* "shouldn't happen" */
            (worker->state == STATE_NONE && /* need to create new worker */
             create_worker(dd, worker) != CONFD_OK)) {
            confd_action_seterr_extended(uinfo, CONFD_ERRCODE_RESOURCE_DENIED,
                                         0, 0, "Couldn't start action");
            return CONFD_ERR;
        }
        worker->uinfo = uinfo;
        uinfo->u_opaque = worker;
    }
    worker->state = STATE_ACTIVE;
    confd_action_set_fd(uinfo, worker->sock);
    return CONFD_OK;
}

/********************************************************************/

static int do_sleep(struct confd_user_info *uinfo,
                    struct xml_tag *name,
                    confd_hkeypath_t *kp,
                    confd_tag_value_t *params,
                    int n)
{
    confd_tag_value_t reply[1];
    time_t *startp = uinfo->actx.t_opaque;
    time_t start, stop;
    int sleeptime = CONFD_GET_UINT32(CONFD_GET_TAG_VALUE(&params[0]));

    start = *startp;
    free(uinfo->actx.t_opaque);
    confd_action_set_timeout(uinfo, sleeptime + 3); /* avoid timeout */
    sleep(sleeptime);
    time(&stop);
    CONFD_SET_TAG_UINT32(&reply[0], model_slept, stop - start);
    confd_action_reply_values(uinfo, reply, 1);
    return CONFD_OK;
}

static int totals(struct confd_user_info *uinfo,
                  struct xml_tag *name,
                  confd_hkeypath_t *kp,
                  confd_tag_value_t *params,
                  int n)
{
    struct confd_daemon_ctx *dctx = uinfo->actx.dx;
    struct daemon_data *dd = dctx->d_opaque;
    confd_tag_value_t reply[2];
    int sock, th;
    char *path = "/model:dm/proc";
    struct maapi_cursor mc;
    confd_value_t v;
    int nprocs = 0, tcpu = 0;
    int ret = CONFD_ERR;

    if ((sock = socket(PF_INET, SOCK_STREAM, 0)) <= 0) {
        fprintf(stderr, "Failed to open maapisocket\n");
    } else {
        if (maapi_connect(sock, (struct sockaddr*)&dd->addr,
                          sizeof (struct sockaddr_in)) != CONFD_OK) {
            fprintf(stderr, "Failed to maapi_connect() to confd\n");
        } else if ((th = maapi_start_trans2(sock, CONFD_RUNNING,
                                            CONFD_READ, uinfo->usid)) < 0) {
            fprintf(stderr, "Failed to start transaction\n");
        } else {
            ret = CONFD_OK;
        }
        if (ret != CONFD_OK) {
            close(sock);
        }
    }

    if (ret == CONFD_OK) {
        if ((ret = maapi_init_cursor(sock, th, &mc, "%s", path)) == CONFD_OK) {
            ret = maapi_get_next(&mc);
        }
        while (ret == CONFD_OK && mc.n != 0) {
            nprocs++;
            ret = maapi_get_elem(sock, th, &v, "%s{%x}/cpu", path, &mc.keys[0]);
            if (ret == CONFD_OK) {
                tcpu += CONFD_GET_UINT32(&v);
                ret = maapi_get_next(&mc);
            }
        }
        close(sock);
    }

    if (ret == CONFD_OK) {
        CONFD_SET_TAG_UINT32(&reply[0], model_num_procs, nprocs);
        CONFD_SET_TAG_UINT32(&reply[1], model_total_cpu, tcpu);
        confd_action_reply_values(uinfo, reply, 2);
        return CONFD_OK;
    } else {
        confd_action_seterr(uinfo, "Failed to calculate total");
        return CONFD_ERR;
    }
}

static int abort_sleep(struct confd_user_info *uinfo)
{
    struct thread *worker = uinfo->u_opaque;

    /* send SIGUSR1 to the worker thread - with the catch_usr1()
       handler installed, this will just interrupt a blocking
       system call, making the thread return from the callback  */
    if (worker != NULL) {
        pthread_kill(worker->thread, SIGUSR1);
    }
    return CONFD_OK;
}

/* no-op signal handler to let SIGUSR1 interrupt
   a system call without terminating the process */
static void catch_usr1(int sig)
{
}

/********************************************************************/

/* Make action worker thread available when user session ends */
void usess_stop(struct confd_daemon_ctx *dctx,
                struct confd_user_info *uinfo)
{
    struct thread *worker = uinfo->u_opaque;

    /* Make the thread for this user session (if any) available
       - the callback is invoked on the control socket, so
       modification is safe */
    if (worker != NULL) {
        worker->state = STATE_IDLE;
        worker->uinfo = NULL;
        uinfo->u_opaque = NULL;
    }
}

/********************************************************************/

static int create_daemon(struct daemon_data *dd)
{
    struct confd_trans_cbs trans;
    struct confd_data_cbs data;
    struct confd_action_cbs action;
    struct confd_usess_cbs usess;
    struct thread *control, *worker;
    int i;

    if ((dd->dctx = confd_init_daemon("thr_daemon")) == NULL) {
        fprintf(stderr, "Failed to init daemon\n");
        return CONFD_ERR;
    }
    dd->dctx->d_opaque = dd;
    control = &dd->control;
    if ((control->sock = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
        fprintf(stderr, "Failed to open ctlsocket\n");
        goto error_ctx;
    }
    if (confd_connect(dd->dctx, control->sock, CONTROL_SOCKET,
                      (struct sockaddr*)&dd->addr,
                      sizeof (struct sockaddr_in)) < 0) {
        fprintf(stderr, "Failed to confd_connect() to confd\n");
        goto error_ctlsock;
    }

    /* register transaction callback */
    memset(&trans, 0, sizeof (trans));
    trans.init = init_trans;
    trans.finish = finish_trans;
    if (confd_register_trans_cb(dd->dctx, &trans) == CONFD_ERR) {
        fprintf(stderr, "Failed to register trans cb\n");
        goto error_ctlsock;
    }

    /* register data callbacks */
    memset(&data, 0, sizeof (data));
    data.get_elem = get_elem;
    data.get_next = get_next;
    strcpy(data.callpoint, model__callpointid_proc);
    if (confd_register_data_cb(dd->dctx, &data) == CONFD_ERR) {
        fprintf(stderr, "Failed to register data cb\n");
        goto error_ctlsock;
    }

    /* register the 'sleep' action callbacks */
    memset(&action, 0, sizeof (action));
    action.init = init_action;
    action.abort = abort_sleep;
    action.action = do_sleep;
    action.cb_opaque = do_sleep;
    strcpy(action.actionpoint, model__actionpointid_sleep);
    if (confd_register_action_cbs(dd->dctx, &action) == CONFD_ERR) {
        fprintf(stderr, "Failed to register sleep action cb\n");
        goto error_ctlsock;
    }

    /* register the 'totals' action callbacks */
    memset(&action, 0, sizeof (action));
    action.init = init_action;
    action.action = totals;
    strcpy(action.actionpoint, model__actionpointid_totals);
    if (confd_register_action_cbs(dd->dctx, &action) == CONFD_ERR) {
        fprintf(stderr, "Failed to register totals action cb\n");
        goto error_ctlsock;
    }

    /* register usess callback */
    memset(&usess, 0, sizeof (usess));
    usess.stop = usess_stop;
    if (confd_register_usess_cb(dd->dctx, &usess) == CONFD_ERR) {
        fprintf(stderr, "Failed to register usess cb\n");
        goto error_ctlsock;
    }

    if (confd_register_done(dd->dctx) != CONFD_OK) {
        fprintf(stderr, "Failed to complete registration\n");
        goto error_ctlsock;
    }

    /* create only the data worker */
    worker = &dd->data_worker;
    if (create_worker(dd, worker) != CONFD_OK) {
        fprintf(stderr, "Failed to create data worker thread\n");
        goto error_ctlsock;
    }
    worker->state = STATE_ACTIVE;
    for (i = 0; i < MAX_SESSIONS; i++) {
        dd->action_worker[i].state = STATE_NONE;
    }

    /* finally create the control thread */
    control->dctx = dd->dctx;
    if (pthread_create(&control->thread, NULL, run_thread, control) != 0) {
        fprintf(stderr, "Failed to create control thread\n");
        goto error_worker;
    }
    control->state = STATE_ACTIVE;

    return CONFD_OK;

  error_worker:
    pthread_cancel(worker->thread);
    pthread_join(worker->thread, NULL);
    close(worker->sock);
  error_ctlsock:
    close(control->sock);
  error_ctx:
    confd_release_daemon(dd->dctx);
    return CONFD_ERR;
}

static void destroy_daemon(struct daemon_data *dd)
{
    struct thread *worker;
    int i;

    /* kill all threads and close all sockets */
    if (dd->control.state != STATE_NONE) {
        pthread_cancel(dd->control.thread);
        pthread_join(dd->control.thread, NULL);
    }
    close(dd->control.sock);
    worker = &dd->data_worker;
    pthread_cancel(worker->thread);
    pthread_join(worker->thread, NULL);
    close(worker->sock);
    for (i = 0; i < MAX_SESSIONS; i++) {
        worker = &dd->action_worker[i];
        if (worker->state != STATE_NONE) {
            pthread_cancel(worker->thread);
            pthread_join(worker->thread, NULL);
            close(worker->sock);
            if (worker->uinfo != NULL) {
                worker->uinfo->u_opaque = NULL;
                worker->uinfo = NULL;
            }
        }
    }

    /* release all library memory */
    confd_release_daemon(dd->dctx);
}

/********************************************************************/

int main(int argc, char **argv)
{
    int debuglevel = CONFD_TRACE;
    int c;
    struct daemon_data dd;

    while ((c = getopt(argc, argv, "qdt")) != -1) {
        switch (c) {
        case 'q':
            debuglevel = CONFD_SILENT;
            break;
        case 'd':
            debuglevel = CONFD_DEBUG;
            break;
        case 't':
            debuglevel = CONFD_TRACE;
            break;
        default:
            fprintf(stderr, "usage: %s [-qdt]\n", argv[0]);
            exit(1);
        }
    }

    /* initialize confd library */
    confd_init("thr_daemon", stderr, debuglevel);

    dd.addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    dd.addr.sin_family = AF_INET;
    dd.addr.sin_port = htons(CONFD_PORT);

    /* This could alternatively be done as part of create_daemon()
       if we need to pick up schema changes in ConfD restart */
    if (confd_load_schemas((struct sockaddr*)&dd.addr,
                           sizeof (struct sockaddr_in)) != CONFD_OK) {
        fprintf(stderr, "Failed to load schemas from confd\n");
        exit(1);
    }

    /* setup signal handler for abort_sleep() */
    signal(SIGUSR1, catch_usr1);

    while(1) {
        int attempts = 0;

        /* create the daemon */
        while (create_daemon(&dd) != CONFD_OK) {
            if (++attempts >= MAX_ATTEMPTS) {
                fprintf(stderr, "Failed to create daemon, giving up\n");
                exit(2);
            } else {
                fprintf(stderr, "Failed to create daemon, will retry...\n\n");
                sleep(3);
            }
        }

        /* wait for control thread to exit */
        fprintf(stdout, "Running daemon\n");
        pthread_join(dd.control.thread, NULL);
        fprintf(stderr, "Daemon terminated, restarting\n");
        dd.control.state = STATE_NONE;

        /* cleanup */
        destroy_daemon(&dd);

    }

    return 0;
}

/********************************************************************/
