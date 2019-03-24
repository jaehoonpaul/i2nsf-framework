/*
 * Copyright 2014 Tail-F Systems AB
 * Tail-F customers are permitted to redistribute in binary form, with
 * or without modification, for use in customer products.
 */

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <unistd.h>

#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/param.h>
#include <sys/poll.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <confd.h>
#include <confd_maapi.h>
#include <confd_cdb.h>
#include <confd_logsyms.h>

#include "linuxcfg_api.h"
#include "linuxcfg_util.h"

#define LOG_FACILITY LOG_DAEMON
#define MAX_SUBSCRIPTIONS 200

extern struct component *components[];
extern int num_components;

static int ctlsock, workersock, maapisock;
static int notifsock, subsock, readsock;
static struct confd_daemon_ctx *dctx;
int linuxcfg_change_is_trigger;

struct subscription {
    struct subscription *next;
    int spoint;
    void (*cb)(int rsock, int ssock, int spoint);
};
static struct subscription *subscriptions;

static int get_ctlsock(struct addrinfo *addr);
static int get_workersock(struct addrinfo *addr);
static int get_maapisock(struct addrinfo *addr);
static int get_readsock(struct addrinfo *addr);
static int get_notifsock(struct addrinfo *addr);
static int get_subsock(struct addrinfo *addr);

static void components_phase0(struct confd_trans_ctx *utctx);

static int init_validation(struct confd_trans_ctx *tctx);
static int stop_validation(struct confd_trans_ctx *tctx);

static int init_data(struct confd_trans_ctx *tctx);
static int finish_data(struct confd_trans_ctx *tctx);

static void register_valpoints(struct confd_valpoint_cb *valpoints);
static void register_datapoints(struct confd_data_cbs *datapoints);
static void register_actionpoints(struct confd_action_cbs *actionpoints);

static int start_read_session(int sock);
static void call_subscriber(int spoint, int ssock, int rsock);
static void clear_subscriptions(void);
static int ensure_death(pid_t *pid, int n);
static int killpids(pid_t *pid, int n, int signal);

static int main_loop(int do_phase0);

int linuxcfg_loglevel = 0; // shared between components

static char *root = "/"; /*"/sys";*/
static int do_daemon = 1;
static int trace_daemon = 0;
static int debuglevel = CONFD_SILENT;
static int dummy_cmd_run = 0; /* if true, do not run system cmd, only print it*/
static int do_phase0 = 0;
static int do_init = 0;
static char confd_port[16];
static struct addrinfo *addr = NULL;
static struct addrinfo hints;

struct custom_poll_fd {
    int fd;
    int events;
    void (*handler)(int fd, int revents);
};

static int num_fds = 0;
static struct custom_poll_fd *custom_fds = NULL;

#define INFO_LOG_STDERR (1<<0)
#define INFO_LOG_SYSLOG (1<<1)

int parse_arguments(int argc, char **argv)
{
    int ret = CONFD_OK;

    char *p;
    int c;

    while ((c = getopt(argc, argv, "Dlsdtpmiur:c:")) != -1) {
        switch (c) {
        case 'D':
            do_daemon = 0;
            break;
        case 'd':
            debuglevel = CONFD_DEBUG;
            trace_daemon = 1;
            break;
        case 't':
            debuglevel = CONFD_TRACE;
            trace_daemon = 1;
            break;
        case 'l':
            linuxcfg_loglevel |= INFO_LOG_STDERR;
            trace_daemon = 1;
            break;
        case 's':
            linuxcfg_loglevel |= INFO_LOG_SYSLOG;
            break;
        case 'p': /* undocumented */
            debuglevel = CONFD_PROTO_TRACE;
            trace_daemon = 1;
            break;
        case 'm': /* undocumented */
            dummy_cmd_run = 1;
            break;
        case 'i':
            do_phase0 = 1;
            do_init = 1;
            break;
        case 'u':
            do_phase0 = 1;
            do_init = 0;
            break;
        case 'r':
            root = optarg;
            break;
        case 'c':
            if ((p = strchr(optarg, '/')) != NULL)
                *p++ = '\0';
            else
                p = confd_port;
            if (getaddrinfo(optarg, p, &hints, &addr) != 0) {
                if (p != confd_port) {
                    *--p = '/';
                    p = "/port";
                } else {
                    p = "";
                }
                fprintf(stderr, "%s: Invalid address%s: %s\n",
                        argv[0], p, optarg);
                exit(1);
            }
            break;
        default:
            fprintf(stderr, "Usage: %s [-Ddtu] [-r root] "
                    "[-c address[/port]]\n",
                    argv[0]);
            ret = CONFD_OK;
            break;
        }
    }

    return ret;
}

void bind_msock_to_opaque(struct confd_trans_ctx *tctx, int *sock_ptr) {
    if (NULL == tctx->t_opaque) {
        size_t siz = sizeof(struct opaque_data);
        tctx->t_opaque = malloc(siz);
        memset(tctx->t_opaque, 0x00, siz);
    }
    struct opaque_data *data = (struct opaque_data *) tctx->t_opaque;
    data->msock = sock_ptr;
}

int main(int argc, char **argv)
{
    char *dname;
    int delay = 1;
    int i;
    struct confd_trans_validate_cbs vcb;
    struct confd_trans_cbs tcb;
    static struct confd_trans_ctx upgrade_tctx, *utctx;

    snprintf(confd_port, sizeof(confd_port), "%d", CONFD_PORT);

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = PF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    info("Starting linuxCfg.");
    if (CONFD_OK != parse_arguments(argc, argv)) {
        error("linuxcfg failed to parse arguments, exiting.");
        exit(1);
    }

    if (addr == NULL
        && (i = getaddrinfo("127.0.0.1", confd_port, &hints, &addr))
        != 0) {
        /* "Can't happen" */
        fail("%s: Failed to get address for ConfD: %s",
             argv[0],
             gai_strerror(i));
    }
    if ((dname = strrchr(argv[0], '/')) != NULL) {
        dname++;
    }else {
        dname = argv[0];
    }
    openlog(dname, LOG_PERROR, LOG_FACILITY);

    /* Init library */
    confd_init(dname, stderr, debuglevel);

    /* Pre-setup component initialization (optional) */
    for (i = 0; i < num_components; i++) {
        if (components[i]->init != NULL) {
            info("Initializing component %i ... ", i);
            (*components[i]->init)();
        } else {
            info("Component %i initialization skipped.", i);
        }
        info("Initializing component %i ... done", i);
    }

    info("Components initialized!");

    /* Outer - wait for connection to ConfD/CDB to succeed,
       do initial setup, reconnect if connection lost */
    while (1) {
        int connected = 0;

        if (dctx == NULL &&
            (dctx = confd_init_daemon(dname)) == NULL) {
            fail("Failed to initialize ConfD");
        }

        if ((ctlsock = get_ctlsock(addr)) < 0) {
            int nextdelay = 2 * delay;

            if (delay < 8) {
                warn("Failed to connect to ConfD, "
                     "retrying in %d seconds...",
                     delay);
            } else if (delay == 8) {
                warn("Failed to connect to ConfD, "
                     "will keep trying...");
            } else {
                nextdelay = delay;
            }
            sleep(delay);
            delay = nextdelay;
            continue;
        }

        if (delay > 1) {
            warn("Connected to ConfD");
            delay = 1;
        }

        do {
            if ((workersock = get_workersock(addr)) >= 0) {
                if ((maapisock = get_maapisock(addr)) >= 0) {
                    readsock = -1; /* no read socket when do_init */
                    if (do_init ||
                        (readsock = get_readsock(addr)) >= 0) {
                        if ((do_phase0 ?
                             (notifsock = get_notifsock(addr)) :
                             (subsock = get_subsock(addr))) >= 0) {
                            connected = 1;
                            break;
                        }
                        if (readsock != -1)
                            cdb_close(readsock);
                    }
                    maapi_close(maapisock);
                }
                close(workersock);
            }
            close(ctlsock);
        } while (0);

        if (connected) {

            if (maapi_load_schemas(maapisock) != CONFD_OK)
                fail("Failed to load schemas");

            /* Init crypto - may not be needed, so ignore errors */
            confd_install_crypto_keys(dctx);

            memset(&vcb, 0, sizeof(vcb));
            vcb.init = init_validation;
            vcb.stop = stop_validation;
            confd_register_trans_validate_cb(dctx, &vcb);

            memset(&tcb, 0, sizeof(tcb));
            tcb.init = init_data;
            tcb.finish = finish_data;
            confd_register_trans_cb(dctx, &tcb);

            if (do_phase0) {

                connected = 0;

                /* dummy tctx to provide common interface
                   for components */
                if (maapi_attach_init(maapisock,
                                      &upgrade_tctx.thandle)
                    == CONFD_OK) {

                    utctx = &upgrade_tctx;

                    bind_msock_to_opaque(utctx, &maapisock);
                } else {
                    if (!do_init &&
                        confd_errno == CONFD_ERR_NOEXISTS) {
                        /* This is OK in case of SW upgrade that
                           didn't change .fxs contents */
                        warn("No upgrade transaction found");
                        utctx = NULL;
                        cdb_close(readsock);
                        readsock = -1;
                    } else {
                        fail("Failed to attach to %s transaction",
                             do_init ? "init" : "upgrade");
                    }
                }

                if (readsock != -1 &&
                    start_read_session(readsock) != 0)
                    fail("Failed to start phase 0 read session");

                components_phase0(utctx);

                if (confd_register_done(dctx) != CONFD_OK)
                    fail("Failed to complete callback registration");

                if (readsock != -1) {
                    cdb_end_session(readsock);
                    cdb_close(readsock);
                }

                if (do_daemon) {
                    if (daemon(0, trace_daemon) != 0)
                        fail("Failed to change to daemon mode");
                    do_daemon = 0;
                }

                if (main_loop(1) == 0) {
                    do_phase0 = do_init = 0;
                    do {
                        if ((readsock = get_readsock(addr)) >= 0) {
                            if ((subsock = get_subsock(addr)) >= 0) {
                                connected = 1;
                                break;
                            }
                            cdb_close(readsock);
                        }
                    } while (0);
                }

                close(notifsock);

            } else { /* no phase 0 */
                components_phase0(NULL);

                if (confd_register_done(dctx) != CONFD_OK)
                    fail("Failed to complete callback registration");

            }

            if (connected) {

                /* Since the calls to setup functions are wrapped
                 * within a start_session() we have a read-lock on
                 * the configuration. I.e. we know that no
                 * modifications will be done until we call
                 * end_session()
                 */
                if (start_read_session(readsock) != 0)
                    fail("Failed to start read session");

                clear_subscriptions();

                //make deamon before setup, as setup can start pthread
                if (do_daemon) {
                    if (daemon(0, trace_daemon) != 0) {
                        fail("Failed to change to daemon mode");
                    }
                    do_daemon = 0;
                }

                for (i = 0; i < num_components; i++)
                    (*components[i]->setup)(dctx, readsock);

                if (subscriptions != NULL
                    && cdb_subscribe_done(subsock) != CONFD_OK)
                    fail("Failed to complete subscriptions");


                cdb_end_session(readsock);

                /* Trigger all subscriptions */
                trigger_subscriptions(NULL);

                (void) main_loop(0);

                cdb_close(readsock);
                cdb_close(subsock);
            }

            close(ctlsock);
            close(workersock);
            maapi_close(maapisock);
        }
        confd_release_daemon(dctx);
        dctx = NULL;
    }
}

void register_fd_handler(int fd, int events,
                         void (*handler)(int fd, int revents))
{
    num_fds = num_fds + 1;
    custom_fds = realloc(custom_fds,
                         num_fds * sizeof(struct custom_poll_fd));
    custom_fds[num_fds-1].fd = fd;
    custom_fds[num_fds-1].events = events;
    custom_fds[num_fds-1].handler = handler;

}

static struct pollfd *poll_set = NULL;

/* Main loop - receive and act on events from ConfD/CDB */
static int main_loop(int do_phase0)
{
    struct confd_notification notif;
    int pollsize;
    int i, ret;

    while (1) {
        pollsize = 3 + num_fds;

        XFREE(poll_set);
        poll_set = xmalloc(sizeof(struct pollfd) * pollsize);

        poll_set[0].fd = ctlsock;
        poll_set[0].events = POLLIN;
        poll_set[0].revents = 0;

        poll_set[1].fd = workersock;
        poll_set[1].events = POLLIN;
        poll_set[1].revents = 0;

        if (do_phase0) {
            poll_set[2].fd = notifsock;
            poll_set[2].events = POLLIN;
            poll_set[2].revents = 0;
        } else {
            poll_set[2].fd = subsock;
            poll_set[2].events = POLLIN;
            poll_set[2].revents = 0;
        }

        for (i=3; i < pollsize; i++) {
            poll_set[i].fd      = custom_fds[i-3].fd;
            poll_set[i].events  = custom_fds[i-3].events;
            poll_set[i].revents = 0;
        }

        if (poll(poll_set, pollsize, -1) < 0) {
            error("Poll failed: %s. Restarting.",
                  strerror(errno));
            return -1;
        }

        /* Check for I/O */

        if (poll_set[0].revents & POLLIN) { /* ctlsock */
            if ((ret = confd_fd_ready(dctx, ctlsock)) == CONFD_EOF) {
                error("Control socket closed - restarting");
                return -1;
            } else if (ret == CONFD_ERR &&
                       confd_errno != CONFD_ERR_EXTERNAL) {
                error("Error on control socket request - "
                      "restarting");
                return -1;
            }
        }

        if (poll_set[1].revents & POLLIN) { /* workersock */
            if ((ret = confd_fd_ready(dctx, workersock))
                == CONFD_EOF) {
                error("Worker socket closed - restarting");
                return -1;
            } else if (ret == CONFD_ERR && confd_errno
                       != CONFD_ERR_EXTERNAL) {
                error("Error on worker socket request - "
                      "restarting");
                return -1;
            }
        }

        if (!do_phase0 && (poll_set[2].revents & POLLIN)) { /* subsock */
            static int *spoint = NULL;
            int num_spoints;
            int flags;
            enum cdb_sub_notification type;

            XFREE(spoint);

            if (cdb_read_subscription_socket2(subsock,
                                              &type,
                                              &flags,
                                              &spoint,
                                              &num_spoints)
                != CONFD_OK) {
                error("Failed to read subscription - restarting");
                return -1;
            }

            if (flags & CDB_SUB_FLAG_TRIGGER) {
                linuxcfg_change_is_trigger = 1;
            } else {
                linuxcfg_change_is_trigger = 0;
            }

            if (num_spoints > 0) {

                if (start_read_session(readsock) != 0) {
                    error("Failed to start read session - "
                          "restarting");
                    return -1;
                }
                for (i = 0; i < num_spoints; i++)
                    call_subscriber(spoint[i], subsock, readsock);
                cdb_end_session(readsock);
                if (cdb_sync_subscription_socket(subsock,
                                                 CDB_DONE_PRIORITY)
                    != CONFD_OK) {
                    error("Failed to sync subscription socket - "
                          "restarting");
                    return -1;
                }
            }
        }

        if (do_phase0 && (poll_set[2].revents & POLLIN)) {
            /* notifsock */
            if (confd_read_notification(notifsock, &notif)
                != CONFD_OK) {
                error("Failed to read notification - restarting");
                return -1;
            }
            if (notif.type == CONFD_NOTIF_SYSLOG
                && notif.n.syslog.logno == CONFD_PHASE1_STARTED) {
                return 0;
            }
        }

        /* Check custom poll hooks */
        for(i=3; i < pollsize; i++) {
            if (poll_set[i].revents & custom_fds[i-3].events) {
                LOG("Custom trigger %d", i-3);
                custom_fds[i-3].handler(custom_fds[i-3].fd,
                                        poll_set[i].revents);
            }
        }
    }
}


static void components_phase0(struct confd_trans_ctx *utctx) {
    int i;

    for (i = 0; i < num_components; i++) {
        if (components[i]->setup0 != NULL)
            (*components[i]->setup0)(dctx, utctx,
                                     readsock);
        if (components[i]->valpoints != NULL)
            register_valpoints(components[i]->valpoints);
        if (components[i]->datapoints != NULL)
            register_datapoints(components[i]->datapoints);
        if (components[i]->actionpoints != NULL)
            register_actionpoints(components[i]->actionpoints);
    }

}

static int get_ctlsock(struct addrinfo *addr)
{
    int sock;

    if ((sock = socket(addr->ai_family,
                       addr->ai_socktype, addr->ai_protocol))
        < 0)
        fail("Failed to create control socket");
    if (confd_connect(dctx, sock, CONTROL_SOCKET, addr->ai_addr,
                      addr->ai_addrlen) != CONFD_OK) {
        close(sock);
        return -1;
    }
    fcntl(sock, F_SETFD, FD_CLOEXEC);
    return sock;
}


static void register_valpoints(struct confd_valpoint_cb *valpoints) {
    struct confd_valpoint_cb *valp;

    /* Register validation points */
    for (valp = valpoints; valp->valpoint[0] != '\0'; valp++) {
        if (confd_register_valpoint_cb(dctx, valp) != CONFD_OK)
            fail("Failed to register validation point \"%s\"",
                 valp->valpoint);
    }

}

static void register_datapoints(struct confd_data_cbs *datapoints) {
    struct confd_data_cbs *datap;

    /* Register data point callbacks */
    for (datap = datapoints;
         datap->callpoint[0] != '\0'; datap++) {

        LOG("Registering data point %s",
            datap->callpoint);

        if (confd_register_data_cb(dctx, datap) != CONFD_OK)
            fail("Failed to register data point \"%s\"",
                 datap->callpoint);
    }

}

int action_init_default(struct confd_user_info *uinfo) {

    LOG("Setting worker sock %d.", workersock);

    confd_action_set_fd(uinfo, workersock);
    return CONFD_OK;
}

static void register_actionpoints(struct confd_action_cbs *actionpoints) {
    struct confd_action_cbs *actionp;

    /* Register action points */
    for (actionp = actionpoints;
         actionp->actionpoint[0] != '\0'; actionp++) {
        if (actionp->init == NULL) {
            actionp->init = action_init_default;
        }

        info("registering action point %s", actionp->actionpoint);
        if (confd_register_action_cbs(dctx, actionp) != CONFD_OK)
            fail("Failed to register action point \"%s\"",
                 actionp->actionpoint);
    }

}

static int get_workersock(struct addrinfo *addr)
{
    int sock;

    if ((sock = socket(addr->ai_family,
                       addr->ai_socktype, addr->ai_protocol))
        < 0)
        fail("Failed to create worker socket");
    if (confd_connect(dctx, sock, WORKER_SOCKET, addr->ai_addr,
                      addr->ai_addrlen) != CONFD_OK) {
        error("Failed to connect worker socket");
        close(sock);
        return -1;
    }
    fcntl(sock, F_SETFD, FD_CLOEXEC);
    return sock;
}

static int get_maapisock(struct addrinfo *addr)
{
    int sock;

    if ((sock = socket(addr->ai_family, addr->ai_socktype,
                       addr->ai_protocol))
        < 0)
        fail("Failed to create maapi socket");
    if (maapi_connect(sock, addr->ai_addr, addr->ai_addrlen)
        != CONFD_OK) {
        error("Failed to connect maapi socket");
        close(sock);
        return -1;
    }
    fcntl(sock, F_SETFD, FD_CLOEXEC);
    return sock;
}

static int get_readsock(struct addrinfo *addr)
{
    int sock;

    if ((sock = socket(addr->ai_family, addr->ai_socktype,
                       addr->ai_protocol))
        < 0)
        fail("Failed to create read socket");
    if (cdb_connect(sock, CDB_DATA_SOCKET, addr->ai_addr,
                    addr->ai_addrlen) != CONFD_OK) {
        error("Failed to connect read socket");
        close(sock);
        return -1;
    }
    fcntl(sock, F_SETFD, FD_CLOEXEC);
    return sock;
}

static int get_notifsock(struct addrinfo *addr)
{
    int sock;

    if ((sock = socket(addr->ai_family, addr->ai_socktype,
                       addr->ai_protocol))
        < 0)
        fail("Failed to create notifications socket");
    if (confd_notifications_connect(sock, addr->ai_addr,
                                    addr->ai_addrlen,
                                    CONFD_NOTIF_SYSLOG) != CONFD_OK) {
        error("Failed to connect notifications socket");
        close(sock);
        return -1;
    }
    fcntl(sock, F_SETFD, FD_CLOEXEC);
    return sock;
}

static int get_subsock(struct addrinfo *addr)
{
    int sock;

    if ((sock = socket(addr->ai_family, addr->ai_socktype,
                       addr->ai_protocol))
        < 0)
        fail("Failed to create subscription socket");
    if (cdb_connect(sock, CDB_SUBSCRIPTION_SOCKET, addr->ai_addr,
                    addr->ai_addrlen) != CONFD_OK) {
        error("Failed to connect subscription socket");
        close(sock);
        return -1;
    }
    fcntl(sock, F_SETFD, FD_CLOEXEC);
    return sock;
}

static int init_validation(struct confd_trans_ctx *tctx)
{
    int i, ret = CONFD_OK;

    confd_trans_set_fd(tctx, workersock);
    maapi_attach(maapisock, 0, tctx);

    bind_msock_to_opaque(tctx, &maapisock);
    for (i = 0; i < num_components && ret == CONFD_OK; i++) {
        if (components[i]->init_validation != NULL)
            ret = (*components[i]->init_validation)(tctx);
    }
    if (ret != CONFD_OK) {
        /* Need to call stop for successfully called inits */
        for (i -= 2; i >= 0; i--) {
            if (components[i]->stop_validation != NULL)
                (*components[i]->stop_validation)();
        }
    }
    return ret;
}

static int stop_validation(struct confd_trans_ctx *tctx)
{
    int i;

    for (i = num_components - 1; i >= 0; i--) {
        if (components[i]->stop_validation != NULL)
            (*components[i]->stop_validation)();
    }
    maapi_detach(maapisock, tctx);
    return CONFD_OK;
}

static int init_data(struct confd_trans_ctx *tctx)
{
    int i, ret = CONFD_OK;

    confd_trans_set_fd(tctx, workersock);
    maapi_attach(maapisock, 0, tctx);

    bind_msock_to_opaque(tctx, &maapisock);
    for (i = 0; i < num_components && ret == CONFD_OK; i++) {
        if (components[i]->init_data != NULL)
            ret = (*components[i]->init_data)(tctx);
    }

    if (ret != CONFD_OK) {
        /* Need to call finish for successfully called inits */
        for (i -= 2; i >= 0; i--) {
            if (components[i]->finish_data != NULL)
                (*components[i]->finish_data)(tctx);
        }
    }
    return ret;
}

static int finish_data(struct confd_trans_ctx *tctx)
{
    int i;

    for (i = num_components - 1; i >= 0; i--) {
        if (components[i]->finish_data != NULL)
            (*components[i]->finish_data)(tctx);
    }
    return CONFD_OK;
}

static int start_read_any_session(int sock, enum cdb_db_type type)
{
    int started = 0;

    while (!started) {
        if (cdb_start_session(sock, type) != CONFD_OK) {
            if (confd_errno != CONFD_ERR_LOCKED) {
                return -1;
            } else {
                sleep(1);
            }
        } else {
            started = 1;
        }
    }
#if 0
    if (cdb_set_namespace(sock, NAMESPACE) != CONFD_OK)
        fail("Failed to set namespace \"linuxcfg\"");
#endif
    if (cdb_cd(sock, "%s", root[0] == '\0' ? "/" : root) != CONFD_OK)
        fail("Failed CDB cd to root: %s", root);
    return 0;
}

static int start_read_session(int sock)
{
    return start_read_any_session(sock, CDB_RUNNING);
}

int start_read_old_session()
{
    int sock = get_readsock(addr);
    if (-1 == sock) {
        error("Failed to open CDB socket!");
        return -1;
    }

    int ret = start_read_any_session(sock, CDB_PRE_COMMIT_RUNNING);
    if (-1 == ret) {
        error("Failed to start CDB PRE-COMMIT session!");
        return -1;
    }

    return sock;
}

static void call_subscriber(int spoint, int ssock, int rsock)
{
    struct subscription *p = subscriptions;

    while (p != NULL) {
        if (p->spoint == spoint) {
            (*p->cb)(rsock, ssock, spoint);
            return;
        }
        p = p->next;
    }
}

static void clear_subscriptions(void)
{
    struct subscription *p = subscriptions, *np;

    while (p != NULL) {
        np = p->next;
        free(p);
        p = np;
    }
    subscriptions = NULL;
}

static int ensure_death(pid_t *pid, int n)
{
    usleep(100000);
    if (killpids(pid, n, 0) == 0)
        return 1;
    sleep(1);
    if (killpids(pid, n, 0) == 0)
        return 1;
    (void) killpids(pid, n, SIGKILL);
    usleep(100000);
    if (killpids(pid, n, 0) == 0)
        return 1;
    return 0;
}

static int killpids(pid_t *pid, int n, int signal)
{
    int i, nfound = 0;

    for (i = 0; i < n; i++) {
        if (pid[i] != 0) {
            if (kill(pid[i], signal) == 0)
                nfound++;
            else
                pid[i] = 0;
        }
    }
    return nfound;
}

/************** exported functions **************/

int subscribe(int prio, void (*cb)(int readsock, int subsock,
                                    int subpoint),
               char *path)
{
    struct subscription **p = &subscriptions;
    int n = 0;
    int ret;

    while (*p != NULL) {
        if (++n >= MAX_SUBSCRIPTIONS)
            fail("Too many subscriptions %d (max %d)",
                 n, MAX_SUBSCRIPTIONS);
        p = &(*p)->next;
    }
    *p = xmalloc(sizeof(struct subscription));
    memset(*p, 0, sizeof(struct subscription));
    if ((ret = cdb_subscribe(subsock, prio, 0, &(*p)->spoint,
                             "%s",
                             path)) != CONFD_OK) {
        if (ret == CONFD_EOF) {
            /* Lost connection - will be handled in poll loop */
            error("Failed to do initial subscription - restarting");
        } else {
            fail("Failed to subscribe to %s/%s", root, path);
        }
    }
    (*p)->cb = cb;

    return (*p)->spoint;
}

const char *get_system_root()
{
    return root;
}

int validate_fail(struct confd_trans_ctx *tctx, char *fmt, ...)
{
    va_list ap;
    char err[256];

    va_start(ap, fmt);
    vsnprintf(err, sizeof(err), fmt, ap);
    va_end(ap);
    confd_trans_seterr(tctx, "%s", err);
    return CONFD_ERR;
}

int validate_warn(struct confd_trans_ctx *tctx, char *fmt, ...)
{
    va_list ap;
    char err[256];

    va_start(ap, fmt);
    vsnprintf(err, sizeof(err), fmt, ap);
    va_end(ap);
    confd_trans_seterr(tctx, "%s", err);
    return CONFD_VALIDATION_WARN;
}

int get_data_not_found(struct confd_trans_ctx *tctx)
{
    confd_data_reply_not_found(tctx);
    return CONFD_OK;
}

int get_data_unknown_path(struct confd_trans_ctx *tctx,
                          confd_hkeypath_t *kp)
{
    char path[BUFSIZ];

    confd_pp_kpath(path, sizeof(path), kp);
    error("Request for data with unknown path: %s", path);
    confd_trans_seterr(tctx, "Internal error");
    return CONFD_ERR;
}


/*
  Run command, optionally returning stdout/err:
  outbuf == NULL:                  Don't return stdout/err
  outbuf != NULL, *outbuf == NULL: malloc for stdout/err,  at most
  outlen if > 0 (caller frees)
  outbuf != NULL, *outbuf != NULL: use buffer (size outlen)
  provided by caller.
  On non-zero exit code from the command or other failure,
  log error unless command is prefixed with '-'.
  Return value:   0 - OK
  -1 - not OK
*/
int run(char **outbuf, int outlen, char *fmt, ...)
{
    va_list ap;
    char cmd[256];
    char errbuf[1024], dumbuf[1024];
    char *cmdp = cmd;
    char *obuf, *obp, *p;
    int malloced = 0;
    int full = 0;
    int pipefd[2];
    pid_t pid;
    int i, n, olen, status;
    char *argv[64];
    int ok = 0;

    va_start(ap, fmt);
    vsnprintf(cmd, sizeof(cmd), fmt, ap);
    va_end(ap);

    info("run()-ing command: '%s'", cmd);
    if (dummy_cmd_run) {
        LOG("running in dummy mode command %s", cmd);
        return 0;
    }
    cmdp = cmd;
    if (*cmdp == '-')
        cmdp++;
    if (outbuf == NULL) {
        olen = sizeof(errbuf);
        obuf = errbuf;
    } else {
        if (*outbuf == NULL) {
            olen = (outlen > 0 && outlen < 1024) ? outlen : 1024;
            *outbuf = xmalloc(olen);
            malloced = 1;
        } else {
            olen = outlen;
        }
        obuf = *outbuf;
    }
    obp = obuf;
    *obp = '\0';
    if (pipe(pipefd) != 0)
        fail("Failed to create pipe for running command");

    switch ((pid = fork())) {
    case 0: /* child */
        close(pipefd[0]);
        dup2(pipefd[1], STDOUT_FILENO);
        dup2(pipefd[1], STDERR_FILENO);
        if (pipefd[1] != STDOUT_FILENO &&
            pipefd[1] != STDERR_FILENO)
            close(pipefd[1]);
        argv[0] = cmdp;
        for (i = 1, p = cmdp; (p = strchr(p, ' ')) != NULL; i++) {
            *p++ = '\0';
            if (i > sizeof(argv) / sizeof(argv[0]) - 2) {
                fprintf(stderr, "Too many arguments\n");
                exit(126);
            }
            argv[i] = p;
        }
        argv[i] = NULL;
        if ((p = strrchr(argv[0], '/')) != NULL)
            argv[0] = ++p;
        execve(cmdp, argv, NULL);
        perror(NULL);
        _exit(127);
    case -1: /* failure */
        close(pipefd[1]);
        break;
    default: /* parent */
        close(pipefd[1]);
        while (!full &&
               (n = read(pipefd[0], obp, olen - (obp - obuf) - 1))
               > 0) {
            obp += n;
            if (olen - (obp - obuf) <= 1) {
                if (!malloced) {
                    full = 1;
                } else {
                    if (outlen > 0) {
                        if (olen >= outlen)
                            full = 1;
                        else
                            n = (outlen < olen + 1024) ? outlen
                                : olen + 1024;
                    } else {
                        n = olen + 1024;
                    }
                    if (!full) {
                        if ((*outbuf = realloc(obuf, n)) == NULL) {
                            *outbuf = obuf;
                            full = 1;
                        } else {
                            olen = n;
                            obp = *outbuf + (obp - obuf);
                            obuf = *outbuf;
                        }
                    }
                }
            }
        }
        *obp = '\0';
        if (full) {
            while (read(pipefd[0], dumbuf, sizeof(dumbuf)) > 0)
                ;
        }
        waitpid(pid, &status, 0);
        if (WIFEXITED(status) && WEXITSTATUS(status) == 0)
            ok = 1;
        break;
    }
    close(pipefd[0]);

    if (!ok) {
        if (cmd[0] != '-') {
            if (obp > obuf && obp[-1] == '\n')
                obp--;
            if (obp > obuf)
                error("Failed to run '%s': %s",
                      cmd, obuf);
            else
                error("Failed to run '%s'", cmd);
        }
        return -1;
    }
    return 0;
}

/* Returns:
   -1: file1 doesn't exist
   -2: file2 doesn't exist
   0: files are identical
   1: files differ
*/
int cmp(char *file1, char *file2)
{
    struct stat st;
    int n;
    FILE *fp1, *fp2;
    char buf1[BUFSIZ], buf2[BUFSIZ];

    if (stat(file1, &st) != 0)
        return -1;
    n = st.st_size;
    if (stat(file2, &st) != 0)
        return -2;
    if (n != st.st_size)
        return 1;
    if ((fp1 = fopen(file1, "r")) == NULL)
        return -1;
    if ((fp2 = fopen(file2, "r")) == NULL) {
        fclose(fp1);
        return -2;
    }
    while ((n = fread(buf1, 1, sizeof(buf1), fp1))
           == fread(buf2, 1, sizeof(buf2), fp2) && n != 0
           && memcmp(buf1, buf2, n) == 0)
        ;
    fclose(fp1);
    fclose(fp2);
    if (n != 0)
        return 1;
    return 0;
}

/* read complete file to malloced buffer (caller frees)
   returns size of file or -1 on error */
int read_file(char *file, char **bufp)
{
    int fd, n, ret = -1;
    struct stat st;
    char *p;

    if ((fd = open(file, O_RDONLY)) >= 0) {
        if (fstat(fd, &st) == 0) {
            n = st.st_size;
            p = xmalloc(n + 1);
            if (read(fd, p, n) == n) {
                p[n] = '\0';
                *bufp = p;
                ret = 0;
            }
            if (ret != 0)
                free(p);
        }
        close(fd);
    }
    return ret;
}


void file_kill(char *server, char *pidfile, int signal)
{
    FILE *fp;
    char buf[16];
    pid_t pid = 0;
    char *action = signal == SIGHUP ? "restart" : "kill";

    if ((fp = fopen(pidfile, "r")) != NULL) {
        if (fgets(buf, sizeof(buf), fp) != NULL
            && (pid = atoi(buf)) > 0) {
            if (kill(pid, signal) != 0) {
                error("Failed to %s %s server: Signal %d "
                      "to pid %d: %s", action,
                      server, signal, pid, strerror(errno));
                pid = 0;
            }
        } else {
            error("Failed to %s %s server: %s: No pid found",
                  action, server, pidfile);
            pid = 0;
        }
        fclose(fp);
    } else {
        error("Failed to %s %s server: %s: %s",
              action, server, pidfile, strerror(errno));
    }
    if (pid == 0 || signal == SIGHUP)
        return;
    if (!ensure_death(&pid, 1))
        error("Failed to kill %s server: Process %d won't die", pid);
}

void trigger_subscriptions(int *subpoints) {
  int s;
  pid_t pid;


  pid = fork();

  if (!pid) {
      assert((s = socket(PF_INET, SOCK_STREAM, 0)) >= 0);
      CHECK2(cdb_connect(s, CDB_DATA_SOCKET,
                         (struct sockaddr *)addr->ai_addr,
                         addr->ai_addrlen),
             "Failed to connect to cdb");

      info("calling cdb_wait_start()");
      cdb_wait_start(s);      /* Make sure phase1 is complete */

      /* blocks until all clients have ack:ed */
      CHECK2(cdb_trigger_subscriptions(s, subpoints, 0),
             "Failed to trigger subscriptions");

      info("Initial synchronization done.");

      cdb_close(s);
      exit(0);
  }
}

void killall(char *server, int signal)
{
    DIR *dirp;
    struct dirent *dp;
    char fname[MAXPATHLEN], cmdline[MAXPATHLEN];
    FILE *fp;
    pid_t pid[64];
    char *p;
    int n = 0;
    char *action = signal == SIGHUP ? "restart" : "kill";

    if ((dirp = opendir("/proc")) != NULL) {
        while (n < sizeof(pid) / sizeof(pid[0]) &&
               (dp = readdir(dirp)) != NULL) {
            pid[n] = strtol(dp->d_name, &p, 10);
            if (*p == '\0') {
                snprintf(fname, sizeof(fname),
                         "/proc/%d/cmdline", pid[n]);
                if ((fp = fopen(fname, "r")) != NULL) {
                    if (fgets(cmdline, sizeof(cmdline), fp)
                        != NULL) {
                        if ((p = strrchr(cmdline, '/')) != NULL)
                            p++;
                        else
                            p = cmdline;
                        if (strcmp(p, server) == 0) {
                            if (kill(pid[n], signal) != 0) {
                                error("Failed to %s %s server:"
                                      " Signal %d to pid %d: %s",
                                      action,
                                      server, signal, pid[n],
                                      strerror(errno));
                                pid[n] = 0;
                            }
                            n++;
                        }
                    }
                    fclose(fp);
                }
            }
        }
        closedir(dirp);
    }
    if (n == 0) {
        error("Failed to %s %s server: No processes found",
              action, server);
        return;
    }
    if (signal == SIGHUP)
        return;
    if (!ensure_death(pid, n))
        error("Failed to kill %s server: Some processes won't die");
}

/* Returns:
   1   - file exists and is executable
   0   - otherwise
*/
int is_executable_file(char *path)
{
    struct stat sb;

    if (0 == stat(path, &sb) && S_ISREG(sb.st_mode) &&
        sb.st_mode & S_IXUSR)
        return 1;

    return 0;
}

char *get_ext_cmd(struct external_cmd *ext_cmd)
{
    int i;
    char *cmd = NULL, *s = NULL;
    char buf[512];

    // No operation when ext_cmd->cmd is already filled
    if (ext_cmd->cmd != NULL) {
        return ext_cmd->cmd;
    }

    // Check the environment variable
    if (ext_cmd->env_var != NULL) {
        s = getenv(ext_cmd->env_var);
        info("Trying environment variable %s: '%s'",
             ext_cmd->env_var, s);
        if (s != NULL && is_executable_file(s)) {
            cmd = s;
            goto end;
        }

        if (NULL != ext_cmd->binary_name) {
            snprintf(buf, 512, "%s%s", s, ext_cmd->binary_name);

            if (is_executable_file(buf)) {
                cmd = strdup(buf);
                goto end;
            }
        }
    }

    // Check the compiler's macro
    if (ext_cmd->cc_macro != NULL) {
        info("Trying CC macro: '%s'", ext_cmd->cc_macro);
        if (is_executable_file(ext_cmd->cc_macro)) {
            cmd = ext_cmd->cc_macro;
            goto end;
        }
    }

    // Check the binary in $PATH
    if (NULL != ext_cmd->binary_name &&
        (NULL != (s = getenv("PATH")))) {

        char *sdup = strdup(s);
        char *str;
        char buf[BUFSIZ];

        for (str = sdup;; str = NULL) {
            s = strtok(str, ":");
            if (s == NULL)
                break;

            snprintf(buf, BUFSIZ, "%s/%s", s, ext_cmd->binary_name);
            info("Trying binary in $PATH '%s'", buf);
            if (is_executable_file(buf)) {
                cmd = buf;
                free(sdup);
                goto end;
            }
        }
        free(sdup);
    }

    // Check the '/path/cmd' strings
    for (i = 0; ext_cmd->paths[i] != NULL; i++) {
        info("Trying path[%d]: '%s'", i, ext_cmd->paths[i]);
        if (is_executable_file(ext_cmd->paths[i])) {
            cmd = ext_cmd->paths[i];
            goto end;
        }
    }

  end: if (cmd != NULL) {
        int len = strlen(cmd) + 1;
        if (ext_cmd->args != NULL)
            len += 1 + strlen(ext_cmd->args);

        ext_cmd->cmd = calloc(len, sizeof(char));

        if (ext_cmd->args != NULL)
            snprintf(ext_cmd->cmd, len, "%s %s",
                     cmd, ext_cmd->args);
        else
            snprintf(ext_cmd->cmd, len, "%s", cmd);

        info("Using '%s' as '%s' command",
             ext_cmd->cmd, ext_cmd->descr_name);

        return ext_cmd->cmd;

    } else {
        error("Can't find external command '%s'!",
              ext_cmd->descr_name);
        return NULL;
    }
}


#define MASK(addr, len) \
    (addr & (0xffffffff << (8 * sizeof(addr) - (len))))

void mk4subnet(struct in_addr *addr, int prefixlen,
               struct in_addr *net)
{
    net->s_addr = htonl(MASK(ntohl(addr->s_addr), prefixlen));
}

void mk6subnet(struct in6_addr *addr, int prefixlen,
               struct in6_addr *net)
{
    int i, rem;

    for (i = 0, rem = prefixlen; i < 16; i++, rem -= 8) {
        if (rem >= 8)
            net->s6_addr[i] = addr->s6_addr[i];
        else if (rem > 0)
            net->s6_addr[i] = MASK(addr->s6_addr[i], rem);
        else
            net->s6_addr[i] = 0;
    }
}

void info(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    if ((linuxcfg_loglevel & INFO_LOG_STDERR) != 0) {
        vfprintf(stderr, fmt, ap);
        fprintf(stderr, "\n");
    }
    va_end(ap);

    va_start(ap, fmt);
    if ((linuxcfg_loglevel & INFO_LOG_SYSLOG) != 0)
        vsyslog(LOG_INFO, fmt, ap);
    va_end(ap);
}

void warn(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    if ((linuxcfg_loglevel & INFO_LOG_STDERR) != 0) {
        vfprintf(stderr, fmt, ap);
        fprintf(stderr, "\n");
    }
    va_end(ap);

    va_start(ap, fmt);
    vsyslog(LOG_WARNING, fmt, ap);
    va_end(ap);
}

char *kpath2str(const confd_hkeypath_t *hkeypath) {
    char *buf = malloc(512);
    confd_pp_kpath(buf, 512,
                   hkeypath);

    return buf;
}

void error(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    if ((linuxcfg_loglevel & INFO_LOG_STDERR) != 0) {
        vfprintf(stderr, fmt, ap);
        fprintf(stderr, "\n");
    }
    va_end(ap);

    va_start(ap, fmt);
    vsyslog(LOG_ERR, fmt, ap);
    va_end(ap);
}

void fail(const char *fmt, ...)
{
    va_list ap;
    char buf[BUFSIZ];

    va_start(ap, fmt);
    snprintf(buf, sizeof(buf), "%s, exiting\n", fmt);
    if ((linuxcfg_loglevel & INFO_LOG_STDERR) != 0) {
        vfprintf(stderr, buf, ap);
    }
    vsyslog(LOG_ERR, buf, ap);
    va_end(ap);
    exit(1);
}

void *xmalloc(size_t size)
{
    void *ret;
    if ((ret = malloc(size)) == NULL)
        fail("Failed to allocate %d bytes", size);
    return ret;
}

int get_msock_from_opaq(void *tctx_opaque_data)
{
    struct opaque_data *data = (struct opaque_data *) tctx_opaque_data;
    return (NULL == data) ? -1 : *(data->msock);
}