/*********************************************************************
 * ConfD logging framework example (use of traceh.h)
 * Implements subscriber, operational data provider and validation.
 *
 * (C) 2017 Tail-f Systems
 * Permission to use this code as a starting point hereby granted
 * This is ConfD Sample Code
 *
 * See the README file for more information
 ********************************************************************/

#include <sys/poll.h>
#include <sys/socket.h>
#include <string.h>
#include <stdlib.h>
#include <confd.h>
#include <confd_cdb.h>
// the following define should be used befor inclucde of traceh.h
// in (only) one source file to force definition of supporting functions
#define _TRACE_DECLARE
#include <traceh.h>
#include "logging-model.h"

/********************************************************************/
/* Our daemon context as a global variable */
static struct confd_daemon_ctx *dctx;
static int ctlsock;
static int workersock;
static int subsock;
static int logging_spoint;
static char* daemon_name = "logging-example";

static void report_logging_levels(void)
{
    INFO_ENTER("");
    FATAL("FATAL is enabled");
    WARN("WARN is enabled");
    INFO("INFO is enabled");
    DEBUG("DEBUG is enabled");
    TRACE("TRACE is enabled");
    INFO_EXIT("");
}

static int s_init(struct confd_trans_ctx *tctx)
{
    TRACE_ENTER("");
    int ret = CONFD_OK;
    confd_trans_set_fd(tctx, workersock);
    TRACE_EXIT("ret %i", ret);
    return ret;
}

static int s_finish(struct confd_trans_ctx *tctx)
{
    TRACE_ENTER("");
    int ret = CONFD_OK;
    TRACE_EXIT("ret %i", ret);
    return ret;
}

static int get_elem(struct confd_trans_ctx *tctx, confd_hkeypath_t *keypath)
{
    TRACE_ENTER("");
    int ret = CONFD_OK;
    confd_value_t v;
    int val;

    switch (CONFD_GET_XMLTAG(&(keypath->v[0][0]))) {
    case logging_console_level:
        TRACE("console level");
        val = get_trace_log_level();
        CONFD_SET_ENUM_VALUE(&v, val);
        break;
    case logging_stream_level:
        TRACE("stream level");
        val = get_trace_stream_level();
        CONFD_SET_ENUM_VALUE(&v, val);
        break;
    case logging_syslog_level:
        TRACE("syslog level");
        val = get_trace_syslog_level();
        CONFD_SET_ENUM_VALUE(&v, val);
        break;
    case logging_compiled_level:
        TRACE("compile level");
        val = get_trace_log_compiled_level();
        CONFD_SET_ENUM_VALUE(&v, val);
        break;
    case logging_syslog_initialized:
        TRACE("syslog initialized");
        val = is_syslog_initialized();
        CONFD_SET_BOOL(&v, val);
        break;
    case logging_streams_initialized:
        TRACE("streams initialized");
        val = are_streams_initialized();
        CONFD_SET_BOOL(&v, val);
        break;
    case logging_time_format:
        TRACE("time format");
        val = get_trace_time_format();
        CONFD_SET_ENUM_VALUE(&v, val);
        break;
    }

    TRACE("get_elem value found val=%i", val);
    confd_data_reply_value(tctx, &v);

    TRACE_EXIT("ret %i", ret);
    return ret;
}

static int get_next(struct confd_trans_ctx *tctx, confd_hkeypath_t *keypath,
        long next)
{
    TRACE_ENTER("");
    int ret = CONFD_ERR;
    FATAL("As data model has no list, the 'get_next' should not be called!");
    TRACE_EXIT("ret %i", ret);
    return ret;
}

static int init_validation(struct confd_trans_ctx *tctx)
{
    TRACE_ENTER("");
    int ret = CONFD_OK;
    confd_trans_set_fd(tctx, workersock);
    TRACE_EXIT("ret %i", ret);
    return ret;
}

static int stop_validation(struct confd_trans_ctx *tctx)
{
    TRACE_ENTER("");
    int ret = CONFD_OK; // no need to do anything
    TRACE_EXIT("ret %i", ret);
    return ret;
}

static int validate_logging(struct confd_trans_ctx *tctx,
        confd_hkeypath_t *keypath, confd_value_t *newval)
{
    TRACE_ENTER("");
    char buf[BUFSIZ];
    int ret = CONFD_OK;
    confd_pp_kpath(buf, BUFSIZ, keypath);
    TRACE("Keypath=%s", buf);

    int new_level = CONFD_GET_ENUM_VALUE(newval);
    TRACE("validation new_level=%i", new_level);

    if (new_level > get_trace_log_compiled_level()) {
        WARN("Validation issue level %i compiled level %i", new_level,
                get_trace_log_compiled_level());
        char ebuf[BUFSIZ];
        sprintf(ebuf, "Level %s cannot be set as it is more detailed than"
                " compiled level %s!", get_trace_level_name(new_level),
                get_trace_level_name(get_trace_log_compiled_level()));
        confd_trans_seterr(tctx, "%s", ebuf);
        ret = CONFD_ERR;
    }

    TRACE_EXIT("ret %i", ret);
    return ret;
}

int init_confd(void)
{
    INFO_ENTER("");
    int ret_val = CONFD_ERR;
    int status;
    struct sockaddr_in addr;
    enum confd_debug_level debuglevel = CONFD_DEBUG;

#ifdef T_LOG_TRACE  // set ConfD log level according to traceh.h
    debuglevel = CONFD_TRACE;
#endif

    /* initialize confd library */
    confd_init(daemon_name, stderr, debuglevel);

    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(CONFD_PORT);

    if (confd_load_schemas((struct sockaddr*) &addr,
            sizeof(struct sockaddr_in)) != CONFD_OK)
        confd_fatal("Failed to load schemas from confd\n");

    if ((dctx = confd_init_daemon(daemon_name)) == NULL)
        confd_fatal("Failed to initialize confdlib\n");

    /* Create the control socket */
    if ((ctlsock = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
        FATAL("Failed to open ctlsocket");
        goto term;
    }

    if (confd_connect(dctx, ctlsock, CONTROL_SOCKET, (struct sockaddr*) &addr,
            sizeof(struct sockaddr_in)) < 0) {
        FATAL("Failed to confd_connect() to confd");
        goto term;
    }

    /* Create the workersocket */
    if ((workersock = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
        FATAL("Failed to open workersocket");
        goto term;
    }

    if (confd_connect(dctx, workersock, WORKER_SOCKET, (struct sockaddr*) &addr,
            sizeof(struct sockaddr_in)) < 0) {
        FATAL("Failed to confd_connect() to confd");
        goto term;
    }

    /* trnsaction callbacks */
    struct confd_trans_cbs trans;
    memset(&trans, 0, sizeof(struct confd_trans_cbs));
    trans.init = s_init;
    trans.finish = s_finish;
    if (confd_register_trans_cb(dctx, &trans) == CONFD_ERR) {
        FATAL("Failed to register trans cb");
        goto term;
    }

    /* data (provider) callbacks */
    struct confd_data_cbs data;
    memset(&data, 0, sizeof(struct confd_data_cbs));
    data.get_elem = get_elem;
    data.get_next = get_next;
    strcpy(data.callpoint, logging__callpointid_logging_state_dp);
    if (confd_register_data_cb(dctx, &data) == CONFD_ERR) {
        FATAL("Failed to register data cb \n");
        goto term;
    }

    /* validation */
    struct confd_trans_validate_cbs vcb;
    memset(&vcb, 0, sizeof(struct confd_trans_validate_cbs));
    vcb.init = init_validation;
    vcb.stop = stop_validation;
    confd_register_trans_validate_cb(dctx, &vcb);
    struct confd_valpoint_cb val_logging;
    memset(&val_logging, 0, sizeof(struct confd_valpoint_cb));
    val_logging.validate = validate_logging;
    strcpy(val_logging.valpoint, logging__validateid_logging_config_val);
    if (CONFD_OK != confd_register_valpoint_cb(dctx, &val_logging)) {
        FATAL("Failed to register_validation point \n");
        goto term;
    }

    if (confd_register_done(dctx) != CONFD_OK)
        confd_fatal("Failed to complete registration \n");

    /* subscription */
    if ((subsock = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
        FATAL("Failed to open socket\n");
        goto term;
    }
    if (cdb_connect(subsock, CDB_SUBSCRIPTION_SOCKET, (struct sockaddr*) &addr,
            sizeof(struct sockaddr_in)) < 0) {
        FATAL("Failed to cdb_connect() to confd \n");
        goto term;
    }
    if ((status = cdb_subscribe(subsock, 3, logging__ns, &logging_spoint,
            "/logging")) != CONFD_OK) {
        FATAL("subscribe %d\n", status);
        goto term;

    }
    if (cdb_subscribe_done(subsock) != CONFD_OK) {
        FATAL("cdb_subscribe_done() failed");
        goto term;
    }
    TRACE("Subscription point = %d\n", logging_spoint);

    ret_val = CONFD_OK;

    term:
    INFO_EXIT("tet_val=%i", ret_val);
    return ret_val;
}

int confd_loop(void)
{
    INFO_ENTER("");
    int ret = CONFD_ERR;
    int status;

    while (1) {
        struct pollfd set[3];
        TRACE("ctlsock=%i workersock=%i subsock=%i", ctlsock, workersock);

        set[0].fd = ctlsock;
        set[0].events = POLLIN;
        set[0].revents = 0;

        set[1].fd = workersock;
        set[1].events = POLLIN;
        set[1].revents = 0;

        set[2].fd = subsock;
        set[2].events = POLLIN;
        set[2].revents = 0;

        if (poll(set, sizeof(set) / sizeof(*set), -1) < 0) {
            FATAL("Poll failed:");
            break;
        }

        /* Check for I/O */
        if (set[0].revents & POLLIN) {
            TRACE("ctrlsock event");
            if ((ret = confd_fd_ready(dctx, ctlsock)) == CONFD_EOF) {
                confd_fatal("Control socket closed\n");
            } else if (ret == CONFD_ERR && confd_errno != CONFD_ERR_EXTERNAL) {
                FATAL("Error on control socket request: %s (%d): %s\n",
                        confd_strerror(confd_errno), confd_errno,
                        confd_lasterr());
                break;
            }
        }

        if (set[1].revents & POLLIN) {
            TRACE("workersock event");
            if ((ret = confd_fd_ready(dctx, workersock)) == CONFD_EOF) {
                FATAL("Worker socket closed\n");
                break;
            } else if (ret == CONFD_ERR && confd_errno != CONFD_ERR_EXTERNAL) {
                FATAL("Error on worker socket request: %s (%d): %s\n",
                        confd_strerror(confd_errno), confd_errno,
                        confd_lasterr());
                break;
            }
        }

        /* process subscription */
        if (set[2].revents & POLLIN) {
            int sub_points[1];
            int reslen;
            TRACE("subsock event");
            if ((status = cdb_read_subscription_socket(subsock, &sub_points[0],
                    &reslen)) != CONFD_OK) {
                FATAL("sub_read: %d\n", status);
                break;
            }
            if (sub_points[0] == logging_spoint) {
                TRACE("subscription triggered");
                confd_tag_value_t *values;
                int nvalues, i, val;

                if (cdb_get_modifications(subsock, logging_spoint, 0, &values,
                        &nvalues,
                        NULL) == CONFD_OK) {
                    INFO("Logging subscription triggered");
                    for (i = 0; i < nvalues; i++) {
                        if (values[i].tag.tag == logging_console_level) {
                            TRACE("Processing console level");
                            val = CONFD_GET_ENUM_VALUE(&values[i].v);
                            TRACE("val=%i", val);
                            set_trace_log_level(val);
                        }
                        if (values[i].tag.tag == logging_syslog_level) {
                            TRACE("Processing syslog level");
                            val = CONFD_GET_ENUM_VALUE(&values[i].v);
                            TRACE("val=%i", val);
                            set_trace_syslog_level(val);
                        }
                        if (values[i].tag.tag == logging_stream_level) {
                            TRACE("Processing stream level");
                            val = CONFD_GET_ENUM_VALUE(&values[i].v);
                            TRACE("val=%i", val);
                            set_trace_stream_level(val);
                        }
                        if (values[i].tag.tag == logging_time_format) {
                            TRACE("Processing time format");
                            val = CONFD_GET_ENUM_VALUE(&values[i].v);
                            TRACE("val=%i", val);
                            set_trace_time_format(val);
                        }
                        confd_free_value(CONFD_GET_TAG_VALUE(&values[i]));
                    }
                    free(values);
                    report_logging_levels();
                }
            }

            if ((status = cdb_sync_subscription_socket(subsock,
                    CDB_DONE_PRIORITY)) != CONFD_OK) {
                FATAL("failed to sync subscription: %d\n", status);
                break;
            }

        }
    }

    INFO_EXIT("ret %i", ret);
    return ret;
}

int main(int argc, char *argv[])
{
    INFO_ENTER("argc %i", argc);
    int ret = CONFD_OK;

    report_logging_levels();
    if (CONFD_OK != init_confd()) {
        FATAL("Failed to initialize confd! Exiting");
    } else {
        ret = confd_loop();
    }

    INFO_EXIT("ret %i", ret);
    return ret;
}
