/*********************************************************************
 * ConfD hooks example
 *
 * (C) 2005-2017 Tail-f Systems
 * Permission to use this code as a starting point hereby granted
 * This is ConfD Sample Code.
 *
 * See the README file for more information
 ********************************************************************/

#include <arpa/inet.h>
#include <netinet/in.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>

#include <confd_lib.h>
#include <confd_dp.h>
#include <confd_maapi.h>

#define _TRACE_DECLARE
#include <traceh.h>
#include "maapi_example.h"

static const int _DEFAULT_CONFIRMED_TIMEOUT = 600;  // default timeout 600s
/* Our daemon context as a global variable */
static struct confd_daemon_ctx *dctx;
static int ctlsock;
static int workersock;
static int maapisock;
//For commit command we need different worker socket running
//in different confd loop (another thread) not to block validation callpoints.
//We also need different maapi socket as fist one is used by validation.
static int workersock_commit;
static int maapisock_commit;
static const char* items_keypath_string = "/config/items";
static const char* start_log_keyapth_string = "/config/start-log";

static void trace_confd_val(const char *txt, const confd_value_t *val)
{
#ifdef T_LOG_TRACE
    char buf[512];
    confd_pp_value(buf, sizeof(buf), val);
    TRACE("%s%s", txt, buf);
#endif
}

static void trace_confd_kp(const char *txt, confd_hkeypath_t *kp)
{
#ifdef T_LOG_TRACE
    if (kp) {
        char buf[1024];
        confd_pp_kpath(buf, sizeof(buf), kp);
        TRACE("%s%s", txt, buf);
    } else {
        TRACE("%s%p", txt, kp);
    }
#endif
}

static void trace_args(int usid, int argc, char **argv, char *path)
{
#ifdef T_LOG_TRACE
    int i;
    for (i = 0; i < argc; i++) {
        TRACE("argv[%i] = %s", i, argv[i]);
    }
#endif
}

static int s_init(struct confd_trans_ctx *tctx)
{
    TRACE_ENTER("");
    int rv = CONFD_OK;
    confd_trans_set_fd(tctx, workersock);
    DEBUG_EXIT("rv %i", rv);
    return rv;
}

static int s_finish(struct confd_trans_ctx *tctx)
{
    TRACE_ENTER("");
    int rv = CONFD_OK;
    DEBUG_EXIT("rv %i", rv);
    return rv;
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
    int rv = CONFD_OK; // no need to do anything
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

static int validate_vals(struct confd_trans_ctx *tctx,
                         confd_hkeypath_t *keypath, confd_value_t *newval)
{
    TRACE_ENTER("");
    char buf[BUFSIZ];
    int rv = CONFD_OK;
    const int threshold = 100;
    confd_pp_kpath(buf, BUFSIZ, keypath);
    TRACE("keypath=%s", buf);

    if (CONFD_OK
        != (rv = maapi_attach(maapisock, maapi_example__ns, tctx))) {
        FATAL("Failed to attach to maapi!");
        goto term;
    }

    struct maapi_cursor mc;
    maapi_init_cursor(maapisock, tctx->thandle, &mc, items_keypath_string);
    maapi_get_next(&mc);

    uint32_t val, sum = 0;
    while (CONFD_OK == rv && 0 != mc.n) {
        if (maapi_exists(maapisock, tctx->thandle, "%s{%x}/value",
                         items_keypath_string,
                         &mc.keys[0])) {
            TRACE("Value element exists");
            if (CONFD_OK
                != (rv = maapi_get_u_int32_elem(maapisock,
                                                tctx->thandle, &val,
                                                "%s{%x}/value",
                                                items_keypath_string,
                                                &mc.keys[0]))) {
                FATAL("failed to read value element!");
                break;
            }
            TRACE("val=%u", val);
            sum += val;
        }
        rv = maapi_get_next(&mc);
    }
    maapi_destroy_cursor(&mc);
    maapi_detach(maapisock, tctx);

    if (rv == CONFD_OK && sum > threshold) {
        sprintf(buf, "Sum of value elements in %s is %u, which is"
                " greater than %u!", items_keypath_string, sum, threshold);
        confd_trans_seterr(tctx, "%s", buf);
        WARN(buf);
        rv = CONFD_ERR;
    }

term:
    TRACE_EXIT("rv %i", rv);
    return rv;
}

static int get_next(struct confd_trans_ctx *tctx, confd_hkeypath_t *kp,
                    long next)
{
    TRACE_ENTER("next=%i", next);
    trace_confd_kp("", kp);
    int rv = CONFD_OK;

    if (CONFD_OK
        != (rv = maapi_attach(maapisock, maapi_example__ns, tctx))) {
        FATAL("Failed to attach to maapi!");
        goto term;
    }

    if (next == -1) { /* first call */
        next = 0;
    }

    struct maapi_cursor mc;
    int n = 0;
    maapi_init_cursor(maapisock, tctx->thandle, &mc, items_keypath_string);
    maapi_get_next(&mc);
    TRACE("mc.n=%i next=%i", mc.n, next); //mc.n is number of returned keys
    while (0 != mc.n && n != next) {
        maapi_get_next(&mc);
        n++;
        TRACE("mc.n=%i n=%i", mc.n, n);
    }
    if (mc.n == 0) {
        TRACE("No more item entry, element not found.");
        confd_data_reply_next_key(tctx, NULL, -1, -1);
    } else {
        next++;
        confd_data_reply_next_key(tctx, &mc.keys[0], 1, next);
    }
    maapi_destroy_cursor(&mc);
    maapi_detach(maapisock, tctx);

term:
    TRACE_EXIT("rv %i", rv);
    return rv;
}

static int get_elem(struct confd_trans_ctx *tctx, confd_hkeypath_t *kp)
{
    TRACE_ENTER("");
    trace_confd_kp("", kp);
    int rv = CONFD_OK;
    confd_value_t v;

    if (CONFD_OK
        != (rv = maapi_attach(maapisock, maapi_example__ns, tctx))) {
        FATAL("Failed to attach to maapi!");
        goto term;
    }

    if (kp->v[0][0].type == C_XMLTAG &&
        CONFD_GET_XMLTAG(&kp->v[0][0]) == maapi_example_value) {
        if (CONFD_OK != (rv = maapi_get_elem(maapisock,
                                             tctx->thandle, &v, "%s{%x}/value",
                                             items_keypath_string,
                                             &kp->v[1][0]))) {
            FATAL("Failed to fetch value!");
        } else {
            rv = confd_data_reply_value(tctx, &v);
        }
    } else {
        rv = confd_data_reply_not_found(tctx);
    }
    maapi_detach(maapisock, tctx);

term:
    TRACE_EXIT("rv %i", rv);
    return rv;
}

static int init_cmd(struct confd_user_info *uinfo)
{
    TRACE_ENTER("uinfo->usid=%i", uinfo->usid);
    int rv = CONFD_OK;
    confd_action_set_fd(uinfo, workersock);
    TRACE_EXIT("rv=%", rv);
    return rv;
}

static int init_cmd_commit(struct confd_user_info *uinfo)
{
    TRACE_ENTER("uinfo->usid=%i", uinfo->usid);
    int rv = CONFD_OK;
    confd_action_set_fd(uinfo, workersock_commit);
    TRACE_EXIT("rv=%", rv);
    return rv;
}

int do_start_count(struct confd_user_info *uinfo, char *path,
                   int argc_in, char **argv_in)
{
    TRACE_ENTER("path %s argc_in=%d", path, argc_in);
    int rv = CONFD_OK;

    if (CONFD_OK
        != (rv = maapi_attach2(maapisock, maapi_example__ns, uinfo->usid,
                               uinfo->actx.thandle))) {
        FATAL("Failed to attach to maapi!");
        goto term;
    }

    struct maapi_cursor mc;
    maapi_init_cursor(maapisock, uinfo->actx.thandle, &mc,
                      start_log_keyapth_string);
    maapi_get_next(&mc);

    uint32_t count = 0;
    while (CONFD_OK == rv && 0 != mc.n) {
        if (maapi_exists(maapisock, uinfo->actx.thandle, "%s{%x}",
                         start_log_keyapth_string,
                         &mc.keys[0])) {
            count++;
            TRACE("Value element count=%i", count);
        }
        rv = maapi_get_next(&mc);
    }
    maapi_destroy_cursor(&mc);
    maapi_detach2(maapisock, uinfo->actx.thandle);

    TRACE("count=%i", count);
    if (rv == CONFD_OK) {
        maapi_cli_printf(maapisock, uinfo->usid,
                         "\nApplication startup count %i\n", count);
    } else {
        maapi_cli_printf(maapisock, uinfo->usid,
                         "Cannot determine application startup count");
    }

term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

int xpath_eval_iter(confd_hkeypath_t *kp,
                    confd_value_t *v, void *state)
{
    TRACE_ENTER("state=%p", state);
    long usid = (long) state;
    TRACE("usid=%i", usid);
    trace_confd_val("v=", v);
    int rv = ITER_CONTINUE;
    maapi_cli_printf(maapisock, (int) usid, "\nItem %s\n",
                     CONFD_GET_CBUFPTR(v));
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

int do_show_items(struct confd_user_info *uinfo, char *path,
                  int argc_in, char **argv_in)
{
    TRACE_ENTER("path %s argc_in=%d", path, argc_in);
    int rv = CONFD_OK;
    if (argc_in != 2) { //command string + value = 2 arguments
        rv = CONFD_ERR;
        FATAL("Wrong number of arguments %i, expected 2", argc_in);
    }
    TRACE("value to search for is argv_in[1]=%s", argv_in[1]);

    if (CONFD_OK
        != (rv = maapi_attach2(maapisock, maapi_example__ns, uinfo->usid,
                               uinfo->actx.thandle))) {
        FATAL("Failed to attach to maapi!");
        goto term;
    }

    char qstr[BUFSIZ];
    snprintf(qstr, sizeof(qstr),
             "%s[value = %s]/name", items_keypath_string, argv_in[1]);
    TRACE("qrstr=%s", qstr);
    if (CONFD_OK != (rv = maapi_xpath_eval(maapisock, uinfo->actx.thandle, qstr,
                                           &xpath_eval_iter, NULL,
                                           (void*) (long) (uinfo->usid), ""))) {
        FATAL("Failed to evaluate xpath expression");
        goto term;
    }
    maapi_detach2(maapisock, uinfo->actx.thandle);

term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

int do_show_items_smaller_than(struct confd_user_info *uinfo, char *path,
                               int argc_in, char **argv_in)
{
    TRACE_ENTER("path %s argc_in=%d", path, argc_in);
    int rv = CONFD_OK;
    if (argc_in != 2) { //command string + value = 2 arguments
        rv = CONFD_ERR;
        FATAL("Wrong number of arguments %i, expected 2", argc_in);
    }
    TRACE("value to search for is argv_in[1]=%s", argv_in[1]);

    if (CONFD_OK
        != (rv = maapi_attach2(maapisock, maapi_example__ns, uinfo->usid,
                               uinfo->actx.thandle))) {
        FATAL("Failed to attach to maapi!");
        goto term;
    }
    char qstr[BUFSIZ];
    snprintf(qstr, sizeof(qstr),
             "%s[value < %s]", items_keypath_string, argv_in[1]);
    int qh = maapi_query_start(maapisock, uinfo->actx.thandle,
                               qstr, NULL,
                               0, 1, CONFD_QUERY_TAG_VALUE,
                               1, (const char *[] ) { "name" }, 0, NULL);
    TRACE("qh=%i", qh);

    int n = 0, i = 0, j = 0;
    struct confd_query_result *qr;
    do {
        maapi_query_result(maapisock, qh, &qr);
        n = qr->nresults;
        TRACE("n=%i qr->offset=%i", n, qr->offset);
        for (i = 0; i < n; i++) {
            TRACE("result %d", i + qr->offset);
            for (j = 0; j < qr->nelements; j++) {
                // the type is tag-value
                char *tag = confd_hash2str(qr->results[i].tv[j].tag.tag);
                TRACE("tag=%s", tag);
                trace_confd_val("val", &qr->results[i].tv[j].v);
                maapi_cli_printf(maapisock, uinfo->usid, "\nItem %s\n",
                                 CONFD_GET_CBUFPTR(&qr->results[i].tv[j].v));
            }
        }
        maapi_query_free_result(qr);
    } while (n > 0);
    maapi_query_stop(maapisock, qh);
    maapi_detach2(maapisock, uinfo->actx.thandle);

term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

/**
 * Convert timeout string to int (seconds)
 * @param timeout ... string with timout (number) or NULL
 * @param ret ... value to return (if timout is NULL, fill with default 600)
 * @return CONFD_OK or CONFD_ERR
 */
int timeout_to_int(const char * timeout, int *ret)
{
    TRACE_ENTER("timeout=%s", timeout);
    int rv = CONFD_OK;
    if (timeout) {
        *ret = atoi(timeout);
    } else {
        *ret = _DEFAULT_CONFIRMED_TIMEOUT;
        INFO("Default timeout used");
    }
    TRACE_EXIT("rv=%i *ret=%d", rv, *ret);
    return rv;
}

/**
 * Start confirmed commit.
 * @param usid ... session id for maapi_cli_printf
 * @param id ... persist id for the commit or NULL
 * @param timeout ... timeout value is sec as string or NULL
 *                    (then default 600 is used)
 * @return CONFD_OK or confd error value
 */
int perform_maapi_candidate_confirmed_commit(const int usid, const char * id,
                                             const char * timeout)
{
    TRACE_ENTER("usid=%d, id=%s, timeout=%s", usid, id, timeout);
    int tim;
    int rv = CONFD_ERR;
    if (CONFD_OK != (rv = timeout_to_int(timeout, &tim))) {
        FATAL("Failed to resolve timeout to int timeout=%s", timeout);
        goto term;
    }
    TRACE("tim=%d", tim);
    if (CONFD_OK != (rv = maapi_candidate_confirmed_commit_persistent(
                         maapisock_commit, tim, id, NULL))) {
        FATAL("failed to perform confirmed commit, id=%s, timeout=%s tim=%d",
              id, timeout, tim);
    }
    maapi_cli_printf(maapisock_commit, usid, "Confirmed commit started!\n");
    if (id) {
        maapi_cli_printf(maapisock_commit, usid, "Persist: %s\n", id);
    }
    if (timeout) {
        maapi_cli_printf(maapisock_commit, usid, "Timeout: %ds\n", tim);
    }
term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

/**
 * Print to CLI status info if there is ongoing confirmed commit.
 * @param usid ... session id for maapi_cli_printf
 * @return CONFD_OK or CONFD_ERR
 * NOTE: maapi_confirmed_commit_in_progress return usid of ongoing commit
 *      (ConfD User Guide says 1)
 */
int perform_maapi_commit_status(const int usid)
{
    TRACE_ENTER("usid=%d", usid);
    int rv = CONFD_OK;
    int stat = maapi_confirmed_commit_in_progress(maapisock_commit);
    TRACE("stat=%d", stat);
    if (stat != 0) { //
        maapi_cli_printf(maapisock_commit, usid,
                         "Ongoing commit in progress!\n");
        maapi_cli_printf(maapisock_commit, usid, "Session id: %d\n", stat);

    } else {
        maapi_cli_printf(maapisock_commit, usid,
                         "No ongoing commit in progress!\n");
    }
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

/**
 * Copy candidate to running.
 * Optionally use persist id of ongoing commit operation.
 * @param usid ... session id for maapi_cli_printf
 * @param id ... persist id or NULL
 * @return CONFD_OK or confd error value
 */
int confirm_maapi_candidate_commit(const int usid, const char * id)
{
    TRACE_ENTER("usid=%d, id=%s", usid, id);
    int rv = CONFD_ERR;
    if (CONFD_OK !=
        (rv = maapi_candidate_commit_persistent(maapisock_commit, id))) {
        maapi_cli_printf(maapisock_commit, usid,
                         "Commit not confirmed! (Is persist id correct?)\n");
        WARN("Failed to confirm commit! usid=%i, rv=%i, id=%s confd_errno=%i",
             usid, rv, id, confd_errno);
        rv = CONFD_OK;
        goto term;
    }
    maapi_cli_printf(maapisock_commit, usid,
                     "Commit successfully confirmed!\n");
    if (id) {
        maapi_cli_printf(maapisock_commit, usid, "Persist id: %s\n", id);
    }

term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

/**
 * Abort ongoig commit operation
 * @param usid ... session id for maapi_cli_printf
 * @param id ... persist id for the commit or NULL
 * @return CONFD_OK or confd error value
 */
int perform_maapi_commit_abort(const int usid, const char * id)
{
    TRACE_ENTER("usid=%d, id=%s", usid, id);
    int rv = CONFD_ERR;
    if (CONFD_OK != (rv =maapi_candidate_abort_commit_persistent(
                         maapisock_commit,id))) {
        maapi_cli_printf(maapisock_commit, usid,
                         "Commit not aborted! (Is persist id correct?)\n");
        WARN("Failed to abort commit! usid=%d, rv=%i, id=%s, confd_errno=%d",
             usid, rv, id,confd_errno);
        rv = CONFD_OK;
        goto term;
    }
    maapi_cli_printf(maapisock_commit, usid, "Confirmed commit aborted!\n");
    if (id) {
        maapi_cli_printf(maapisock_commit, usid, "Persist id: %s\n", id);
    }
term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

/**
   according to argc_in number
   1 ... maapi_confirmed_commit
   2 ... maapi_confirmed_commit info|abort|confirm (see argv_in[1])
   3 ... maapi_confirmed_commit abort|confirm <id> (see argv_in[1])
   maapi_confirmed_commit timeout <val> (see argv_in[1])
   maapi_confirmed_commit persist <id> (see argv_in[1])
   5 ... maapi_confirmed_commit persist <id> timeout <val>
 */
int do_confirmed_commit(struct confd_user_info *uinfo, char *path,
                        int argc_in, char **argv_in)
{
    TRACE_ENTER("path %s argc_in=%d", path, argc_in);
    trace_args(uinfo->usid, argc_in, argv_in, path);
    int rv = CONFD_ERR;
    if (CONFD_OK != (rv = maapi_attach2(maapisock_commit, maapi_example__ns,
                                        uinfo->usid, uinfo->actx.thandle))) {
        FATAL("Failed to attach to maapi!");
        goto term;
    }
    switch (argc_in) {
    case 1:
        // start new commit without id and with default timeout
        rv = perform_maapi_candidate_confirmed_commit(uinfo->usid, NULL, NULL);
        break;
    case 2:
        if (!strcmp(argv_in[1], "status")) {
            // display commit info
            rv = perform_maapi_commit_status(uinfo->usid);
            break;
        }
        if (!strcmp(argv_in[1], "abort")) {
            // abort ongoing confirmed commit - without ID
            rv = perform_maapi_commit_abort(uinfo->usid, NULL);
            break;
        }
        if (!strcmp(argv_in[1], "confirm")) {
            // confirm ongoing confirmed commit - without ID
            rv = confirm_maapi_candidate_commit(uinfo->usid, NULL);
            break;
        }
        FATAL("Unexpected argv_in=%s value", argv_in[1]);
        break;
    case 3:
        if (!strcmp(argv_in[1], "abort")) {
            // abort ongoing confirmed commit - with ID
            rv = perform_maapi_commit_abort(uinfo->usid, argv_in[2]);
            break;
        }
        if (!strcmp(argv_in[1], "confirm")) {
            // confirm ongoing confirmed commit - with ID
            rv = confirm_maapi_candidate_commit(uinfo->usid, argv_in[2]);
            break;
        }
        if (!strcmp(argv_in[1], "timeout")) {
            // start new commit without id and with timeout
            rv = perform_maapi_candidate_confirmed_commit(uinfo->usid, NULL,
                                                          argv_in[2]);
            break;
        }
        if (!strcmp(argv_in[1], "persist")) {
            // start new commit with id and without timeout
            rv = perform_maapi_candidate_confirmed_commit(uinfo->usid,
                                                          argv_in[2], NULL);
            break;
        }
        FATAL("Unexpected argv_in=%s value", argv_in[1]);
        break;
    case 5:
        // start new commit with id and timeout
        rv = perform_maapi_candidate_confirmed_commit(uinfo->usid, argv_in[2],
                                                      argv_in[4]);
        break;
    default:
        FATAL("Unexpected argument count argc_in=%i (should be 1,2,3 or 5)",
              argc_in);
        break;
    }
    maapi_detach2(maapisock_commit, uinfo->actx.thandle);
term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

int update_start_log(void)
{
    INFO_ENTER("");
    int rv = CONFD_ERR;
    TRACE("Creating start-log record");
    struct confd_ip ip;
    ip.af = AF_INET;
    inet_pton(AF_INET, "127.0.0.1", &ip.ip.v4);
    static int tid;
    static char *user = "admin";
    static const char *groups[] = { "admin" };
    static enum confd_dbname dbname = CONFD_CANDIDATE;
    static char *context = "maapi";

    if (CONFD_OK
        != maapi_start_user_session(maapisock, user, context, groups, 1,
                                    &ip, CONFD_PROTO_TCP)) {
        FATAL("Failed to create user session!");
        goto term;
    }

    if (0 > (tid = maapi_start_trans(maapisock, dbname, CONFD_READ_WRITE))) {
        FATAL("failed to start trans");
        goto term;
    }

    confd_value_t current_time;
    struct confd_datetime dt;
    time_t rawtime;
    time(&rawtime);
    struct tm *tm = localtime(&rawtime);
    dt.year = tm->tm_year + 1900;
    dt.month = tm->tm_mon + 1;
    dt.day = tm->tm_mday;
    dt.hour = tm->tm_hour;
    dt.min = tm->tm_min;
    dt.sec = tm->tm_sec;
    dt.micro = 0;
    dt.timezone = CONFD_TIMEZONE_UNDEF;
    CONFD_SET_DATETIME(&current_time, dt);

    if (CONFD_OK != maapi_create(maapisock, tid, "%s{%x}",
                                 start_log_keyapth_string, &current_time)) {
        FATAL("Failed to create start log record!");
        goto term;
    }
    if (CONFD_OK != maapi_apply_trans(maapisock, tid, 0)) {
        FATAL("Failed to apply record transaction!");
        goto term;
    }
    if (CONFD_OK != maapi_finish_trans(maapisock, tid)) {
        FATAL("Failed to finish transaction!");
        goto term;
    }
    if (CONFD_OK != maapi_candidate_commit(maapisock)) {
        FATAL("Failed to commit log record transaction!");
        goto term;
    }

    rv = CONFD_OK;
term:
    INFO_EXIT("rv=%i", rv);
    return rv;
}

int init_confd_daemon(void)
{
    INFO_ENTER("");
    int rv = CONFD_ERR;
    struct sockaddr_in addr;
    int debuglevel = CONFD_DEBUG;

    /* initialize confd library */
    confd_init("maapi_daemon", stderr, debuglevel);

    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(CONFD_PORT);

    if (CONFD_OK
        != confd_load_schemas((struct sockaddr*) &addr,
                              sizeof(struct sockaddr_in))) {
        FATAL("Failed to load schemas from confd!");
        goto term;
    }

    if (NULL == (dctx = confd_init_daemon("maapi_example_daemon"))) {
        FATAL("Failed to initialize confdlib!");
        goto term;
    }

    /* Create the first control socket, all requests to */
    /* create new transactions arrive here */

    if (0 > (ctlsock = socket(PF_INET, SOCK_STREAM, 0))) {
        FATAL("Failed to open ctlsocket!");
        goto term;
    }
    if (0 > confd_connect(dctx, ctlsock, CONTROL_SOCKET,
                          (struct sockaddr*) &addr,
                          sizeof(struct sockaddr_in))) {
        FATAL("Failed to confd_connect() ctlsock to confd!");
        goto term;
    }

    /* Also establish a workersocket, this is the most simple */
    /* case where we have just one ctlsock and one workersock */

    if (0 > (workersock = socket(PF_INET, SOCK_STREAM, 0))) {
        FATAL("Failed to open workersocket!");
        goto term;
    }
    if (0 > confd_connect(dctx, workersock, WORKER_SOCKET,
                          (struct sockaddr*) &addr,
                          sizeof(struct sockaddr_in))) {
        FATAL("Failed to confd_connect() workersock to confd!");
        goto term;
    }
    if (0 > (workersock_commit = socket(PF_INET, SOCK_STREAM, 0))) {
        FATAL("Failed to open commit workersocket!");
        goto term;
    }
    if (0 > confd_connect(dctx, workersock_commit, WORKER_SOCKET,
                          (struct sockaddr*) &addr,
                          sizeof(struct sockaddr_in))) {
        FATAL("Failed to confd_connect() workersock_commit to confd!");
        goto term;
    }
    /* Establish a MAAPI socket */
    if (0 > (maapisock = socket(PF_INET, SOCK_STREAM, 0))) {
        FATAL("Failed to open socket\n");
        goto term;
    }

    if (CONFD_OK
        != maapi_connect(maapisock, (struct sockaddr*) &addr,
                         sizeof(struct sockaddr_in))) {
        FATAL("Failed to confd_connect() to confd!");
        goto term;
    }
    /* Establish a MAAPI socket */
    if (0 > (maapisock_commit = socket(PF_INET, SOCK_STREAM, 0))) {
        FATAL("Failed to open socket\n");
        goto term;
    }

    if (CONFD_OK
        != maapi_connect(maapisock_commit, (struct sockaddr*) &addr,
                         sizeof(struct sockaddr_in))) {
        FATAL("Failed to confd_connect() to confd!");
        goto term;
    }

    struct confd_trans_cbs trans;
    memset(&trans, 0, sizeof(struct confd_trans_cbs));
    trans.init = s_init;
    trans.finish = s_finish;
    if (CONFD_OK != confd_register_trans_cb(dctx, &trans)) {
        FATAL("Failed to register trans cb!");
    }

    /* validation */
    struct confd_trans_validate_cbs vcb;
    memset(&vcb, 0, sizeof(struct confd_trans_validate_cbs));
    vcb.init = init_validation;
    vcb.stop = stop_validation;
    confd_register_trans_validate_cb(dctx, &vcb);
    struct confd_valpoint_cb val_logging;
    memset(&val_logging, 0, sizeof(struct confd_valpoint_cb));
    val_logging.validate = validate_vals;
    strcpy(val_logging.valpoint, maapi_example__validateid_val_items);
    if (CONFD_OK != confd_register_valpoint_cb(dctx, &val_logging)) {
        FATAL("Failed to register_validation point!");
        goto term;
    }

    /* data provider */
    struct confd_data_cbs data;
    memset(&data, 0, sizeof(struct confd_data_cbs));
    data.get_elem = get_elem;
    data.get_next = get_next;
    strcpy(data.callpoint, maapi_example__callpointid_items);
    if (CONFD_OK != confd_register_data_cb(dctx, &data)) {
        FATAL("Failed to register data provider callback!");
        goto term;
    }

    /* clispec commands */
    struct confd_action_cbs acb;
    memset(&acb, 0, sizeof(acb));
    acb.init = init_cmd;
    strncpy(acb.actionpoint, "start_count_cp", MAX_CALLPOINT_LEN);
    acb.command = do_start_count;
    if (CONFD_OK != confd_register_action_cbs(dctx, &acb)) {
        FATAL("Failed to register 'start-count' action callback!");
        goto term;
    }

    memset(&acb, 0, sizeof(acb));
    acb.init = init_cmd;
    strncpy(acb.actionpoint, "show_items_with_value_cp", MAX_CALLPOINT_LEN);
    acb.command = do_show_items;
    if (CONFD_OK != confd_register_action_cbs(dctx, &acb)) {
        FATAL("Failed to register 'show-items' action callback!");
        goto term;
    }

    memset(&acb, 0, sizeof(acb));
    acb.init = init_cmd;
    strncpy(acb.actionpoint, "show_items_with_smaller_than_value_cp",
            MAX_CALLPOINT_LEN);
    acb.command = do_show_items_smaller_than;
    if (CONFD_OK != confd_register_action_cbs(dctx, &acb)) {
        FATAL("Failed to register 'start-count' action callback!");
        goto term;
    }

    memset(&acb, 0, sizeof(acb));
    acb.init = init_cmd_commit; //has to run in different thread
    strncpy(acb.actionpoint, "start_confirmed_commit",
            MAX_CALLPOINT_LEN);
    acb.command = do_confirmed_commit;
    if (CONFD_OK != confd_register_action_cbs(dctx, &acb)) {
        FATAL("Failed to register 'start-count' action callback!");
        goto term;
    }

    if (CONFD_OK != confd_register_done(dctx)) {
        FATAL("Failed to complete registration!");
        goto term;
    }

    rv = CONFD_OK;
term:
    INFO_EXIT("Initialization complete rv=%i", rv);
    return rv;
}

void* commit_confd_loop(void *arg)
{
    INFO_ENTER("");
    while (1) {
        struct pollfd set[1];
        int ret;

        set[0].fd = workersock_commit;
        set[0].events = POLLIN;
        set[0].revents = 0;

        if (poll(set, sizeof(set) / sizeof(*set), -1) < 0) {
            perror("Poll failed:");
            continue;
        }

        TRACE("POLL event in commit_confd_loop");
        if (set[0].revents & POLLIN) {
            if ((ret = confd_fd_ready(dctx, workersock_commit)) == CONFD_EOF) {
                confd_fatal("Worker socket for commit closed\n");
            } else if (ret == CONFD_ERR && confd_errno != CONFD_ERR_EXTERNAL) {
                confd_fatal("Error on worker socket for commit request:"
                            " %s (%d): %s\n",
                            confd_strerror(confd_errno), confd_errno,
                            confd_lasterr());
            }
        }
    }

    INFO_EXIT("");
}

int confd_loop(void)
{
    INFO_ENTER("");
    int rv = CONFD_ERR;
    pthread_t commit_th;
    pthread_create(&commit_th, NULL, commit_confd_loop, NULL);

    while (1) {
        struct pollfd set[2];
        int ret;

        set[0].fd = ctlsock;
        set[0].events = POLLIN;
        set[0].revents = 0;

        set[1].fd = workersock;
        set[1].events = POLLIN;
        set[1].revents = 0;

        if (poll(set, sizeof(set) / sizeof(*set), -1) < 0) {
            perror("Poll failed:");
            continue;
        }

        /* Check for I/O */
        if (set[0].revents & POLLIN) {
            if ((ret = confd_fd_ready(dctx, ctlsock)) == CONFD_EOF) {
                confd_fatal("Control socket closed\n");
            } else if (ret == CONFD_ERR && confd_errno != CONFD_ERR_EXTERNAL) {
                confd_fatal("Error on control socket request: %s (%d): %s\n",
                            confd_strerror(confd_errno), confd_errno,
                            confd_lasterr());
            }
        }
        if (set[1].revents & POLLIN) {
            if ((ret = confd_fd_ready(dctx, workersock)) == CONFD_EOF) {
                confd_fatal("Worker socket closed\n");
            } else if (ret == CONFD_ERR && confd_errno != CONFD_ERR_EXTERNAL) {
                confd_fatal("Error on worker socket request: %s (%d): %s\n",
                            confd_strerror(confd_errno), confd_errno,
                            confd_lasterr());
            }
        }
    }

    INFO_EXIT("rv %i", rv);
    return rv;
}

int main(int argc, char *argv[])
{
    INFO_ENTER("");
    int rv = CONFD_ERR;
    if (CONFD_OK != (rv = init_confd_daemon())) {
        FATAL("Failed to initialize confd! Exiting");
    } else {
        if (CONFD_OK != (rv = update_start_log())) {
            FATAL("Failed to update start-log! Exiting");
        } else {
            rv = confd_loop();
        }
    }
    INFO_EXIT("rv=%i", rv);
    return rv;
}
