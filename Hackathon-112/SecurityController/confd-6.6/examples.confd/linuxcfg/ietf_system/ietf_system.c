/*
 * Copyright 2014 Tail-F Systems AB
 * Tail-F customers are permitted to redistribute in binary form, with
 * or without modification, for use in customer products.
 */

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/sysinfo.h>
#include <sys/types.h>
#include <sys/utsname.h>

#include <confd.h>
#include <confd_maapi.h>
#include <confd_cdb.h>

#include "ietf-system.h"
#include "linuxcfg_api.h"
#include "linuxcfg_util.h"

#include "tz.h"
#include "ntp.h"
#include "dns.h"

static void setup(struct confd_daemon_ctx *dctx, int rsock);
static void handle_update(int rsock, int ssock, int spoint);

/* Handler for the system-shutdown rpc */
static int system_shutdown(struct confd_user_info *uinfo,
                           struct xml_tag *name,
                           confd_hkeypath_t *kp,
                           confd_tag_value_t *params,
                           int nparams);
/* Handler for the system-restart  rpc */
static int system_restart(struct confd_user_info *uinfo,
                          struct xml_tag *name,
                          confd_hkeypath_t *kp,
                          confd_tag_value_t *params,
                          int nparams);

/* The primary and fallback paths to use in the transformation
   callbacks */
#define CONTACT_FIRSTLOC   "/SNMPv2-MIB/system/sysContact"
#define CONTACT_SECONDLOC  "/system/secretcontact"
#define LOCATION_FIRSTLOC  "/SNMPv2-MIB/system/sysLocation"
#define LOCATION_SECONDLOC "/system/secretlocation"

/* Validation points */
static struct confd_valpoint_cb valpoints[] = {
    {"system_tz", tz_validate},
    {"", NULL}
};

/* Action points */
static struct confd_action_cbs actionpoints[] = {
    {.actionpoint = "comp_sys_tz",     .completion = tz_complete,
     .init=NULL },
    {.actionpoint = "system_setdt",    .action     = ntp_setdt,
     .init=NULL },
    {.actionpoint = "system_restart",  .action     = system_restart,
     .init=NULL },
    {.actionpoint = "system_shutdown", .action     = system_shutdown,
     .init=NULL },

    {.actionpoint = ""}
};

/* Transformation handlers */
static int trans_get_elem(struct confd_trans_ctx *tctx,
                          confd_hkeypath_t *kp);
static int trans_set_elem(struct confd_trans_ctx *tctx,
                          confd_hkeypath_t *kp,
                          confd_value_t *newval);
static int trans_remove(struct confd_trans_ctx *tctx,
                        confd_hkeypath_t *kp);

/* Data providers for /system-state */
static int dp_get_elem(struct confd_trans_ctx *tctx,
                       confd_hkeypath_t *kp);


static struct confd_data_cbs transpoints[] = {
    {.callpoint = "transcontact",
     .get_elem = trans_get_elem, .set_elem = trans_set_elem,
     .remove = trans_remove },
    { .callpoint = "system_dp",
      .get_elem = dp_get_elem },
    { .callpoint = "\0" }
};

static struct external_cmd hostname_cmd = {
    .descr_name = "hostname",
    .cmd = NULL,
    .args = "%s",
    .env_var = NULL,
#ifdef HOSTNAME_CMD
    .cc_macro = HOSTNAME_CMD,
#else
    .cc_macro = NULL,
#endif
    .binary_name = "hostname",
    .paths = { NULL }
};

static struct external_cmd shutdown_cmd = {
    .descr_name = "shutdown",
    .cmd = NULL,
    .args = "%s",
    .env_var = NULL,
#ifdef HOSTNAME_CMD
    .cc_macro = HOSTNAME_CMD,
#else
    .cc_macro = NULL,
#endif
    .binary_name = "shutdown",
    .paths = { NULL }
};

/* The component declaration that provides the hooks for linuxcfg */
const struct component ietf_system = {
    NULL,         /* init */
    NULL,         /* setup0 */
    setup,        /* setup */
    valpoints,    /* valpoints */
    transpoints,  /* transpoints */
    actionpoints, /* actionpoints */
    NULL,         /* init_validation */
    NULL,         /* stop_validation */
    NULL,         /* init_data */
    NULL,         /* finish_data */
};

/* Setup this component on startup */
static void setup(struct confd_daemon_ctx *dctx, int rsock)
{
    /* Subscribe to all changes */
    subscribe(NORMAL_PRIO, handle_update, "system/");
}

/* Handles an update to system/hostname */
static enum confd_iter_ret
hostname_handle(confd_hkeypath_t *kp, confd_value_t *val)
{
    char *hostname = (char *) CONFD_GET_BUFPTR(val);

    if (val == NULL) {
        LOG("Deleted hostname.");
        /* TODO: Default hostname? */
        return ITER_CONTINUE;
    }

    run(NULL, 0, GET_EXT_CMD(hostname_cmd), hostname);

    return ITER_CONTINUE;
}

/* This is the central iterator function, it will be called once for
 * each change to the /system subtree. */
static enum cdb_iter_ret iter(confd_hkeypath_t *kp,
                              enum cdb_iter_op op,
                              confd_value_t *oldv,
                              confd_value_t *newv,
                              void *state)
{
    int tag;
    int idx;
    int rsock  = *((int *) state);
    enum cdb_iter_ret status = ITER_CONTINUE;

    LOG("keypath: %s.", KPSTR(kp));

    /* Top level entity */
    idx = kp->len - 1;
    tag = CONFD_GET_XMLTAG(&kp->v[idx][0]);

    if (tag == sys_system) {
        idx = idx - 1;
        tag = CONFD_GET_XMLTAG(&kp->v[idx][0]);

        /* system/clock/ */
        if (tag == sys_clock) {
            kp->len = idx;
            status = tz_handle(kp, newv);
        }

        /* system/hostname */
        if (tag == sys_hostname) {
            kp->len = idx;
            status = hostname_handle(kp, newv);
        }

        /* system/ntp */
        if (tag == sys_ntp) {
            kp->len = idx;
            status = ntp_handle(kp, newv, op, rsock);
        }

        /* system/dns-resolver */
        if (tag == sys_dns_resolver) {
            kp->len = idx;
            status  = dns_handle(kp, newv, op, rsock);
        }

    }

    return status;
}

/* This function is called on every update to the subscription. */
static void handle_update(int rsock, int ssock, int spoint)
{
    LOG("rsock: %d, spoint: %d.", rsock, spoint);

    CHECK2(cdb_diff_iterate(ssock, spoint, iter, 0, &rsock),
           "Failed to call cdb_diff_iterate.");

    /* Perform any pending updates. */
    ntp_finish();
    dns_finish();
}

static char *firstLoc, *secondLoc;

static int set_trans_loc(int tag)  {
    switch (tag) {
    case sys_contact:
        firstLoc  = CONTACT_FIRSTLOC;
        secondLoc = CONTACT_SECONDLOC;
        break;
    case sys_location:
        firstLoc  = LOCATION_FIRSTLOC;
        secondLoc = LOCATION_SECONDLOC;
        break;
    default:
        warn("Unknown tag: %d", tag);
        return CONFD_ERR;
    }

    return CONFD_OK;
}

/* This transformation attempts to first use firstLoc and if that
 * fails use secondLoc.
 *
 * This allows us to coordinate with SNMPv2 if present and use our
 * own storage if not.
 */
static int trans_get_elem(struct confd_trans_ctx *tctx,
                          confd_hkeypath_t *kp) {
    int tag;
    int result;
    confd_value_t v;
    int maapi_socket = get_msock_from_opaq(tctx->t_opaque);

    LOG("%s.", KPSTR(kp));

    tag = CONFD_GET_XMLTAG(&kp->v[kp->len-2][0]);
    if (set_trans_loc(tag) != CONFD_OK) {
        confd_data_reply_not_found(tctx);
        return CONFD_ERR;
    }

    result = maapi_get_elem(maapi_socket, tctx->thandle, &v,
                            firstLoc);
    if ((result != CONFD_OK) && (confd_errno == CONFD_ERR_BADPATH)) {
        LOG("Trying %s.", secondLoc);
        result = maapi_get_elem(maapi_socket, tctx->thandle, &v,
                                secondLoc);
    }
    if ((result != CONFD_OK) && (confd_errno == CONFD_ERR_NOEXISTS)) {
            confd_data_reply_not_found(tctx);
            return CONFD_OK;
    }

    if (result != CONFD_OK) {
        error("Problem: %d.", confd_errno);
        return CONFD_ERR;
    }

    confd_data_reply_value(tctx, &v);
    free(CONFD_GET_BUFPTR(&v));

    return CONFD_OK;
}

static int trans_set_elem(struct confd_trans_ctx *tctx,
                          confd_hkeypath_t *kp,
                          confd_value_t *newval) {
    int tag;
    int maapi_socket = get_msock_from_opaq(tctx->t_opaque);
    int result;

    LOG("%s.", KPSTR(kp));

    tag = CONFD_GET_XMLTAG(&kp->v[kp->len-2][0]);
    CHECK(set_trans_loc(tag),
          "Failed to find model locations.");

    result = maapi_set_elem(maapi_socket, tctx->thandle, newval,
                            firstLoc);
    if ((result != CONFD_OK) && (confd_errno == CONFD_ERR_BADPATH)) {
        LOG("Trying %s.", secondLoc);
        result = maapi_set_elem(maapi_socket, tctx->thandle, newval,
                                secondLoc);
    }
    if ((result != CONFD_OK) && (confd_errno == CONFD_ERR_NOEXISTS)) {
            confd_data_reply_not_found(tctx);
            return CONFD_OK;
    }

    if (result != CONFD_OK) {
        error("Problem: %d.", confd_errno);
        return CONFD_ERR;
    }

    return CONFD_OK;
}

static int trans_remove(struct confd_trans_ctx *tctx,
                        confd_hkeypath_t *kp) {
    int tag, result;
    int maapi_socket = get_msock_from_opaq(tctx->t_opaque);

    tag = CONFD_GET_XMLTAG(&kp->v[kp->len-2][0]);
    CHECK(set_trans_loc(tag),
          "Failed to find model locations.");


    result = maapi_delete(maapi_socket, tctx->thandle, firstLoc);

    if ((result != CONFD_OK) && (confd_errno == CONFD_ERR_BADPATH)) {
        info("Trying %s.", secondLoc);
        result = maapi_delete(maapi_socket, tctx->thandle, secondLoc);
    }

    if ((result != CONFD_OK) && (confd_errno == CONFD_ERR_NOEXISTS)) {
            confd_data_reply_not_found(tctx);
            return CONFD_OK;
    }

    if (result != CONFD_OK) {
        error("Problem: %d.", confd_errno);
        return CONFD_ERR;
    }

    return CONFD_OK;
}

static int dp_get_elem(struct confd_trans_ctx *tctx,
                       confd_hkeypath_t *kp) {
    confd_value_t val;
    time_t  t;
    struct confd_datetime dt;
    struct sysinfo sys_info;

    int idx = kp->len - 1;
    int tag = CONFD_GET_XMLTAG(&kp->v[idx][0]);

    LOG("kp: %s", KPSTR(kp));

    /* /system-state */
    if (tag == sys_system_state) {
        idx = idx - 1;
        tag = CONFD_GET_XMLTAG(&kp->v[idx][0]);

        /* /system-state/clock:
         *  Common code for both branches, either the current time
         *  or the boot time.
         */
        if (tag == sys_clock) {
            idx = idx - 1;
            tag = CONFD_GET_XMLTAG(&kp->v[idx][0]);

            time(&t);

            /* system-state/clock/current-datetime */
            if (tag == sys_current_datetime) {
                LOG("current-datetime");
            }

            /* system-state/clock/boot-datetime */
            if (tag == sys_boot_datetime) {
                LOG("boot-datetime");
                sysinfo(&sys_info);
                t = t - sys_info.uptime;
            }

            if (time_to_dt(t, &dt) == CONFD_OK) {
                CONFD_SET_DATETIME(&val, dt);
                confd_data_reply_value(tctx, &val);

                return CONFD_OK;
            }
        }

        /* system-state/platform */
        if (tag == sys_platform) {
            char *value = NULL;
            struct utsname uts;

            idx = idx - 1;
            tag = CONFD_GET_XMLTAG(&kp->v[idx][0]);

            if (uname(&uts) != 0) {
                error("Calling uname failed: %s", strerror(errno));
                NOT_FOUND;
            }

            /* system-state/platform/os-name */
            if (tag == sys_os_name) {
#ifdef SYSTEM_OS_NAME
                value = SYSTEM_SYSNAME;
#else
                value = uts.sysname;
#endif
            }

            /* system-state/platform/os-version */
            if (tag == sys_os_version) {
#ifdef SYSTEM_OS_VERSION
                value = SYSTEM_OS_VERSION;
#else
                value = uts.version;
#endif
            }

            /* system-state/platform/os-release */
            if (tag == sys_os_release) {
#ifdef SYSTEM_OS_RELEASE
                value = SYSTEM_OS_RELEASE;
#else
                value = uts.release;
#endif
            }

            /* system-state/platform/machine */
            if (tag == sys_machine) {
#ifdef SYSTEM_MACHINE
                value = SYSTEM_MACHINE;
#else
                value = uts.machine;
#endif
            }

            if (value != NULL) {
                CONFD_SET_STR(&val, value);
                confd_data_reply_value(tctx, &val);
                return CONFD_OK;
            }
        }

    }

    confd_data_reply_not_found(tctx);

    return CONFD_OK;
}
/* Handler for the system-shutdown rpc */
static int system_shutdown(struct confd_user_info *uinfo,
                           struct xml_tag *name,
                           confd_hkeypath_t *kp,
                           confd_tag_value_t *params,
                           int nparams) {

    warn("Ordered shutdown!");

    run(NULL, 0, GET_EXT_CMD(shutdown_cmd), "-h now");

    return CONFD_OK;
}

/* Handler for the system-restart  rpc */
static int system_restart(struct confd_user_info *uinfo,
                          struct xml_tag *name,
                          confd_hkeypath_t *kp,
                          confd_tag_value_t *params,
                          int nparams) {

    warn("Ordered restart");

    run(NULL, 0, GET_EXT_CMD(shutdown_cmd), "-r now");

    return CONFD_OK;
}
