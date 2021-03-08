/*
 * Copyright 2014 Tail-F Systems AB Tail-F customers are permitted to
 * redistribute in binary form, with or without modification, for use
 * in customer products.
 */

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <confd.h>
#include <confd_maapi.h>
#include <confd_cdb.h>

#include "ietf-system.h"
#include "linuxcfg_api.h"
#include "linuxcfg_util.h"
#include "ntp.h"

/*
** Configuration settings
*/
#define NTP_CONFIG  "/etc/ntp/ntp-servers.conf"

/*
** Global variables
*/
enum ntp_state {
    ntp_stopped,
    ntp_running,
};

/* To keep track if we have to start, stop or restart ntpd */
static int ntp_state     = ntp_stopped;
static int ntp_new_state = ntp_stopped;
static int ntp_restart = 0;

static struct external_cmd ntpd_cmd = {
    .descr_name = "ntpd",
    .cmd = NULL,
    .args = "-g",
    .env_var = "NTPD_CMD",
#ifdef USER_NTPD_CMD
    .cc_macro = USER_NTPD_CMD,
#else
    .cc_macro = NULL,
#endif
    .binary_name = "ntpd",
    .paths = { "/usr/sbin/ntpd", "/usr/bin/ntpd", NULL }
};

#define NTPD_CMD        GET_EXT_CMD(ntpd_cmd)

/* Local prototypes */
static void restart_ntpd(void);
static void stop_ntpd(void);

/* Writes a single ntp server to the ntpd configuration file */
static int write_server(int sock, FILE *fd) {
    char *name;
    int name_len;
    int rv;
    confd_value_t addr;
    int association_type = 0;
    int iburst = 0;
    int prefer = 0;

    /* Gather all the parameters */
    CHECK(cdb_get_buf(sock, (unsigned char **)&name, &name_len,
                      "name"),
          "Failed to get server name");

    rv = cdb_get(sock, &addr, "udp/address");

    if (rv == CONFD_OK) {
        rv = cdb_get_enum_value(sock, &association_type,
                                "association-type");
    }

    if (rv == CONFD_OK) {
        rv = cdb_get_bool(sock, &iburst, "iburst");
    }

    if (rv == CONFD_OK) {
        rv = cdb_get_bool(sock, &prefer, "prefer");
    }

    /* Write to the config file */
    if (rv == CONFD_OK) {
        switch (association_type) {
        case sys_peer:
            fprintf(fd,"peer ");
            break;
        case sys_pool:
            fprintf(fd,"server ");
            // TODO: Clear up what this means.
            break;
        case sys_assoc_server:
            fprintf(fd, "server ");
            break;
        }

        fprintf(fd, "%s", VALSTR2(addr));

        if (iburst)
            fprintf(fd, " iburst");
        if (prefer)
            fprintf(fd, " prefer");

        fprintf(fd, "\n");
    }

    if (rv != CONFD_OK)
        error("Failed to gather ntpd parameters: %s (%d).",
              confd_lasterr(), confd_errno);

    XFREE(name);
    return rv;
}

/* Handles an update to system/ntp */
enum confd_iter_op ntp_handle(confd_hkeypath_t *kp,
                              confd_value_t *val,
                              enum cdb_iter_op op, int sock)
{
    int tag, len, i;
    FILE *fd;

    LOG("%s.", KPSTR(kp));

    /* No action for ntp/ */
    if (kp->len == 0)
        return ITER_RECURSE;

    tag = CONFD_GET_XMLTAG(&kp->v[kp->len-1][0]);

    if (tag == sys_enabled) {

        if ((op == MOP_DELETED) || !CONFD_GET_BOOL(val)) {
            LOG("Disabling ntp.");
            ntp_new_state = ntp_stopped;
        } else {
            LOG("Enabling ntp.");
            ntp_new_state = ntp_running;
        }
    }

    /* This is the list of ntp servers, we iterate through it */
    if (tag == sys_server) {

        len = cdb_num_instances(sock, "system/ntp/server");

        LOG("Num inst: %d.", len);

        /* Truncate the file */
        fd = fopen(NTP_CONFIG, "w");

        if (fd == NULL) {
            error("Failed to open %s, errorcode: %d",
                  NTP_CONFIG, errno);
            return CONFD_ERR;
        }

        for(i = 0; i < len; i++) {
            CHECK3(cdb_pushd(sock, "system/ntp/server[%d]", i),
                   "Pushd to system/ntp/server failed", ITER_STOP);

            CHECK3(write_server(sock, fd),
                   "Writing server data failed", ITER_STOP);

            cdb_popd(sock);
        }

        fclose(fd);
        ntp_restart = 1;

        /* We only need to do this once */
        return ITER_UP;
    }

    return ITER_CONTINUE;
}

/* Start/Stop/Restart ntpd as required to finish up the transaction */
int ntp_finish(void) {
    if (ntp_state != ntp_new_state) {
        if (ntp_new_state == ntp_stopped)
            stop_ntpd();
        if (ntp_new_state == ntp_running)
            restart_ntpd();
        ntp_state = ntp_new_state;
    }

    if ((ntp_restart) && (ntp_state == ntp_running))
        restart_ntpd();

    ntp_restart = 0;
    return CONFD_OK;
}


static void stop_ntpd(void) {
    /* ntpd doesn't restart on SIGHUP */
    /* furthermore, in some cases it may take a long time before the
       newly started daemon writes the pid file, which means that we
       can't actually rely on it for robustness in case of multiple
       restarts in quick succession - instead we kill by name */
    killall("ntpd", SIGTERM);
}

static void start_ntpd(void) {
    run(NULL, 0, NTPD_CMD);
}

static void restart_ntpd(void) {
    stop_ntpd();
    start_ntpd();
}

int ntp_setdt(struct confd_user_info *uinfo,
             struct xml_tag *name,
             confd_hkeypath_t *kp,
             confd_tag_value_t *params,
             int nparams)
{
    confd_value_t *val;
    struct confd_datetime dt;
    struct tm tm_time;
    time_t time_total;
    struct timeval tval;
    struct timezone tzval;

    val = CONFD_GET_TAG_VALUE(&params[0]);
    dt  = CONFD_GET_DATETIME(val);

    LOG("set-current-datetime: %s", VALSTR(val));
    LOG("dt.hour %i dt.min %i dt.sec %i dt.timezone %i dt.timezone_minutes %i",
            dt.hour, dt.min, dt.sec, dt.timezone, dt.timezone_minutes);

    if (ntp_state == ntp_running) {
        confd_action_seterr_extended(uinfo,
                                     CONFD_ERRCODE_APPLICATION,
                                     sys__ns, 0,
                                     "ntp-active");
        return CONFD_ERR;
    }

    /* Convert Yang time into C-time */
    memset(&tm_time, 0, sizeof(struct tm));
    tm_time.tm_year = dt.year - 1900;
    tm_time.tm_mon  = dt.month - 1;
    tm_time.tm_mday = dt.day;
    tm_time.tm_hour = dt.hour;
    tm_time.tm_min   = dt.min;
    tm_time.tm_sec   = dt.sec;
    tm_time.tm_isdst = -1;

    /* Convert into time_t and stuff into the right struct */
    LOG("Before mktime tm_time.tm_hour %i tm_time.tm_min %i tm_time.tm_sec %i",
            tm_time.tm_hour, tm_time.tm_min, tm_time.tm_sec);
    time_total = mktime(&tm_time);
    LOG("After mktime tm_time.tm_hour %i tm_time.tm_min %i tm_time.tm_sec %i",
            tm_time.tm_hour, tm_time.tm_min, tm_time.tm_sec);

    tval.tv_usec = 0;
    tval.tv_sec     = time_total;

    tzval.tz_minuteswest = dt.timezone * 60 + dt.timezone_minutes;
    tzval.tz_dsttime     = 0;
    (void) tzval;
    /* Set time */
    if (settimeofday(&tval, NULL) == -1) {
        // TODO: Error check
        error("settimeofday failed, errno: %d", errno);
        return CONFD_ERR;
    }

    return CONFD_OK;
}
