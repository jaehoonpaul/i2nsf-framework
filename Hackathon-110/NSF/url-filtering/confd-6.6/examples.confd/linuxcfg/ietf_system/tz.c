/*
 * Copyright 2014 Tail-F Systems AB Tail-F customers are permitted to
 * redistribute in binary form, with or without modification, for use
 * in customer products.
 */
#include <confd.h>
#include <confd_maapi.h>
#include <confd_cdb.h>
#include <confd_dp.h>

#include <errno.h>
#include <fts.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "linuxcfg_api.h"
#include "linuxcfg_util.h"
#include "ietf-system.h"
#include "tz.h"

/*
 * Configuration settings
 */
static const char tzroot[]      = "/usr/share/zoneinfo";
static const char tzlocaltime[] = "/etc/localtime";
static const char tztimezone[]  = "/etc/timezone";


#define TZLIST_NAME_SZ 512
struct tzlist {
    char name[TZLIST_NAME_SZ];
    struct tzlist *next;
};

/* How to copy files */
static struct external_cmd cp_cmd = {
    .descr_name = "cp",
    .cmd = NULL,
    .args = "%s %s",
    .env_var = NULL,
#ifdef CP_CMD
    .cc_macro = CP_CMD,
#else
    .cc_macro = NULL,
#endif
    .binary_name = "cp",
    .paths = { NULL }
};

/* zone info compiler (zic) */
static struct external_cmd zic_cmd = {
    .descr_name = "zic",
    .cmd = NULL,
    .args = "-d " TMPDIR " %s",
    .env_var = NULL,
#ifdef ZIC_CMD
    .cc_macro = ZIC_CMD,
#else
    .cc_macro = NULL,
#endif
    .binary_name = "zic",
    .paths = { NULL }
};

/* Writes /etc/timezone */
static int write_timezone(char *tzname) {
    FILE *fp = fopen(tztimezone, "w");

    if (fp == NULL) {
        error("Failed to open %s, errocode: %d",
              tztimezone, errno);
        return CONFD_ERR;
    }

    fprintf(fp, "%s\n", tzname);

    fclose(fp);

    return CONFD_OK;
}

/* Makes and compiles a custom timezone */
static int make_timezone(int tz_ofs) {
    char filename[] = TMPDIR "/tzXXXXXX";
    char content[512];
    int  contentlen;
    int  hours, minutes;

    int  fd = mkstemp(filename);

    if (fd == 0) {
        error("Failed to open %s, errocode: %d",
              filename, errno);
        return CONFD_ERR;
    }

    hours   = abs(tz_ofs / 60);
    minutes = abs(tz_ofs % 60);

    /* Write a custom zone info file */
    contentlen = snprintf(content, 512, "Zone CUSTOM %s%d:%d - Custom\n",
                          (tz_ofs < 0 ? "-" : "+"), hours, minutes);


    if (write(fd, content, contentlen) != contentlen) {
        error("Failed to write to tz file %s, errocode: %d",
              filename, errno);
        return CONFD_ERR;
    }

    /* Compile the file and copy it into the correct place */
    run(NULL, 0, GET_EXT_CMD(zic_cmd),
        filename);

    run(NULL, 0, GET_EXT_CMD(cp_cmd),
        TMPDIR "/CUSTOM",
        tzlocaltime);

    return CONFD_OK;
}

/* Handles an update to system/clock */
enum confd_iter_ret tz_handle(confd_hkeypath_t *kp,
                              confd_value_t *val)
{
    int  tag;
    int  tz_ofs;
    char buf[512];

    LOG("%s.", KPSTR(kp));

    if (val == NULL) {
        LOG("Deleted.");
        /* TODO: Default timezone? UTC? */

        return ITER_CONTINUE;
    }

    tag = CONFD_GET_XMLTAG(&kp->v[kp->len-1][0]);

    if (tag == sys_timezone_name) {

        LOG("Setting timezone_name: %s.", CONFD_GET_BUFPTR(val));

        /* Copy the file corresponding to the timezone into place */
        snprintf(buf, 512, "%s/%s", tzroot, CONFD_GET_BUFPTR(val));
        if (run(NULL, 0, GET_EXT_CMD(cp_cmd),
                buf,
                tzlocaltime) == 0) {
            write_timezone((char *) CONFD_GET_BUFPTR(val));
        }
    } else if (tag == sys_timezone_utc_offset) {

        tz_ofs = CONFD_GET_INT16(val);
        LOG("Setting timezone offset: %d.", tz_ofs);

        make_timezone(tz_ofs);
        write_timezone("Custom");

    } else {
        error("Unknown keypath: %s", KPSTR(kp));
        return ITER_CONTINUE;
    }

    return ITER_CONTINUE;
}

int tz_validate(struct confd_trans_ctx *tctx,
                confd_hkeypath_t *keypath,
                confd_value_t *newval)
{
    // TODO: Timezone validation.

    return CONFD_OK;
}

/* Lists all the actual zones available in tzroot.
 *
 * An alternative to traversing the file structure would be to read
 * the zone.tab file, but that only lists the formal names, not
 * certain practical/common abbreviations such as UTC, GMT and EST.
 */
static int gather_timezones(struct tzlist **list, int *num)
{
    struct tzlist *item;
    FTS *fts;
    FTSENT *ftse;
    char *pathlist[] = {".", NULL};
    char *oldpath = getcwd(NULL, 0);

    if (chdir(tzroot) == -1) {
        error("chdir failed, errno: %d", errno);
        return CONFD_ERR;
    }

    fts = fts_open(pathlist, FTS_NOSTAT, NULL);
    if (fts == NULL) {
        error("fts_open failed, no timezones? errno: %d", errno);
        return CONFD_OK;
    }

    while ( (ftse = fts_read(fts)) != NULL) {

        /* Only visit files with an initial capital letter */
        if ((ftse->fts_name[0] < 'A') || (ftse->fts_name[0] > 'Z'))
            continue;
        /* Only allow prefixes with an initial capital letter */
        if ((ftse->fts_path[2] < 'A') || (fts->fts_path[2] > 'Z'))
            continue;

        /* Add files and symbolic links to the list */
        if ((ftse->fts_info == FTS_F) || (ftse->fts_info == FTS_SL)) {
            item = malloc(sizeof(struct tzlist));
            snprintf(item->name, TZLIST_NAME_SZ, "%s",
                     ftse->fts_path+2);
            item->next = *list;
            *list = item;
            *num = *num + 1;
        }

    }

    fts_close(fts);

    if (chdir(oldpath) == -1) {
        error("chdir failed, errno: %d", errno);
        return CONFD_ERR;
    }

    return CONFD_OK;
}

int tz_complete(struct confd_user_info *uinfo,
                int cli_style, char *token, int completion_char,
                confd_hkeypath_t *kp,
                char *cmdpath, char *cmdparam_id,
                struct confd_qname *simpleType, char *extra)
{
    struct tzlist *tzl = NULL, *oldp;
    int    numtz = 0;
    int    ix    = 0;
    struct confd_completion_value *values;


    gather_timezones(&tzl, &numtz);

    LOG("Number of timezones: %d.", numtz);

    /* Go through the list of timezones and add them to the value
     * array. */
    values = malloc( sizeof(struct confd_completion_value) * numtz);

    while (tzl != NULL) {
        values[ix].type  = CONFD_COMPLETION;
        values[ix].value = strdup(tzl->name);
        values[ix].extra = NULL;

        if (ix >= numtz) {
            error("Extra completions in list?");
            break;
        }

        ix   = ix + 1;
        oldp = tzl;
        tzl  = tzl->next;
        free(oldp);
    }

    CHECK(confd_action_reply_completion(uinfo, values, numtz),
          "Failed to send completion reply.");

    /* Free memory */
    for (ix=0; ix<numtz; ix++)
        free(values[ix].value);
    free(values);

    return CONFD_OK;
}
