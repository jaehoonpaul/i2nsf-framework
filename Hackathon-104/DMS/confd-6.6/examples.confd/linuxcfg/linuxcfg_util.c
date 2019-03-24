/*
 * Copyright 2014 Tail-F Systems AB
 * Tail-F customers are permitted to redistribute in binary form, with
 * or without modification, for use in customer products.
 */

#include <errno.h>
#include <string.h>
#include <malloc.h>
#include <time.h>

#include <confd.h>

#include "linuxcfg_api.h"
#include "linuxcfg_util.h"

/* Formats a buffer of bytes in the octet string format XX:XX:XX.. */
char *physaddr2str(unsigned char *buf, int len) {
    const char hexdigits[] = "0123456789ABCDEF";

    int str_length = (2 * len) + len;
    char *str;
    int ix, strix;
    str = xmalloc(str_length);
    str[str_length - 1] = '\0';

    strix = 0;
    for(ix = 0; ix < len; ix++) {
        if (ix != 0) {
            str[strix++] = ':';
        }

        str[strix++] = hexdigits[buf[ix] / 16];
        str[strix++] = hexdigits[buf[ix] % 16];
    }

    return str;
}

/* Helper function that converts a time_t to a confd_datetime */
int time_to_dt(time_t t, struct confd_datetime *dt) {
    struct tm *tm_time;
    tm_time = localtime(&t);

    if ((tm_time != NULL) && (dt != NULL)) {
        dt->year   = tm_time->tm_year + 1900;
        dt->month  = tm_time->tm_mon + 1;
        dt->day    = tm_time->tm_mday;
        dt->hour   = tm_time->tm_hour;
        dt->min    = tm_time->tm_min;
        dt->sec    = tm_time->tm_sec;
        dt->micro  = 0;
        dt->timezone =  timezone / 3600;
        dt->timezone_minutes = (timezone % 3600) / 60;
        return CONFD_OK;
    } else {
        error("Failed to convert time.");
        return CONFD_ERR;
    }
}

int get_int32_file_cfg(const char *file, int32_t * value)
{
    FILE *fl;

    fl = fopen(file, "r");
    if (!fl) {
        error("\n%s - Failed fopen(\"%s\") : ", __FUNCTION__, file,
              strerror(errno));
        return -1;
    }
    if (fscanf(fl, "%d", value) != 1) {
        error("\n%s - Failed fscanf(\"%s\" ...)", __FUNCTION__, file);
        fclose(fl);
        return -1;
    }
    fclose(fl);
    return 0;
}
