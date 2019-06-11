/*********************************************************************
 * ConfD Transformation callpoint example
 *
 * This is ConfD Sample Code.
 *
 * (C) 2017 Tail-f Systems
 * Permission to use this code as a starting point hereby granted
 *
 * See the README file for more information
 ********************************************************************/

#include <stdlib.h>

#include <confd_lib.h>
#include <traceh.h>

#include "common.h"

void OK(int rval)
{
    if (rval != CONFD_OK) {
        fprintf(stderr, "error not CONFD_OK: %d : %s \n",
                confd_errno, confd_lasterr());
        abort();
    }
}

void print_val(const char *prefix, const confd_value_t *val)
{
    if (NULL != val) {
        char buffer[TRANSFORM_BUFF_LEN];
        confd_pp_value(buffer, TRANSFORM_BUFF_LEN, val);
        TRACE("%s == %s (type %i)", prefix, buffer, val->type);
    } else {
        TRACE("%s == NULL", prefix);
    }
}

void print_path(const char *prefix, const confd_hkeypath_t *kp)
{
    char buffer[TRANSFORM_BUFF_LEN];
    confd_pp_kpath(buffer, TRANSFORM_BUFF_LEN, kp);
    TRACE("%s == %s", prefix, buffer);
}

struct global_vars glob;
