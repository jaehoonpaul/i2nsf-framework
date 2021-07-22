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

#include "cb_common.h"

#include <string.h>

#include "cb_get_elem.h"
#include "cb_get_next.h"
#include "cb_get_case.h"
#include "cb_exists.h"
#include "cb_set_elem.h"
#include "cb_create.h"
#include "cb_remove.h"

void * transform_alloc_opaque_data(void)
{
    return cb_get_next_alloc_opaque_data();
}

void transform_free_opaque_data(void *data)
{
    cb_get_next_free_opaque_data(data);
}

static struct confd_data_cbs data;

// register all the required transformation callbacks
struct confd_data_cbs * transform_cb(void)
{
    memset(&data, 0x00, sizeof(data));
    data.get_elem = cb_get_elem;
    data.get_next = cb_get_next;
    data.get_case = cb_get_case;
    data.exists_optional = cb_exists_optional;
    data.set_elem = cb_set_elem;
    data.create   = cb_create;
    data.remove   = cb_remove;
    strcpy(data.callpoint, "transcp");

    return &data;
}