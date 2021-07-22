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

#include <confd_lib.h>

// implementation of the ".get_next" transform callback
int cb_get_next(
    struct confd_trans_ctx *tctx,
    confd_hkeypath_t *kp,
    long next
);

// transaction opaque memory handling
void *cb_get_next_alloc_opaque_data(void);
void cb_get_next_free_opaque_data(void * ptr);