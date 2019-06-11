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

// implementation of the ".get_case" transform callback
int cb_get_case(
    struct confd_trans_ctx *tctx,
    confd_hkeypath_t *kp,
    confd_value_t *choice
);