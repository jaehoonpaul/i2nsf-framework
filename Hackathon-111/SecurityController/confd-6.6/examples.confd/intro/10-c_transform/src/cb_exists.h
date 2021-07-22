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

// implementation of the ".exists_optional" transform callback
int cb_exists_optional(
    struct confd_trans_ctx *tctx,
    confd_hkeypath_t *kp
);