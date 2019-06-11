/*
 * Copyright 2014 Tail-F Systems AB Tail-F customers are permitted to
 * redistribute in binary form, with or without modification, for use
 * in customer products.
 */

#ifndef _TZ_H_
#define _TZ_H_

/* Validates an update to system/clock */
int tz_validate(struct confd_trans_ctx *tctx,
                confd_hkeypath_t *keypath,
                confd_value_t *newval);

/* Handles an update to system/clock */
enum confd_iter_ret tz_handle(confd_hkeypath_t *kp,
                              confd_value_t *val);

/* CLI completion for timezones */
int tz_complete(struct confd_user_info *uinfo,
                int cli_style, char *token, int completion_char,
                confd_hkeypath_t *kp,
                char *cmdpath, char *cmdparam_id,
                struct confd_qname *simpleType, char *extra);

#endif
