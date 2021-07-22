/*
 * Copyright 2014 Tail-F Systems AB Tail-F customers are permitted to
 * redistribute in binary form, with or without modification, for use
 * in customer products.
 */

#ifndef _NTP_H_
#define _NTP_H_

/* Handle a value in the /system/ntp sub-tree */
enum confd_iter_op ntp_handle(confd_hkeypath_t *kp,
                              confd_value_t *val,
                              enum cdb_iter_op op, int sock);

/* Write the accumulated ntp config to file */
int ntp_finish(void);

/* Handler for the set-current-datetime rpc */
int ntp_setdt(struct confd_user_info *uinfo,
              struct xml_tag *name,
              confd_hkeypath_t *kp,
              confd_tag_value_t *params,
              int nparams);

#endif
