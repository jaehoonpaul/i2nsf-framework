/*
 * Copyright 2014 Tail-F Systems AB Tail-F customers are permitted to
 * redistribute in binary form, with or without modification, for use
 * in customer products.
 */

#ifndef _DNS_H_
#define _DNS_H_

/* Handle an update to the system/dns-resolver subtree */
enum confd_iter_op dns_handle(confd_hkeypath_t *kp,
                              confd_value_t *val,
                              enum cdb_iter_op op, int sock);

/* Write the accumulated dns config to file */
int dns_finish(void);

#endif
