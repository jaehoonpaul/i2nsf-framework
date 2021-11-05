/*
 * Copyright 2014 Tail-F Systems AB Tail-F customers are permitted to
 * redistribute in binary form, with or without modification, for use
 * in customer products.
 */

#ifndef _IPV6_H
#define _IPV6_H 1

enum cdb_iter_ret ipv6_iter(confd_hkeypath_t *kp,
                            enum cdb_iter_op op,
                            confd_value_t *oldv,
                            confd_value_t *newv,
                            void *state);

void apply_ipv6enabled(char *ifname, int val);

/* State handlers */
int ipv6_get_elem(struct confd_trans_ctx *tctx,
                  confd_hkeypath_t *kp);

int ipv6_get_next(struct confd_trans_ctx *tctx,
                  confd_hkeypath_t *kp,
                  long next);
#endif
