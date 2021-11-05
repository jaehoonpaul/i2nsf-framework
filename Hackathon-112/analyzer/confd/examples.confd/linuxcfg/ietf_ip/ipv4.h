/*
 * Copyright 2014 Tail-F Systems AB Tail-F customers are permitted to
 * redistribute in binary form, with or without modification, for use
 * in customer products.
 */

#ifndef _IPV4_H
#define _IPV4_H 1

enum cdb_iter_ret ipv4_iter(confd_hkeypath_t *kp,
                            enum cdb_iter_op op,
                            confd_value_t *oldv,
                            confd_value_t *newv,
                            void *state);

void apply_ipv4enabled(char *ifname, int val);

/* State handlers */
int ipv4_get_elem(struct confd_trans_ctx *tctx,
                  confd_hkeypath_t *kp);

int ipv4_get_next(struct confd_trans_ctx *tctx,
                  confd_hkeypath_t *kp,
                  long next);

int ipv4_get_case(struct confd_trans_ctx *tctx,
                  confd_hkeypath_t *kp, confd_value_t *choice);

#endif
