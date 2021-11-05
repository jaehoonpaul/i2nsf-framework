/*
 * Copyright 2014 Tail-F Systems AB Tail-F customers are permitted to
 * redistribute in binary form, with or without modification, for use
 * in customer products.
 */

#ifndef _IPMIBS_H_
#define _IPMIBS_H_

#include "ip.h"

/* External interface to the ipmibs module.
 * This allows the ietf_ip and ietf_interfaces modules to share
 * data with ipmibs, as mandated by the standards */

/* A get_next iterator that returns the next interface name */
int if_status_get_next_name(struct confd_trans_ctx *tctx,
                            confd_hkeypath_t * keypath, long next);

/* A get_next iterator that returns an ip address, either
 * ipv4 or ipv6 depending on the flag*/
int ip_address_get_next_name(struct confd_trans_ctx *tctx,
                             confd_hkeypath_t * kp, long next,
                             char *name, int ipv6);

/* Gets a specific ip entry, for a specific ip-address and a specific
   interface. Either ipv4 or ipv6 depending on the flag. */
ipAddressEntry_t *getIpEntry(char *ifName, void *ip, int ipv6);

/* A get_next iterator that returns an ip address, either
 * ipv4 or ipv6 depending on the flag.
 *
 * This one iterates over the neighbour table.
 */
int ip_net_phys_get_next_name(struct confd_trans_ctx *tctx,
                              confd_hkeypath_t * kp,
                              long next, char *ifname, int ipv6);

/* Gets a specific neighbour entry, for a specific ip-address and a specific
   interface. Either ipv4 or ipv6 depending on the flag. */
ip_net_phys_stats_t *getNeighEntry(char *ifname, void *ip, int ipv6);

#endif
