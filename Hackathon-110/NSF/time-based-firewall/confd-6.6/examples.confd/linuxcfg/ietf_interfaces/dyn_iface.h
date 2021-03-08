/*
 * Copyright 2014 Tail-F Systems AB Tail-F customers are permitted to
 * redistribute in binary form, with or without modification, for use
 * in customer products.
 */

#ifndef _DYN_IFACE_H_
#define _DYN_IFACE_H_

#define DYN_IF_CREATE 0x1
#define DYN_IF_DELETE 0x2
#define DYN_IF_STATUS 0x4
#define DYN_IF_ALL    0xFFFFFFFF

#define IFNAMELEN   (IFNAMSIZ + 1)

/* Initialize the dyn_iface thread */
void dyn_iface_init(void);

/* A callback to be used on state changes on an interface.
 *
 * The flags correspond to the ifr_flags, documented in netdevice(7). */
typedef void (dyn_callback) (int ifindex, char *ifname,
                             int change, int flags);

/* Register to get callbacks on all new interfaces.
 *
 * The mask should be logical or of DYN_IF_XXXX-flags and specify
 * which events to subscribe to.
 *
 * On subscription the callback will be called for each interface that
 * is already known at the time.
 */
int dyn_iface_subscribe(dyn_callback *cb, int mask);


#endif
