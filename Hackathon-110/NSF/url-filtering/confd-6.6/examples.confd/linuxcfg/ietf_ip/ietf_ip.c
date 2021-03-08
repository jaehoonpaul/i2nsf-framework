/*
 * Copyright 2014 Tail-F Systems AB
 * Tail-F customers are permitted to redistribute in binary form, with
 * or without modification, for use in customer products.
 */

#include <errno.h>
#include <fcntl.h>
#include <malloc.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>

#include <confd.h>
#include <confd_maapi.h>
#include <confd_cdb.h>

#include "ietf-ip.h"
#include "ietf-interfaces.h"
#include "iana-if-type.h"

#include "linuxcfg_api.h"
#include "linuxcfg_util.h"

/* Interfaces inside ietf_ip */
#include "iflist.h"
#include "ipv4.h"
#include "ipv6.h"

/* IETF Interfaces API */
#include "ietf_interfaces_api.h"

static void setup(struct confd_daemon_ctx *dctx, int rsock);
static void ifs_handler(char *ifname,
                        int   admin_state,
                        int   cdb_state,
                        int   if_state);

/* Interface state handling */
static void set_ifstate(char *ifname, int newstate);


static struct confd_data_cbs transpoints[] = {
    { .callpoint = "ip_state_ipv4", .get_elem = ipv4_get_elem,
      .exists_optional = ipv4_get_elem, .get_next = ipv4_get_next,
      .get_case = ipv4_get_case },
    { .callpoint = "ip_state_ipv6", .get_elem = ipv6_get_elem,
      .exists_optional = ipv6_get_elem, .get_next = ipv6_get_next },
    { .callpoint = "" }
};

/* The component declaration that provides the hooks for linuxcfg */
const struct component ietf_ip = {
    NULL,         /* init */
    NULL,         /* setup0 */
    setup,        /* setup */
    NULL,         /* valpoints */
    transpoints,  /* transpoints */
    NULL,         /* actionpoints */
    NULL,         /* init_validation */
    NULL,         /* stop_validation */
    NULL,         /* init_data */
    NULL,         /* finish_data */
};

/* Setup this component on startup */
static void setup(struct confd_daemon_ctx *dctx, int rsock)
{
    /* Subscribe to all changes  */
    if_register_sub_handler(ip_ipv4, ipv4_iter);
    if_register_sub_handler(ip_ipv6, ipv6_iter);
    if_register_if_handler(ifs_handler);
    LOG("ietf_ip started");
}

static void ifs_handler(char *ifname,
                        int admin_state,
                        int cdb_state,
                        int if_state) {
    int aggr_state;

    LOG("if: %s, admin: %d, cdb: %d, ifs: %d",
        ifname, admin_state, cdb_state, if_state);

    if (cdb_state == 0) {
      clear_ifstate(ifname);
    }

    aggr_state = admin_state & cdb_state & if_state;
    set_ifstate(ifname, aggr_state);
}

/*
** Interface state helpers and executors.
*/

static void set_ifstate(char *ifname, int newstate) {
    struct ifstate *ifs = get_ifstate(ifname);

    if (ifs->state != newstate) {
        /* When the interface comes up, push our settings */
        if (newstate == 1) {
            apply_ipv4enabled(ifname, ifs->ipv4enabled);
            apply_ipv6enabled(ifname, ifs->ipv6enabled);
        }

    }
    ifs->state = newstate;
}
