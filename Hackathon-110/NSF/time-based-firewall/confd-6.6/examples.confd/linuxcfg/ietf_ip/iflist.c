/*
 * Copyright 2014 Tail-F Systems AB
 * Tail-F customers are permitted to redistribute in binary form, with
 * or without modification, for use in customer products.
 */

#include <malloc.h>
#include <string.h>

#include <confd.h>
#include <confd_maapi.h>
#include <confd_cdb.h>

#include "linuxcfg_api.h"
#include "linuxcfg_util.h"

#include "iflist.h"

static struct ifstate *ifstate_root = NULL;

/* Free a linked list of pairs */
void free_list(struct keyval *ip) {
    struct keyval *next;

    while (ip != NULL) {
        XFREE(ip->key);
        XFREE(ip->val);
        next = ip->next;
        XFREE(ip);
        ip = next;
    }
}

/* Helper that does the actual clearing, including deallocation of
 * allocated lists. */
void _clear_ifstate(struct ifstate *ifs) {

    ifs->state       = NOT_SET;

    /* IPv4 variables */
    ifs->ipv4enabled    = NOT_SET;
    ifs->ipv4forwarding = NOT_SET;
    ifs->ipv4mtu        = NOT_SET;

    free_list(ifs->ipv4addrlist);
    ifs->ipv4addrlist       = NULL;

    free_list(ifs->ipv4neighlist);
    ifs->ipv4neighlist  = NULL;

    /* IPv6 variables */
    ifs->ipv6dad_transmits  = NOT_SET;
    ifs->ipv6create_global_addresses = NOT_SET;
    ifs->ipv6enabled        = NOT_SET;
    ifs->ipv6forwarding     = NOT_SET;
    ifs->ipv6mtu            = NOT_SET;
    ifs->ipv6use_tempaddr   = NOT_SET;
    ifs->ipv6tmp_valid_life = NOT_SET;
    ifs->ipv6tmp_pref_life  = NOT_SET;

    free_list(ifs->ipv6addrlist);
    ifs->ipv6addrlist       = NULL;
    free_list(ifs->ipv6neighlist);
    ifs->ipv6neighlist      = NULL;

}

/* Look up an ifstate record by ifname */
struct ifstate *get_ifstate(char *ifname) {
    struct ifstate *ifs;

    for (ifs = ifstate_root; ifs != NULL; ifs = ifs->next) {
        if (strcmp(ifs->ifname, ifname) == 0)
            return ifs;
    }

    /* Allocate and initialize a new record. */
    ifs = xmalloc(sizeof(struct ifstate));

    ifs->ifname   = strdup(ifname);
    ifs->ipv4addrlist = NULL;
    ifs->ipv4neighlist = NULL;
    ifs->ipv6addrlist = NULL;
    ifs->ipv6neighlist = NULL;

    _clear_ifstate(ifs);


    ifs->next    = ifstate_root;
    ifstate_root = ifs;

    return ifs;

}

/* Clear all recorded data for an interface */
void clear_ifstate(char *ifname) {
    struct ifstate *ifs = get_ifstate(ifname);
    _clear_ifstate(ifs);
}

/* Find a key in a keyval list */
struct keyval *find_list(struct keyval *list, char *key) {

    for(; list != NULL; list = list->next) {
        if (strcmp(list->key, key) == 0) {
            return list;
        }
    }

    return NULL;
}
