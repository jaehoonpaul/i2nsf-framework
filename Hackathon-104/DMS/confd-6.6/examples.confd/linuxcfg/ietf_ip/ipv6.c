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
#include "ipv6.h"

#include "ietf-ip.h"
#include "ietf-interfaces.h"

/* Headers from the ipmibs component */

#include "if.h"
#include "ip.h"
#include "ipmibs.h"
#include "IP-MIB.h"


/* IPv6 helpers */
static void set_ipv6addr_prefix(char *ifname,
                                struct in6_addr addr,
                                int prefix);

/* Interface manipulation functions, IPv6 */
static void apply_ipv6create_global_addresses(char *ifname, int val);
static void apply_ipv6dad_transmits(char *ifname, int val);
static void apply_ipv6forwarding(char *ifname, int val);
static void apply_ipv6use_tempaddr(char *ifname, int val);
static void apply_ipv6tmp_pref_life(char *ifname, int val);
static void apply_ipv6tmp_valid_life(char *ifname, int val);
static void apply_ipv6mtu(char *ifname, int val);

static void add_ipv6addr(char *ifname, char *extra, char *addrstr);
static void delete_ipv6addr(char *ifname, char *extra,char *addrstr);

static void add_ipv6neigh(char *ifname, char *extra, char *addrstr);
static void delete_ipv6neigh(char *ifname, char *addrstr, char *extra);

static struct external_cmd ip_cmd = {
    .descr_name  = "ip",
    .cmd         = NULL,
    .args        = NULL,
    .env_var     = "IP_CMD",
#ifdef USER_IP_CMD
    .cc_macro    = USER_IP_CMD,
#else
    .cc_macro    = NULL,
#endif
    .binary_name = "ip",
    .paths       = { "/usr/sbin/ip", "/sbin/ip", NULL }
};

#define IP_CMD       GET_EXT_CMD(ip_cmd)

static struct external_cmd sysctl_cmd = {
    .descr_name  = "sysctl",
    .cmd         = NULL,
    .args        = NULL,
    .env_var     = "SYSCTL_CMD",
#ifdef USER_SYSCTL_CMD
    .cc_macro    = USER_SYSCTL_CMD,
#else
    .cc_macro    = NULL,
#endif
    .binary_name = "sysctl",
    .paths       = { "/usr/sbin/sysctl", "/sbin/sysctl", NULL }
};

#define SYSCTL_CMD       GET_EXT_CMD(sysctl_cmd)

DECLARE_SIMPLE_HANDLER(ipv6enabled,        NO_FLAG);
DECLARE_SIMPLE_HANDLER(ipv6mtu,            ipv6enabled);
DECLARE_SIMPLE_HANDLER(ipv6forwarding,     ipv6enabled);
DECLARE_SIMPLE_HANDLER(ipv6dad_transmits,  ipv6enabled);
DECLARE_SIMPLE_HANDLER(ipv6create_global_addresses,  ipv6enabled);
DECLARE_SIMPLE_HANDLER(ipv6tmp_pref_life,  ipv6enabled);
DECLARE_SIMPLE_HANDLER(ipv6tmp_valid_life, ipv6enabled);
DECLARE_SIMPLE_HANDLER(ipv6use_tempaddr,   ipv6enabled);
DECLARE_LIST_HANDLER(ipv6neigh,            ipv6enabled);
DECLARE_LIST_HANDLER(ipv6addr,             ipv6enabled);

static enum cdb_iter_ret handle_ipv6addr(int sock,
                                         char *ifname,
                                         struct in6_addr addr,
                                         enum cdb_iter_op op) {
    u_int8_t prefix;
    char     buf[512];

    inet_ntop(AF_INET6, &addr, buf, sizeof(buf));

    if (op == MOP_DELETED) {
        list_delete_ipv6addr(ifname, buf);
    } else {
        CHECK3(cdb_get_u_int8(sock, &prefix, "prefix-length"),
               "Getting prefix-length failed", ITER_STOP);
        set_ipv6addr_prefix(ifname, addr, prefix);
    }

    return ITER_CONTINUE;
}

static enum cdb_iter_ret handle_ipv6_neighbor(int sock,
                                              char *ifname,
                                              struct in6_addr addr,
                                              enum cdb_iter_op op) {
    unsigned char *buf;
    int   bufsz;
    char  *str;
    char  ipbuf[64];

    inet_ntop(AF_INET6, &addr, ipbuf, sizeof(ipbuf));

    if (op == MOP_DELETED) {
        list_delete_ipv6neigh(ifname, ipbuf);

    } else {
        cdb_get_binary(sock, &buf, &bufsz, "link-layer-address");
        str = physaddr2str(buf, bufsz);
        update_ipv6neigh(ifname, ipbuf, str);

        free(buf);
        free(str);
    }

    return ITER_CONTINUE;
}

/* This is the central ipv6 iterator function, it will be called once for
 * each change to the /interfaces/interface{X}/ipv6 subtree. */
enum cdb_iter_ret ipv6_iter(confd_hkeypath_t *kp,
                            enum cdb_iter_op op,
                            confd_value_t *oldv,
                            confd_value_t *newv,
                            void *state)
{
    int   tag, idx;
    int   rsock  = *((int *) state);
    char *ifname;
    struct in6_addr ip6;
    enum cdb_iter_ret status = ITER_CONTINUE;

    LOG("keypath: %s.", KPSTR(kp));

    /* Top level entity */
    idx = kp->len;
    tag = GET_NEXT_TAG;

    if (tag == if_interfaces) {
        tag = GET_NEXT_TAG;

        /* interfaces/interface{X} */
        if (tag == if_interface) {
            idx = idx - 1;
            ifname = (char *) CONFD_GET_BUFPTR(&kp->v[idx][0]);

            tag = GET_NEXT_TAG;

            /* interfaces/interface{X}/ipv6 */
            if (tag == ip_ipv6) {

                /* Top level object isn't interesting, descend */
                if (idx == 0) {
                    return ITER_RECURSE;
                }

                tag = GET_NEXT_TAG;
                switch (tag) {
                case ip_enabled:
                    set_ipv6enabled(ifname, CONFD_GET_BOOL(newv));
                    break;
                case ip_forwarding:
                    set_ipv6forwarding(ifname, CONFD_GET_BOOL(newv));
                    break;
                case ip_mtu:
                    set_ipv6mtu(ifname, CONFD_GET_UINT32(newv));
                    break;
                case ip_dup_addr_detect_transmits:
                    set_ipv6dad_transmits(ifname, CONFD_GET_UINT32(newv));
                    break;
                case ip_address:
                    idx = idx - 1;
                    ip6 = CONFD_GET_IPV6(&kp->v[idx][0]);

                    CHECK3(cdb_pushd(rsock, "%h", kp),
                           "Pushd to ipv6/address failed", ITER_STOP);

                    status = handle_ipv6addr(rsock, ifname, ip6, op);

                    cdb_popd(rsock);
                    break;
                case ip_neighbor:
                    idx = idx - 1;
                    ip6 = CONFD_GET_IPV6(&kp->v[idx][0]);

                    CHECK3(cdb_pushd(rsock, "%h", kp),
                           "Pushd to ipv6/neighbor failed", ITER_STOP);

                    status = handle_ipv6_neighbor(rsock, ifname, ip6, op);

                    cdb_popd(rsock);
                    break;
                case ip_autoconf:
                    tag = GET_NEXT_TAG;

                    switch (tag) {
                    case ip_create_global_addresses:
                        set_ipv6create_global_addresses(ifname,
                                                        CONFD_GET_BOOL(newv));
                        break;
                    case ip_create_temporary_addresses:
                        set_ipv6use_tempaddr(ifname, CONFD_GET_BOOL(newv));
                        break;
                    case ip_temporary_valid_lifetime:
                        set_ipv6tmp_valid_life(ifname, CONFD_GET_UINT32(newv));
                        break;
                    case ip_temporary_preferred_lifetime:
                        set_ipv6tmp_pref_life(ifname, CONFD_GET_UINT32(newv));
                        break;
                    default:
                        warn("Tag not handled! Path: %s.", KPSTR(kp));
                    }
                    break;
                default:
                    warn("Tag not handled! Path: %s.", KPSTR(kp));
                }
            } else {
                warn("Called with unexpected key path %s.", KPSTR(kp));
            }
        }
    }

    return status;
}


/*
** Utility functions for interface manipulations, IPv6
*/

void apply_ipv6enabled(char *ifname, int val) {
    struct ifstate  *ifs;
    struct keyval   *p;

    if (val == NOT_SET)
        return;

    if (val) {
        /* When ipv6 is enabled on an interface,
           push all our settings.  */
        ifs = get_ifstate(ifname);

        run(NULL, 0, "%s net.ipv6.conf.%s.disable_ipv6=0",
            SYSCTL_CMD, ifname);

        apply_ipv6mtu(ifname, ifs->ipv6mtu);
        apply_ipv6forwarding(ifname, ifs->ipv6forwarding);
        apply_ipv6dad_transmits(ifname, ifs->ipv6dad_transmits);
        apply_ipv6create_global_addresses(ifname,
                                          ifs->ipv6create_global_addresses);
        apply_ipv6use_tempaddr(ifname,
                               ifs->ipv6use_tempaddr);
        apply_ipv6tmp_pref_life(ifname,
                                ifs->ipv6tmp_pref_life);
        apply_ipv6tmp_valid_life(ifname,
                                ifs->ipv6tmp_valid_life);


        for(p = ifs->ipv6addrlist; p != NULL; p = p->next) {
            add_ipv6addr(ifname, NULL, p->val);
        }

        for(p = ifs->ipv6neighlist; p != NULL; p = p->next) {
            add_ipv6neigh(ifname, p->key, p->val);
        }

    } else {
        run(NULL, 0, "%s net.ipv6.conf.%s.disable_ipv6=1",
            SYSCTL_CMD, ifname);

        /* Flush ipv6, but not ipv4, if ipv6 is disabled. */
        run(NULL, 0, "%s -6 address flush dev %s", IP_CMD, ifname);

    }
}

static void apply_ipv6mtu(char *ifname, int val) {
    if (val == NOT_SET)
        return;

    run(NULL, 0, "%s -6 link set %s mtu %d", IP_CMD, ifname, val);
}

static void apply_ipv6forwarding(char *ifname, int val) {
    if (val == NOT_SET)
        return;

    run(NULL, 0, "%s net.ipv6.conf.%s.forwarding=%d",
        SYSCTL_CMD, ifname, val);
}

static void apply_ipv6dad_transmits(char *ifname, int val) {
    if (val == NOT_SET)
        return;

    run(NULL, 0, "%s net.ipv6.conf.%s.dad_transmits=%d",
        SYSCTL_CMD, ifname, val);
}

static void apply_ipv6create_global_addresses(char *ifname, int val) {
    if (val == NOT_SET)
        return;

    run(NULL, 0, "%s net.ipv6.conf.%s.autoconf=%d",
        SYSCTL_CMD, ifname, val);
}

static void apply_ipv6use_tempaddr(char *ifname, int val) {
    if (val == NOT_SET)
        return;

    run(NULL, 0, "%s net.ipv6.conf.%s.use_tempaddr=%d",
        SYSCTL_CMD, ifname, val);
}

static void apply_ipv6tmp_pref_life(char *ifname, int val) {
    if (val == NOT_SET)
        return;

    run(NULL, 0, "%s net.ipv6.conf.%s.temp_prefered_lft=%d",
        SYSCTL_CMD, ifname, val);
}

static void apply_ipv6tmp_valid_life(char *ifname, int val) {
    if (val == NOT_SET)
        return;

    run(NULL, 0, "%s net.ipv6.conf.%s.temp_valid_lft=%d",
        SYSCTL_CMD, ifname, val);
}

static void set_ipv6addr_prefix(char *ifname,
                                struct in6_addr addr,
                                int prefix) {
    char buf[64], addr_buf[64];

    inet_ntop(AF_INET6, &addr, addr_buf, sizeof(addr_buf));

    snprintf(buf, sizeof(buf), "%s/%d",
             addr_buf, prefix);

    update_ipv6addr(ifname, addr_buf, buf);
}

static void delete_ipv6addr(char *ifname, char *extra, char *addrstr) {
    run(NULL, 0, "%s -6 address del %s dev %s",
        IP_CMD, addrstr, ifname);
}

static void delete_ipv6neigh(char *ifname, char *addrstr, char *extra) {
    run(NULL, 0, "%s neigh del %s dev %s", IP_CMD, addrstr, ifname);
}

static void add_ipv6addr(char *ifname, char *extra, char *addrstr) {
    run(NULL, 0, "%s -6 address add %s dev %s",
        IP_CMD, addrstr, ifname);
}

static void add_ipv6neigh(char *ifname, char *ipbuf, char *str) {
        run(NULL, 0, "%s neigh replace %s lladdr %s dev %s",
            IP_CMD, ipbuf, str, ifname);
}

/*
** Operational data
*/

/* Handle /interfaces-state/interface{ifname}/ipv6/neighbor */
static int ipv6_get_elem_neighbor(struct confd_trans_ctx *tctx,
                                    confd_hkeypath_t *kp, char *ifname) {
    int                  idx;
    int                  tag;
    confd_value_t        v;
    struct in6_addr      ip;
    ip_net_phys_stats_t *entry;
    char                 buf[64];

    idx = kp->len - 1;
    ip = CONFD_GET_IPV6(&kp->v[idx][0]);
    inet_ntop(AF_INET6, &ip, buf, sizeof(buf));

    entry = getNeighEntry(ifname, ip.s6_addr, 1);

    if (entry == NULL) {
        error("Couldn't find entry for %s (ip: %s).",
              ifname, buf);
        NOT_FOUND;
    }

    /* Handle the leaf tag. */
    tag = GET_NEXT_TAG;
    switch (tag) {
    case ip_ip:
        CONFD_SET_IPV6(&v,ip);
        break;

    case ip_link_layer_address:
        CONFD_SET_BINARY(&v, entry->hw_addr, entry->hw_addr_len);
        break;

    case ip_origin:
        switch (entry->type) {
        case IP_ELEM(dynamic):
            CONFD_SET_ENUM_VALUE(&v, ip_dynamic);
            break;
        case IP_ELEM(static):
            CONFD_SET_ENUM_VALUE(&v, ip_static);
            break;
        default:
            CONFD_SET_ENUM_VALUE(&v, ip_other);
        }
        break;

    case ip_state:
        switch(entry->state) {
        case IP_ELEM(incomplete):
            CONFD_SET_ENUM_VALUE(&v, ip_incomplete);
            break;

        case IP_ELEM(stale):
            CONFD_SET_ENUM_VALUE(&v, ip_stale);
            break;

        case IP_ELEM(delay):
            CONFD_SET_ENUM_VALUE(&v, ip_delay);
            break;

        case IP_ELEM(probe):
            CONFD_SET_ENUM_VALUE(&v, ip_probe);
            break;

        case IP_ELEM(reachable):
        case IP_ELEM(ps_invalid):
        case IP_ELEM(ps_unknown):
        default:
            CONFD_SET_ENUM_VALUE(&v, ip_reachable);
            break;

        }
        break;

    case ip_is_router:
        if (entry->ipv6router) {
            confd_data_reply_found(tctx);
            return CONFD_OK;
        } else {
            NOT_FOUND;
        }

    default:
        LOG("Not found: %s", KPSTR(kp));
        NOT_FOUND;
    }

    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}


/* Handle /interfaces-state/interface{ifname}/ipv6/address */
static int ipv6_get_elem_address(struct confd_trans_ctx *tctx,
                                 confd_hkeypath_t *kp, char *ifname) {
    int               idx;
    int               tag;
    confd_value_t     v;
    struct in6_addr   ip;
    ipAddressEntry_t *entry;
    char              buf[64];

    idx = kp->len - 1;
    ip = CONFD_GET_IPV6(&kp->v[idx][0]);
    inet_ntop(AF_INET6, &ip, buf, sizeof(buf));

    entry = getIpEntry(ifname, ip.s6_addr, 1);

    if (entry == NULL) {
        error("Couldn't find entry for %s (ip: %s).",
              ifname, buf);

        NOT_FOUND;
    }

    /* Handle the leaf tag. */
    tag = GET_NEXT_TAG;
    switch (tag) {
    case ip_ip:
        CONFD_SET_IPV6(&v, ip);
        break;

    case ip_prefix_length:
        CONFD_SET_UINT8(&v, entry->ipPrefixLen);
        break;

    case ip_origin:
        switch (entry->ipAddressOrigin) {
        case IPADDRESSORIGINTC_MANUAL:
            CONFD_SET_ENUM_VALUE(&v, ip_static);
            break;
        case IPADDRESSORIGINTC_DHCP:
            CONFD_SET_ENUM_VALUE(&v, ip_dhcp);
            break;
        case IPADDRESSORIGINTC_LINKLAYER:
            CONFD_SET_ENUM_VALUE(&v, ip_link_layer);
            break;
        case IPADDRESSORIGINTC_RANDOM:
            CONFD_SET_ENUM_VALUE(&v, ip_random);
            break;
        case IPADDRESSORIGINTC_OTHER:
        default:
            CONFD_SET_ENUM_VALUE(&v, ip_other);
        }
        break;

    case ip_status:
        switch (entry->ipAddressStatus) {
        case IPADDRESSSTATUSTC_PREFERRED:
            CONFD_SET_ENUM_VALUE(&v, ip_preferred);
            break;
        case IPADDRESSSTATUSTC_DEPRECATED:
            CONFD_SET_ENUM_VALUE(&v, ip_deprecated);
            break;
        case IPADDRESSSTATUSTC_INVALID:
            CONFD_SET_ENUM_VALUE(&v, ip_invalid);
            break;
        case IPADDRESSSTATUSTC_INACCESSIBLE:
            CONFD_SET_ENUM_VALUE(&v, ip_inaccessible);
            break;
        case IPADDRESSSTATUSTC_TENTATIVE:
            CONFD_SET_ENUM_VALUE(&v, ip_tentative);
            break;
        case IPADDRESSSTATUSTC_DUPLICATE:
            CONFD_SET_ENUM_VALUE(&v, ip_duplicate);
            break;
        case IPADDRESSSTATUSTC_UNKNOWN:
        default:
            CONFD_SET_ENUM_VALUE(&v, ip_unknown);
        }
        break;

    default:
        LOG("Not found: %s", KPSTR(kp));
        NOT_FOUND;
    }

    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}

/* Handle /interfaces-state/interface{ifname}/ipv6/ */
int ipv6_get_elem(struct confd_trans_ctx *tctx,
                  confd_hkeypath_t *kp) {

    int            idx;
    int            tag;
    char          *ifname;
    int            val;
    char           buf[128];
    confd_value_t  v;
    if_entry_t    *ife;

    LOG("%s", KPSTR(kp));
    idx = kp->len;
    tag = GET_NEXT_TAG;

    if (tag == if_interfaces_state) {
        tag = GET_NEXT_TAG;

        if (tag == if_interface) {
            idx = idx - 1;
            ifname = (char *) CONFD_GET_BUFPTR(&kp->v[idx][0]);

            tag = GET_NEXT_TAG;
            if (tag == ip_ipv6) {

                ife = get_if_entry_n(ifname);

                /* The IPv6 presence container */
                if (idx == 0) {
                    if (ife->ifAdminStatus == 2)
                        confd_data_reply_not_found(tctx);
                    else
                        confd_data_reply_found(tctx);
                    return CONFD_OK;
                }

                tag = GET_NEXT_TAG;
                switch (tag) {
                case ip_mtu:
                    snprintf(buf, sizeof(buf),
                             "/proc/sys/net/ipv6/conf/%s/mtu",
                             ifname);

                    if (get_int32_file_cfg(buf, &val) != 0) {
                        NOT_FOUND;
                    }

                    CONFD_SET_UINT32(&v, val);
                    break;

                case ip_forwarding:
                    CONFD_SET_BOOL(&v, ife->if6Forwarding);;
                    break;

                case ip_address:
                    kp->len = idx;
                    return ipv6_get_elem_address(tctx, kp, ifname);

                case ip_neighbor:
                    kp->len = idx;
                    return ipv6_get_elem_neighbor(tctx, kp, ifname);

                default:
                    NOT_FOUND;
                }

                confd_data_reply_value(tctx, &v);
                return CONFD_OK;
            }
        }
    }

    warn("Unknown keypath %s.", KPSTR(kp));
    NOT_FOUND;

}

int ipv6_get_next(struct confd_trans_ctx *tctx,
                  confd_hkeypath_t *kp,
                  long next) {

    int   idx;
    int   tag;
    char *ifname;

    idx = kp->len;
    tag = GET_NEXT_TAG;

    if (tag == if_interfaces_state) {
        tag = GET_NEXT_TAG;

        if (tag == if_interface) {
            idx = idx - 1;
            ifname = (char *) CONFD_GET_BUFPTR(&kp->v[idx][0]);

            tag = GET_NEXT_TAG;
            if (tag == ip_ipv6) {
                tag = GET_NEXT_TAG;

                /* /if:interfaces-state/if:interface{X}/ipv6/address */
                if (tag == ip_address) {
                    return ip_address_get_next_name(tctx, kp, next, ifname, 1);
                }

                /* /if:interfaces-state/if:interface{X}/ipv6/neighbor */
                if (tag == ip_neighbor) {
                    return ip_net_phys_get_next_name(tctx, kp, next, ifname, 1);
                }
            }
        }
    }

    confd_data_reply_next_key(tctx, NULL, 0, 0);
    return CONFD_OK;
}
