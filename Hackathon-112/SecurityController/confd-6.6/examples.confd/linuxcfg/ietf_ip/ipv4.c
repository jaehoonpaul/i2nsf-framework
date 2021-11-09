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
#include "ipv4.h"

#include "ietf-ip.h"
#include "ietf-interfaces.h"

/* Headers from the ipmibs component */
#include "if.h"
#include "ipmibs.h"
#include "IP-MIB.h"

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

/* IPv4 helpers */
static void set_ipv4addr_prefix(char *ifname,
                                struct in_addr addr,
                                int prefix);

/* Interface manipulation functions, IPv4 */
static void apply_ipv4mtu(char *ifname, int val);
static void apply_ipv4forwarding(char *ifname, int val);
static void delete_ipv4addr(char *ifname, char *addrstr, char *extra);
static void add_ipv4addr(char *ifname, char *addrstr, char *extra);
static void add_ipv4neigh(char *ifname, char *ipbuf, char *str);
static void delete_ipv4neigh(char *ifname, char *ip, char *val);

DECLARE_SIMPLE_HANDLER(ipv4enabled,    NO_FLAG);
DECLARE_SIMPLE_HANDLER(ipv4mtu,        ipv4enabled);
DECLARE_SIMPLE_HANDLER(ipv4forwarding, ipv4enabled);
DECLARE_LIST_HANDLER(ipv4neigh,        ipv4enabled);
DECLARE_LIST_HANDLER(ipv4addr,         ipv4enabled);

static enum cdb_iter_ret handle_ipv4_neighbor(int sock,
                                              char *ifname,
                                              struct in_addr addr,
                                              enum cdb_iter_op op) {
    unsigned char *buf;
    int   bufsz;
    char  *str;
    char  *ipbuf;

    /// XXX: IF status?
    ipbuf = inet_ntoa(addr);

    if (op == MOP_DELETED) {
        list_delete_ipv4neigh(ifname, ipbuf);
    } else {
        cdb_get_binary(sock, &buf, &bufsz, "link-layer-address");
        str = physaddr2str(buf, bufsz);
        update_ipv4neigh(ifname, ipbuf, str);

        free(buf);
        free(str);
    }

    return ITER_CONTINUE;
}


static enum cdb_iter_ret handle_ipv4addr(int sock,
                                         char *ifname,
                                         struct in_addr addr,
                                         enum cdb_iter_op op) {
    u_int8_t prefix;

    if (op == MOP_DELETED) {
        char *key = inet_ntoa(addr);
        list_delete_ipv4addr(ifname, key);
    } else {
        if (cdb_get_u_int8(sock, &prefix, "prefix-length") == CONFD_OK) {
            set_ipv4addr_prefix(ifname, addr, prefix);
        } else {
            error("Couldn't find prefix-length for %s on %s.",
                  inet_ntoa(addr), ifname);
        }
    }

    return ITER_CONTINUE;
}

/* This is the central ipv4 iterator function, it will be called once for
 * each change to the /interfaces/interface{X}/ipv4 subtree. */
enum cdb_iter_ret ipv4_iter(confd_hkeypath_t *kp,
                            enum cdb_iter_op op,
                            confd_value_t *oldv,
                            confd_value_t *newv,
                            void *state)
{
    int   tag, idx;
    int   rsock  = *((int *) state);
    char *ifname;
    struct in_addr addr;
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

            /* interfaces/interface{X}/ipv4 */
            if (tag == ip_ipv4) {

                /* Top level object isn't interesting, descend */
                if (idx == 0) {
                    return ITER_RECURSE;
                }

                tag = GET_NEXT_TAG;

                switch (tag) {
                case ip_enabled:
                    set_ipv4enabled(ifname, CONFD_GET_BOOL(newv));
                    break;
                case ip_forwarding:
                    set_ipv4forwarding(ifname, CONFD_GET_BOOL(newv));
                    break;
                case ip_mtu:
                    set_ipv4mtu(ifname, CONFD_GET_UINT16(newv));
                    break;
                case ip_address:
                    idx = idx - 1;
                    addr = CONFD_GET_IPV4(&kp->v[idx][0]);
                    CHECK3(cdb_pushd(rsock, "%h", kp),
                           "Pushd to ipv4/address failed", ITER_STOP);

                    status = handle_ipv4addr(rsock, ifname, addr, op);

                    cdb_popd(rsock);
                    break;
                case ip_neighbor:
                    idx = idx - 1;
                    addr = CONFD_GET_IPV4(&kp->v[idx][0]);
                    CHECK3(cdb_pushd(rsock, "%h", kp),
                           "Pushd to ipv4/neighbor failed", ITER_STOP);

                    status = handle_ipv4_neighbor(rsock, ifname, addr, op);

                    cdb_popd(rsock);
                    break;
                default:
                    warn("Tag not handled! Path: %s.", KPSTR(kp));
                }

            } else {
                warn("Called with bad key path %s.", KPSTR(kp));
            }
        }
    }

    return status;
}

static void set_ipv4addr_prefix(char *ifname,
                                struct in_addr addr,
                                int prefix) {
    char buf[64];

    snprintf(buf, sizeof(buf), "%s/%d",
             inet_ntoa(addr),
             prefix);

    update_ipv4addr(ifname, inet_ntoa(addr), buf);
}

/*
** Utility functions for interface manipulations, IPv4
*/
void apply_ipv4enabled(char *ifname, int val) {
    struct ifstate  *ifs;
    struct keyval *p;

    LOG("val: %d", val);

    if (val == NOT_SET)
        return;

    if (val) {
        /* When ipv4 is enabled on an interface,
           push all our settings  */
        ifs = get_ifstate(ifname);
        apply_ipv4mtu(ifname, ifs->ipv4mtu);
        apply_ipv4forwarding(ifname, ifs->ipv4forwarding);

        for(p = ifs->ipv4addrlist; p != NULL; p = p->next) {
            add_ipv4addr(ifname, NULL, p->val);
        }

        for(p = ifs->ipv4neighlist; p != NULL; p = p->next) {
            add_ipv4neigh(ifname, p->key, p->val);
        }

    } else {
        /* Flush ipv4, but not ipv6, if ipv4 is disabled. */
        run(NULL, 0, "%s -4 address flush dev %s", IP_CMD, ifname);
    }
}

static void apply_ipv4mtu(char *ifname, int val) {
    if (val == NOT_SET)
        return;

    run(NULL, 0, "%s -4 link set %s mtu %d", IP_CMD, ifname, val);
}

static void apply_ipv4forwarding(char *ifname, int val) {
    if (val == NOT_SET)
        return;

    run(NULL, 0, "%s net.ipv4.conf.%s.forwarding=%d",
        SYSCTL_CMD, ifname, val);
}

static void delete_ipv4addr(char *ifname, char *addrstr, char *extra) {
    run(NULL, 0, "%s -4 address del %s dev %s",
        IP_CMD, addrstr, ifname);
}

static void add_ipv4addr(char *ifname, char *extra, char *addrstr) {
    run(NULL, 0, "%s -4 address add %s dev %s",
        IP_CMD, addrstr, ifname);
}

static void add_ipv4neigh(char *ifname, char *ipbuf, char *str) {
    run(NULL, 0, "%s neigh replace %s lladdr %s dev %s",
        IP_CMD, ipbuf, str, ifname);
}

static void delete_ipv4neigh(char *ifname, char *key, char *val) {
    run(NULL, 0, "%s neigh del %s dev %s", IP_CMD, key, ifname);
}

/*
** Operational data
*/


/* Handle /interfaces-state/interface{ifname}/ipv4/neighbor/ */
static int ipv4_get_elem_neighbor(struct confd_trans_ctx *tctx,
                                    confd_hkeypath_t *kp, char *ifname) {
    int                  idx;
    int                  tag;
    confd_value_t        v;
    struct in_addr       ip;
    ip_net_phys_stats_t *entry;

    idx = kp->len - 1;
    ip = CONFD_GET_IPV4(&kp->v[idx][0]);

    LOG("ifname: %s, ip: %s, kp: %s", ifname, inet_ntoa(ip), KPSTR(kp));

    entry = getNeighEntry(ifname, &ip.s_addr, 0);

    if (entry == NULL) {
        error("Couldn't find entry for %s (ip: %s).",
              ifname, inet_ntoa(ip));
        NOT_FOUND;
    }

    /* Handle the leaf tag. */
    tag = GET_NEXT_TAG;
    switch (tag) {
    case ip_ip:
        CONFD_SET_IPV4(&v,ip);
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

    default:
        LOG("Not found: %s", KPSTR(kp));
        NOT_FOUND;
    }

    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}

/* Handle /interfaces-state/interface{ifname}/ipv4/address */
static int ipv4_get_elem_address(struct confd_trans_ctx *tctx,
                                 confd_hkeypath_t *kp, char *ifname) {
    int               idx;
    int               tag;
    confd_value_t     v;
    struct in_addr    ip;
    ipAddressEntry_t *entry;

    idx = kp->len - 1;
    ip = CONFD_GET_IPV4(&kp->v[idx][0]);
    LOG("ifname: %s, ip: %s", ifname, inet_ntoa(ip));

    entry = getIpEntry(ifname, &ip.s_addr, 0);

    if (entry == NULL) {
        error("Couldn't find entry for %s (ip: %s).",
              ifname, inet_ntoa(ip));
        error("Couldn't find entry for.. ");
        NOT_FOUND;
    }

    /* Handle the leaf tag. */
    tag = GET_NEXT_TAG;
    switch (tag) {
    case ip_ip:
        CONFD_SET_IPV4(&v, ip);
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

    default:
        LOG("Not found: %s", KPSTR(kp));
        NOT_FOUND;
    }

    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}

/* Handle /interfaces-state/interface{ifname}/ipv4/ */
int ipv4_get_elem(struct confd_trans_ctx *tctx,
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
            if (tag == ip_ipv4) {

                ife = get_if_entry_n(ifname);

                /* The IPv4 presence container */
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
                    CONFD_SET_UINT16(&v, ife->ifMtu);
                    break;

                case ip_forwarding:
                    snprintf(buf, sizeof(buf),
                             "/proc/sys/net/ipv4/conf/%s/forwarding",
                             ifname);

                    if (get_int32_file_cfg(buf, &val) != 0) {
                        NOT_FOUND;
                    }

                    CONFD_SET_BOOL(&v, val);
                    break;

                case ip_address:
                    kp->len = idx;
                    return ipv4_get_elem_address(tctx, kp, ifname);
                case ip_neighbor:
                    kp->len = idx;
                    return ipv4_get_elem_neighbor(tctx, kp, ifname);

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

int ipv4_get_next(struct confd_trans_ctx *tctx,
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
            if (tag == ip_ipv4) {
                tag = GET_NEXT_TAG;

                /* /if:interfaces-state/if:interface{X}/ipv4/address */
                if (tag == ip_address) {
                    return ip_address_get_next_name(tctx, kp, next, ifname, 0);
                }

                /* /if:interfaces-state/if:interface{X}/ipv4/neighbor */
                if (tag == ip_neighbor) {
                    return ip_net_phys_get_next_name(tctx, kp, next, ifname, 0);
                }
            }
        }
    }

    confd_data_reply_next_key(tctx, NULL, 0, 0);
    return CONFD_OK;
}

int ipv4_get_case(struct confd_trans_ctx *tctx,
                  confd_hkeypath_t *kp, confd_value_t *choice) {
    confd_value_t val;

    LOG("keypath: %s choice: %s.", KPSTR(kp), VALSTR(choice));

    /* There is only a single case-node in the tree, so we hard-code our
       response. */
    CONFD_SET_XMLTAG(&val, ip_prefix_length, ip__ns);

    confd_data_reply_value(tctx, &val);
    return CONFD_OK;
}
