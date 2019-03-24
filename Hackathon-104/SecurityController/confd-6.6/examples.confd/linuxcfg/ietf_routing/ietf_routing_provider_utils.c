#include <string.h>

#include "linuxcfg_api.h"
#include "linuxcfg_util.h"

#include "ietf_routing_utils.h"

#include "ietf-routing.h"
#include "ietf-ipv4-unicast-routing.h"
#include "ietf-ipv6-unicast-routing.h"

int route_afi_from_kp(confd_hkeypath_t *kp, enum route_afi * afi)
{
    TRACE_ENTRY("(kp == %s)", KPSTR(kp));

    int res = CONFD_OK;

    if (NULL == kp || kp->len < 4 || (C_BUF != kp->v[kp->len-4][0].type)) {
        res = CONFD_ERR;
    } else {
        // hard-coded expected position in input keypath
        char * ptr = CONFD_GET_CBUFPTR(&kp->v[kp->len-4][0]);
        if (0 == strcmp(ptr, RIB_NAME_IPv4)) {
            *afi = ipv4;
        } else if (0 == strcmp(ptr, RIB_NAME_IPv6)) {
            *afi = ipv6;
        } else {
            res = CONFD_ERR;
        }
    }

    TRACE_EXIT("(afi == %d; res == %d)", *afi, res);
    return res;
}

void * get_nexthop_list_item(
    struct route_state * state,
    int index
) {
    TRACE_ENTRY("index == %d", index);

    void * ptr = NULL;
    int is_v4 = (ipv4 == state->afi_prefix.afi);
    if (is_v4) {
        ptr = state->nexthop.ipv4.list;
    } else {
        ptr = state->nexthop.ipv6.list;
    }

    // skip "cnt" elements
    int i;
    for (i = 0; i < index; i++) {
        if (NULL != ptr) {
            if (is_v4) {
                ptr = ((struct ipv4_nexthop_list *)ptr)->next;
            } else {
                ptr = ((struct ipv6_nexthop_list *)ptr)->next;
            }
        } else {
            break;
        }
    }

    // and return next one (or NULL, if not present)
    TRACE_EXIT("(%p)", ptr);
    return ptr;
}

void fill_identity_by_rib_name(
    confd_value_t * rib_name,
    confd_value_t * output
) {
    char * ptr = CONFD_GET_CBUFPTR(rib_name);

    if (0 == strcmp(ptr, RIB_NAME_IPv4)) {
        static const struct confd_identityref idr_ipv4 = {
            .id = v4ur_ipv4_unicast,
            .ns = v4ur__ns
        };
        CONFD_SET_IDENTITYREF(output, idr_ipv4);
    } else if (0 == strcmp(ptr, RIB_NAME_IPv6)) {
        static const struct confd_identityref idr_ipv6 = {
            .id = v6ur_ipv6_unicast,
            .ns = v6ur__ns
        };
        CONFD_SET_IDENTITYREF(output, idr_ipv6);
    } else {
        CONFD_SET_NOEXISTS(output);
    }
}

void fill_protocol_identity(
    enum routing_protocol proto,
    confd_value_t * output
) {
    struct confd_identityref prot_ref;
    prot_ref.ns = rt__ns;
    prot_ref.id = -1;

    switch (proto) {
        case prot_direct:
            prot_ref.id = rt_direct;
            break;

        case prot_static:
            prot_ref.id = rt_static;
            break;

        case prot_other:
        default:
            warn("unimplemented protocol enum! (%d)", proto);
    }

    if (-1 != prot_ref.id) {
        CONFD_SET_IDENTITYREF(output, prot_ref);
    } else {
        CONFD_SET_NOEXISTS(output);
    }
}

void fill_ip_prefix_by_rkey(
    struct route_key * key,
    confd_value_t * output
) {
    confd_value_t v;
    CONFD_SET_NOEXISTS(&v);

    if (NULL == key
        || (ipv4 != key->afi && ipv6 != key->afi)
    ) {
        error("Invalid route key source data!");
        return;
    }

    if (ipv4 == key->afi) {
        struct confd_ipv4_prefix pref4;
        pref4.ip = key->addr.ipv4_addr;
        pref4.len = key->prefix_len;
        CONFD_SET_IPV4PREFIX(&v, pref4);
    } else {
        struct confd_ipv6_prefix pref6;
        pref6.ip6 = key->addr.ipv6_addr;
        pref6.len = key->prefix_len;
        CONFD_SET_IPV6PREFIX(&v, pref6);
    }

    confd_value_dup_to(&v, output);
}

void fill_interface_str(
    struct route_state * state,
    confd_value_t * output
) {
    char * ifptr = NULL;

    if (ipv4 == state->afi_prefix.afi) {
        ifptr = state->nexthop.ipv4.val.iface;
    } else if(ipv6 == state->afi_prefix.afi) {
        ifptr = state->nexthop.ipv6.val.iface;
    }
    if (NULL != ifptr) {
        int iflen = strnlen(ifptr, BUFSIZ);
        CONFD_SET_CBUF(output, ifptr, iflen);
    } else {
        CONFD_SET_NOEXISTS(output);
    }
}

int list_index_from_kp(
    confd_hkeypath_t * kp
) {
    TRACE_ENTRY("(%s)", KPSTR(kp));

    int res = -1;

    if (NULL == kp || kp->len < 1) {
        error("Invalid input keypath!");
        goto term;
    }

//   /routing-state/ribs/rib[name]/routes/route[index]/
//        next-hop/next-hop-list/next-hop[index]/outgoing-interface
    confd_value_t * vp = &kp->v[1][0];
    if (C_INT64 != vp->type) {
        error("Invalid keypath, key \"index \" type is %d!", vp->type);
        goto term;
    }

    // beware down-sizing! (long  vs int)
    res = (int) CONFD_GET_INT64(vp);

term:
    TRACE_EXIT("(%d)", res);
    return res;
}

// bind the interface name of the specific next-hop-list
// to the output ConfD variable
void fill_list_interface_str(
    struct route_state * state,
    int index,
    confd_value_t * output
) {
    TRACE_ENTRY("");
    CONFD_SET_NOEXISTS(output);

    if (NULL == state || NULL == output || index < 0) {
        warn("Invalid input variables for next-hop-list interface!");
        goto term;
    }

    if (list != state->nexthop_type) {
        LOG("cross nexthop-type request...");
        CONFD_SET_NOEXISTS(output);
        goto term;
    }

    void * ptr = get_nexthop_list_item(state, index);
    if (NULL == ptr) {
        error("Reached too far in next-hop-list!");
        goto term;
    }

    enum route_afi afi = state->afi_prefix.afi;
    char * ifptr;
    if (ipv4 == afi) {
        ifptr = ((struct ipv4_nexthop *)ptr)->iface;
    } else if (ipv6 == afi) {
        ifptr = ((struct ipv6_nexthop *)ptr)->iface;
    }

    if (NULL != ifptr) {
        LOG("got interface");
        int iflen = strnlen(ifptr, BUFSIZ);
        CONFD_SET_CBUF(output, ifptr, iflen);
    }

term:
    TRACE_EXIT("");
}

void fill_list_nexthop_addr(
    struct route_state * state,
    int index,
    confd_value_t * output
) {
    TRACE_ENTRY("");
    CONFD_SET_NOEXISTS(output);

    if (NULL == state || NULL == output) {
        warn("Invalid input variables for next-hop-list address!");
        goto term;
    }

    void * ptr = get_nexthop_list_item(state, index);

    if (ipv4 == state->afi_prefix.afi ) {
        struct ipv4_nexthop * v4ptr = (struct ipv4_nexthop *)ptr;
        if (v4ptr->has_addr) {
            CONFD_SET_IPV4(output, v4ptr->addr);
        }
    } else {
        struct ipv6_nexthop * v6ptr = (struct ipv6_nexthop *)ptr;
        if (v6ptr->has_addr) {
            CONFD_SET_IPV6(output, v6ptr->addr);
        }
    }

term:
    TRACE_EXIT("");
}

void fill_special_nexthop(
    enum special_nexthop special_nh,
    confd_value_t * output
) {
    CONFD_SET_ENUM_VALUE(output, spec_nexthop_system_to_hash(special_nh));
}


void fill_nexthop_address(
    struct route_state * state,
    confd_value_t * output
) {
    int afi = state->afi_prefix.afi;

    int has_addr = (simple == state->nexthop_type);
    if (ipv4 == afi) {
        has_addr = (has_addr && (state->nexthop.ipv4.val.has_addr));
    } else {
        has_addr = (has_addr && (state->nexthop.ipv6.val.has_addr));
    }

    if (has_addr) {
        if (ipv4 == afi) {
            CONFD_SET_IPV4(output, state->nexthop.ipv4.val.addr);
        } else {
            CONFD_SET_IPV6(output, state->nexthop.ipv6.val.addr);
        }
    } else {
        CONFD_SET_NOEXISTS(output);
    }
}

struct system_route_data *route_data_ptr_get(void *tctx_opaque_data)
{
    struct opaque_data *data = (struct opaque_data *) tctx_opaque_data;
    return (struct system_route_data *) data->routing_data;
}

void route_data_ptr_set(void *tctx_opaque_data, void *dest)
{
    struct opaque_data *data = (struct opaque_data *) tctx_opaque_data;
    data->routing_data = dest;
}