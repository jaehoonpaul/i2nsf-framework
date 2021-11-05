#include "linuxcfg_api.h"

#include "ietf_routing_utils.h"
#include "ietf-routing.h"

int check_kp_tag_pos(confd_hkeypath_t * kp, uint32_t tag, uint32_t pos)
{
    if (kp == NULL) {
        return 0;
    }

    if (kp->len <= pos) {
        return 0;
    }

    if (C_XMLTAG != kp->v[pos][0].type) {
        return 0;
    }

    return (tag == CONFD_GET_XMLTAG(&kp->v[pos][0]));
}

int spec_nexthop_system_to_hash(enum special_nexthop nh)
{
    int ret;
    switch (nh) {
        case blackhole:
            ret = rt_blackhole;
            break;
        case unreachable:
            ret = rt_unreachable;
            break;
        case prohibit:
            ret = rt_prohibit;
            break;
        case receive:
            warn("special-next-hop RECEIVE is not supported!");
            ret = rt_receive;  // TODO - not supported in linux?
            break;
        default:
            ret = -1;
    }
    return ret;
}

enum special_nexthop spec_nexthop_hash_to_system(int hash)
{
    enum special_nexthop ret;
    switch (hash) {
        case rt_blackhole:
            ret = blackhole;
            break;
        case rt_unreachable:
            ret = unreachable;
            break;
        case rt_prohibit:
            ret = prohibit;
            break;
        case rt_receive:
            warn("special-next-hop RECEIVE is not supported!");
            ret = receive;  // TODO - not supported in linux?
            break;
        default:
            ret = none;
    }
    return ret;
}

enum nexthop_type nexthop_type_hash_to_system(int hash)
{
    enum nexthop_type ret;
    switch (hash) {
        case rt_simple_next_hop:
            ret = simple;
            break;
        case rt_special_next_hop:
            ret = special;
            break;
        case rt_next_hop_list:
            ret = list;
            break;
        default:
            error("Unsupported nexthop-type!");
            ret = simple;
    }
    return ret;
}
