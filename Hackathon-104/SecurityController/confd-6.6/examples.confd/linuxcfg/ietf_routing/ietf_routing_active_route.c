#include <string.h>
#include <stdlib.h>

#include "linuxcfg_api.h"
#include "linuxcfg_util.h"

#include "ietf-routing.h"
#include "ietf-ipv4-unicast-routing.h"
#include "ietf-ipv6-unicast-routing.h"

#include "ietf_routing_system.h"
#include "ietf_routing_provider_utils.h"

// namespace tag of the ietf_routing.yang
static const unsigned int common_ns = rt__ns;

static void trace_tag_value_array(
    const confd_tag_value_t * array,
    int array_len
) {
    TRACE_ENTRY("(trace array: %p; length == %d)", array, array_len);
    int i;
    for (i = 0; i < array_len; i++) {
        char * tag_str = confd_xmltag2str(array[i].tag.ns, array[i].tag.tag);
        LOG("#%d == %s: %s (type == %d)", i, tag_str, VALSTRNN(&(array[i].v)),
                array[i].v.type);
    }

    TRACE_EXIT("");
}

// extract IPv4/6 address form the action callback input parameters
static int addr_from_params(
    enum route_afi afi,
    const struct xml_tag *name,
    const confd_tag_value_t *params,
    const int nparams,
    union route_addr * addr
) {
    TRACE_ENTRY("");
    trace_tag_value_array(params, nparams);

    int ret_code = CONFD_OK;
    if (rt_active_route != name->tag) {
        LOG("Unimplemented action! (%s)",
                confd_xmltag2str(name->ns, name->tag));
        ret_code = CONFD_ERR;
        goto term;
    }

    if (NULL == params || nparams < 1) {
        LOG("Missing input parameter data!");
        ret_code = CONFD_ERR;
        goto term;
    }

    struct xml_tag xt = params[0].tag;
    const confd_value_t * val = &(params[0].v);
    unsigned int val_type = val->type;

    LOG("input argument: afi == %d; tag == %d/%d; value type == %d",
            afi, xt.ns, xt.tag, val_type);
    if (ipv4 == afi
        && v4ur__ns == xt.ns
        && v4ur_destination_address == xt.tag
        && C_IPV4 == val_type
    ) {
        addr->ipv4_addr = CONFD_GET_IPV4(val);
    } else if (ipv6 == afi
        && v6ur__ns == xt.ns
        && v6ur_destination_address == xt.tag
        && C_IPV6 == val_type
    ) {
        addr->ipv6_addr = CONFD_GET_IPV6(val);
    } else {
        LOG("Unexpected input parameter type! (%s)",
                confd_xmltag2str(xt.ns, xt.tag));
        ret_code = CONFD_ERR;
        goto term;
    }

term:
    TRACE_EXIT("(%d)", ret_code);
    return ret_code;
}

static int ipv_nexthop_has_addr(
    void * ipv_ptr,
    enum route_afi afi
) {
    int res = 0;

    if (ipv4 == afi) {
        struct ipv4_nexthop * v4ptr = (struct ipv4_nexthop *)ipv_ptr;
        res = (0 != v4ptr->has_addr);
    } else if (ipv6 == afi) {
        struct ipv6_nexthop * v6ptr = (struct ipv6_nexthop *)ipv_ptr;
        res = (0 != v6ptr->has_addr);
    }

    return res;
}

static int ipv_nexthop_has_iface(
    void * ipv_ptr,
    enum route_afi afi
) {
    int res = 0;

    if (ipv4 == afi) {
        struct ipv4_nexthop * v4ptr = (struct ipv4_nexthop *)ipv_ptr;
        res = (NULL != v4ptr->iface);
    } else if (ipv6 == afi) {
        struct ipv6_nexthop * v6ptr = (struct ipv6_nexthop *)ipv_ptr;
        res = (NULL != v6ptr->iface);
    }

    return res;
}

static int is_known_source_protocol(
    enum routing_protocol proto
) {
    confd_value_t temp_identity;
    fill_protocol_identity(proto, &temp_identity);

    return (C_NOEXISTS != temp_identity.type);
}

static int tag_count_nexthop_list(
    union nexthop * root_nexthop,
    enum route_afi afi
) {
    TRACE_ENTRY("");
    int cnt = 0;

    cnt += 2; // next-hop-list - begin/end

    void * ptr;

    int is_v4 = (ipv4 == afi);
    if (is_v4) {
        ptr = root_nexthop->ipv4.list;
    } else {
        ptr = root_nexthop->ipv6.list;
    }

    // for each list entry
    while (NULL != ptr) {

        cnt += 2; // next-hop - begin/end

        cnt += 1; // index - key of the list

        void * ipv_ptr;
        if (is_v4) {
            ipv_ptr = &(((struct ipv4_nexthop_list *)ptr)->val);
        } else {
            ipv_ptr = &(((struct ipv6_nexthop_list *)ptr)->val);
        }

        // next-hop-address
        if (ipv_nexthop_has_addr(ipv_ptr, afi)) {
            cnt += 1;
        }
        // outgoing-interface
        if (ipv_nexthop_has_iface(ipv_ptr, afi)) {
            cnt += 1;
        }

        if (is_v4) {
            ptr = ((struct ipv4_nexthop_list *)ptr)->next;
        } else {
            ptr = ((struct ipv6_nexthop_list *)ptr)->next;
        }
    }

    TRACE_EXIT("(%d)", cnt);
    return cnt;
}

// return number of confd_tag_value_t elements required to build array for the
// input route state data...
static int tag_count_route_subtree(struct route_state * state)
{
    TRACE_ENTRY("");
    int cnt = 0;

    if (NULL == state) {
        goto term;
    }

    cnt += 2; // route - begin/end
    cnt += 2; // route/next-hop - begin/end

    enum nexthop_type nh_type = state->nexthop_type;

    enum route_afi afi = state->afi_prefix.afi;

    void * ipv_ptr;
    if (ipv4 == state->afi_prefix.afi) {
        ipv_ptr = &(state->nexthop.ipv4.val);
    } else {
        ipv_ptr = &(state->nexthop.ipv6.val);
    }

    if (simple == nh_type) {
        // next-hop-address
        if (ipv_nexthop_has_addr(ipv_ptr, afi)) {
            cnt += 1;
        }
        // outgoing-interface
        if (ipv_nexthop_has_iface(ipv_ptr, afi)) {
            cnt += 1;
        }
    } else if (special == nh_type) {
        cnt += 1;   // special-next-hop
    } else if (list == nh_type) {
        cnt += tag_count_nexthop_list(&state->nexthop, afi);
    }

    if (is_known_source_protocol(state->protocol)) {
        cnt += 1;   // route/source-protocol
    }

    // active
    if (state->is_active) {
        cnt += 1;
    }

    // cnt += 1;    // last-updated - not present in current implementation

    cnt += 1; //  destination-prefix

term:
    TRACE_EXIT("(array length == %d)", cnt);
    return cnt;
}

// fill_data_... procedures below set the specific tagged value of for the
// action output parameters

static int fill_data_nexthop_address(
    confd_tag_value_t * dest,
    struct route_state * state
) {
    fill_nexthop_address(state, &(dest->v));
    if (ipv4 == state->afi_prefix.afi) {
        dest->tag.tag = v4ur_next_hop_address;
        dest->tag.ns = v4ur__ns;
    } else {
        dest->tag.tag = v6ur_next_hop_address;
        dest->tag.ns = v6ur__ns;
    }
    return 1;
}

static int fill_data_source_protocol(
    confd_tag_value_t * dest,
    enum routing_protocol proto
) {
    fill_protocol_identity(proto, &(dest->v));
    dest->tag.tag = rt_source_protocol;
    dest->tag.ns = common_ns;
    return 1;
}

static int fill_data_destination_prefix(
    confd_tag_value_t * dest,
    struct route_key * key
) {
    fill_ip_prefix_by_rkey(key, &dest->v);
    if (ipv4 == key->afi) {
        dest->tag.tag = v4ur_destination_prefix;
        dest->tag.ns = v4ur__ns;
    } else {
        dest->tag.tag = v6ur_destination_prefix;
        dest->tag.ns = v6ur__ns;
    }
    return 1;
}

static int fill_data_nexthop_special(
    confd_tag_value_t * dest,
    enum special_nexthop special_nh
) {
    fill_special_nexthop(special_nh, &dest->v);
    dest->tag.tag = rt_special_next_hop;
    dest->tag.ns = common_ns;
    return 1;
}

static int fill_data_nexthop_simple(
    confd_tag_value_t * dest,
    struct route_state * state
) {
    int curr_idx = 0;

    enum route_afi afi = state->afi_prefix.afi;

    void * ipv_ptr;
    if (ipv4 == afi) {
        ipv_ptr = &(state->nexthop.ipv4.val);
    } else {
        ipv_ptr = &(state->nexthop.ipv6.val);
    }

    if (ipv_nexthop_has_addr(ipv_ptr, afi)) {
        curr_idx += fill_data_nexthop_address(&dest[curr_idx], state);
    }

    if (ipv_nexthop_has_iface(ipv_ptr, afi)) {
        fill_interface_str(state, &(dest[curr_idx].v));
        dest[curr_idx].tag.tag = rt_outgoing_interface;
        dest[curr_idx].tag.ns = common_ns;
        curr_idx++;
    }

    return curr_idx;
}

static int fill_data_nexthop_list(
    confd_tag_value_t * dest,
    struct route_state * state
) {
    TRACE_ENTRY("");
    int curr_idx = 0;

    enum route_afi afi = state->afi_prefix.afi;

    // container "next-hop-list"
    CONFD_SET_TAG_XMLBEGIN(&dest[curr_idx], rt_next_hop_list, common_ns);
    curr_idx++;

    int is_v4 = (ipv4 == afi);

    void * list_ptr;
    if (is_v4) {
        list_ptr = state->nexthop.ipv4.list;
    } else {
        list_ptr = state->nexthop.ipv6.list;
    }

    int key_index = 0;
    while (NULL != list_ptr) {

        // list "next-hop" entry begin
        CONFD_SET_TAG_XMLBEGIN(&dest[curr_idx], rt_next_hop, common_ns);
        curr_idx++;

        // list key value
        // TODO - namespace fix needed?
        int index_tag = (is_v4) ? v4ur_index : v6ur_index;
        CONFD_SET_TAG_INT64(&dest[curr_idx], index_tag, key_index);
        curr_idx++;

        void * ipv_ptr;
        if (is_v4) {
            ipv_ptr = &(((struct ipv4_nexthop_list *)list_ptr)->val);
        } else {
            ipv_ptr = &(((struct ipv6_nexthop_list *)list_ptr)->val);
        }

        if (ipv_nexthop_has_addr(ipv_ptr, afi)) {
            fill_list_nexthop_addr(state, key_index, &(dest[curr_idx].v));
            if (is_v4) {
                dest->tag.tag = v4ur_next_hop_address;
                dest->tag.ns = v4ur__ns;
            } else {
                dest->tag.tag = v6ur_next_hop_address;
                dest->tag.ns = v6ur__ns;
            }
            curr_idx++;
        }

        if (ipv_nexthop_has_iface(ipv_ptr, afi)) {
            fill_list_interface_str(state, key_index, &(dest[curr_idx].v));
            dest[curr_idx].tag.tag = rt_outgoing_interface;
            dest[curr_idx].tag.ns = common_ns;
            curr_idx++;
        }

        // list "next-hop" entry end
        CONFD_SET_TAG_XMLEND(&dest[curr_idx], rt_next_hop, common_ns);
        curr_idx++;

        if (is_v4) {
            list_ptr = ((struct ipv4_nexthop_list *)list_ptr)->next;
        } else {
            list_ptr = ((struct ipv6_nexthop_list *)list_ptr)->next;
        }
        key_index++;
    }

    // container "next-hop-list"
    CONFD_SET_TAG_XMLEND(&dest[curr_idx], rt_next_hop_list, common_ns);
    curr_idx++;

    TRACE_EXIT("(new offset == +%d)", curr_idx);
    return curr_idx;
}

static int fill_data_nexthop(
    confd_tag_value_t * dest,
    struct route_state * state
) {
    int curr_idx = 0;

    // container "next-hop"
    CONFD_SET_TAG_XMLBEGIN(&dest[curr_idx], rt_next_hop, common_ns);
    curr_idx++;

    enum nexthop_type nh = state->nexthop_type;
    if (simple == nh) {
        curr_idx += fill_data_nexthop_simple(&dest[curr_idx], state);
    } else if (special == nh) {
        curr_idx += fill_data_nexthop_special(&dest[curr_idx],
                        state->nexthop.special);
        curr_idx++;
    } else if (list == nh) {
        curr_idx += fill_data_nexthop_list(&dest[curr_idx], state);
    }

    // container "next-hop"
    CONFD_SET_TAG_XMLEND(&dest[curr_idx], rt_next_hop, common_ns);
    curr_idx++;

    return curr_idx;
}

// fills in the whole subtree of active-route output parameters
// "dest" must have sufficient length to allow number of tagged values;
// (see "tag_count_route_subtree")
static void fill_data_route(
    confd_tag_value_t * dest,
    struct route_state * state
) {
    TRACE_ENTRY("");

    int curr_idx = 0;

    // container "route"
    CONFD_SET_TAG_XMLBEGIN(&dest[curr_idx], rt_route, common_ns);
    curr_idx++;

    // leaf source-protocol
    if (is_known_source_protocol(state->protocol)) {
        curr_idx += fill_data_source_protocol(&dest[curr_idx], state->protocol);
    }

    // leaf destination-prefix
    curr_idx += fill_data_destination_prefix(&dest[curr_idx],
                    &state->afi_prefix);

    // leaf active
    if (state->is_active) {
        CONFD_SET_TAG_XMLTAG(&dest[curr_idx], rt_active, common_ns);
        curr_idx++;
    }

    // subtree of next-hop
    curr_idx += fill_data_nexthop(&dest[curr_idx], state);

    // container "route"
    CONFD_SET_TAG_XMLEND(&dest[curr_idx], rt_route, common_ns);
    curr_idx++;

    TRACE_EXIT("");
}

static int build_active_route_data(
    struct route_state * state,
    confd_tag_value_t ** output_array,
    int * output_count
) {
    TRACE_ENTRY("");
    int ret_code = CONFD_OK;

    int array_length = tag_count_route_subtree(state);

    confd_tag_value_t * values = NULL;
    values = malloc(array_length * sizeof(*values));
    if (NULL == values) {
        LOG("Failed to allocate action output array!");
        ret_code = CONFD_ERR;
        goto term;
    }

    fill_data_route(values, state);

    *output_array = values;
    *output_count = array_length;

term:
    TRACE_EXIT("");
    return ret_code;
}

// action callback implementation itself
int routing_active_route(
    struct confd_user_info *uinfo,
    struct xml_tag *name,
    confd_hkeypath_t *kp,
    confd_tag_value_t *params,
    int nparams
) {
    TRACE_ENTRY("");

    int ret_code = CONFD_OK;
    char * err_msg = NULL;

    // get AFI of request
    enum route_afi afi;
    ret_code = route_afi_from_kp(kp, &afi);
    if (CONFD_OK != ret_code) {
        err_msg = "Unsupported AFI in action keypath!";
        goto term;
    }
    LOG("afi == %d", afi);

    // get destination IP address of request
    union route_addr addr;
    ret_code = addr_from_params(afi, name, params, nparams, &addr);
    if (CONFD_OK != ret_code) {
        err_msg = "Failed to process input parameters!";
        goto term;
    }

    // get active route from system using AFI and input address
    struct route_state state;
    memset(&state, 0x00, sizeof(state));
    enum routing_active_ret_val rv = get_active_route(afi, addr, &state);
    if (state_active_err == rv) {
        ret_code = CONFD_ERR;
        goto term;
    } else if (state_active_not_found == rv) {
        // TODO - respond not found
        goto term;
    }

    confd_tag_value_t * values = NULL;
    int nvalues = 0;

    ret_code = build_active_route_data(&state, &values, &nvalues);
    if (CONFD_OK != ret_code) {
        warn("failed to extract route data!");
    } else {
        trace_tag_value_array(values, nvalues);
        ret_code = confd_action_reply_values(uinfo, values, nvalues);
    }

    free_route_nexthops(&state);

term:
    if (CONFD_OK != ret_code) {
        confd_action_seterr(uinfo, "%s", err_msg);
    }

    TRACE_EXIT("(%d)", ret_code);
    return ret_code;
}
