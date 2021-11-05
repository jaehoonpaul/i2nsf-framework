#ifndef IETF_ROUTING_PROVIDER_UTILS_H
#define IETF_ROUTING_PROVIDER_UTILS_H

// extract ipv4/ipv6 afi value from the input ietf-routing.yang keypath
int route_afi_from_kp(confd_hkeypath_t *kp, enum route_afi * afi);

// retrieve N-th element form the next-hop-list linked list,
// starting with 0 as a first element index;
// return NULL if the list does not have that many elements
void * get_nexthop_list_item(
    struct route_state * state,
    int index
);

// following set of procedures fills in the confd_value_t variable using
// some corresponding "system" level input

// set the output value to an identity reference of "address-family" type;
// see appropriate v4ur/v6ur yangs for details...
void fill_identity_by_rib_name(
    confd_value_t * rib_name,
    confd_value_t * output
);

// set the output value to an identity reference of "routing-protocol" type;
// see ietf-routing.yang for details...
void fill_protocol_identity(
    enum routing_protocol proto,
    confd_value_t * output
);

// create IPv4/6 prefix from the system route key
void fill_ip_prefix_by_rkey(
    struct route_key * key,
    confd_value_t * output
);

// bind the interface name of the specific route state
void fill_interface_str(
    struct route_state * state,
    confd_value_t * output
);

// extract list index from the input keypath for the next-hop-list element
int list_index_from_kp(
    confd_hkeypath_t * kp
);

// bind the interface name of the specific next-hop-list
void fill_list_interface_str(
    struct route_state * state,
    int index,
    confd_value_t * output
);

// fill in nexthop IPv4/6 address
void fill_list_nexthop_addr(
    struct route_state * state,
    int index,
    confd_value_t * output
);

// fill in special nexthop value
void fill_special_nexthop(
    enum special_nexthop special_nh,
    confd_value_t * output
);

// fill in simple nexthop destination prefix
void fill_nexthop_address(
    struct route_state * state,
    confd_value_t * output
);

// extract route data pointer from the transaction's t_opaque
struct system_route_data *route_data_ptr_get(void *tctx_opaque_data);

// set the value of route data ptr hidden in the tctx->t_opaque
void route_data_ptr_set(void *tctx_opaque_data, void * ptr_dest);

#endif