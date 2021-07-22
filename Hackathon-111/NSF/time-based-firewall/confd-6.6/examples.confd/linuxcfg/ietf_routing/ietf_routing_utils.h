#ifndef IETF_ROUTING_UTILS_H
#define IETF_ROUTING_UTILS_H

#include <stdint.h>
#include <confd.h>
#include "ietf_routing_system.h"

#define RIB_NAME_IPv4 "system-IPv4"
#define RIB_NAME_IPv6 "system-IPv6"

// Check whether the "tag" occupies keypath's position at index "pos".
// Return 1 on above condition met, 0 in any other case (shorter path etc).
int check_kp_tag_pos(confd_hkeypath_t * kp, uint32_t tag, uint32_t pos);

// convert enumeration value used in ietf_routing_system API to
// ConfD hash value
int spec_nexthop_system_to_hash(enum special_nexthop nh);

// convert ConfD hash value to the enumeration value used in
// ietf_routing_system API
enum special_nexthop spec_nexthop_hash_to_system(int hash);

// convert ConfD hash value to the enumeration used in ietf_routing_system API
enum nexthop_type nexthop_type_hash_to_system(int hash);

#endif // IETF_ROUTING_UTILS_H
