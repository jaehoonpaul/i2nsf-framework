#ifndef IETF_ROUTING_ACTIVE_ROUTE_H
#define IETF_ROUTING_ACTIVE_ROUTE_H

#include <confd.h>

// "action" callback procedure for a registration with struct confd_action_cbs
int routing_active_route(
    struct confd_user_info *uinfo,
    struct xml_tag *name,
    confd_hkeypath_t *kp,
    confd_tag_value_t *params,
    int nparams
);

#endif