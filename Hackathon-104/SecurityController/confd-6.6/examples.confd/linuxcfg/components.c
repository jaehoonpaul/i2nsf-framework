/*
 * Copyright 2014 Tail-F Systems AB
 * Tail-F customers are permitted to redistribute in binary form,
 * with or without modification, for use in customer products.
 */

#include <confd.h>
#include "linuxcfg_api.h"

extern struct component ietf_system;
extern struct component ietf_interfaces;
extern struct component ipmibs;
extern struct component ietf_ip;
extern struct component ietf_routing;

struct component *components[] = {
#ifdef WITH_ietf_system
    &ietf_system,
#endif
#ifdef WITH_ietf_interfaces
    &ietf_interfaces,
#endif
#ifdef WITH_ipmibs
    &ipmibs,
#endif
#ifdef WITH_ietf_ip
    &ietf_ip,
#endif
#ifdef WITH_ietf_routing
    &ietf_routing,
#endif
};

int num_components = sizeof(components) / sizeof(components[0]);
