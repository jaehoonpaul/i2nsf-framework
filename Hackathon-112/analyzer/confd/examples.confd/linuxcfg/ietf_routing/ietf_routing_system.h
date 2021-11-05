/*
 * Copyright 2017 Tail-F Systems AB
 * Tail-F customers are permitted to redistribute in binary form, with
 * or without modification, for use in customer products.
 *
 * ietf_routing_system.h
 *
 * interface towards system routing implementation corresponding to RFC8022
 *
 */

#ifndef IETF_ROUTING_IETF_ROUTING_SYSTEM_H_
#define IETF_ROUTING_IETF_ROUTING_SYSTEM_H_

#include <netinet/in.h>

enum nexthop_type {
    simple, special, list,
};

enum special_nexthop {
    none = -1, blackhole = 0, unreachable, prohibit,
    receive, // TODO not supported by linux??
};

enum route_afi {
    ipv4, ipv6,
};

struct ipv4_nexthop {
    int has_addr;
    struct in_addr addr;
    char* iface;
};

struct ipv4_nexthop_list {
    struct ipv4_nexthop val;
    struct ipv4_nexthop_list* next;
};

struct ipv6_nexthop {
    int has_addr;
    struct in6_addr addr;
    char* iface;
};

struct ipv6_nexthop_list {
    struct ipv6_nexthop val;
    struct ipv6_nexthop_list* next;
};

union nexthop {
    enum special_nexthop special;
    union ipv4 {
        struct ipv4_nexthop val;
        struct ipv4_nexthop_list* list;
    } ipv4;
    union ipv6 {
        struct ipv6_nexthop val;
        struct ipv6_nexthop_list* list;
    } ipv6;
};

union route_addr {
       struct in_addr ipv4_addr;
       struct in6_addr ipv6_addr;
};

struct route_key {
    enum route_afi afi;
    union route_addr addr;
    int32_t prefix_len;
};

/**
 * Route structure corresponding to the RFC8022 data model.
 */
struct route {
    struct route_key key;
    enum nexthop_type nexthop_type;
    union nexthop nexthop;
};

enum routing_ret_val {
    exist = -2, err = -1, ok = 0,
};

/**
 * Create route or update existing route in the system.
 * @param pointer to route parameters
 * @return ok or err
 */
enum routing_ret_val create_or_update_route(const struct route*);

/**
 * Delete route existing in the system
 * @param pointer to route key (address and prefix)
 * @param if route is associated with specian nexthop, the nexthop
 *         value or none
 * @return ok , err or exist (if route does not exist)
 */
enum routing_ret_val delete_route(const struct route_key*,
        enum special_nexthop);

/**
 *  State (operational) support
 */
enum routing_state_ret_val {
    state_err = -1, state_ok = 0
};

enum routing_active_ret_val {
    state_active_err = state_err,
    state_active_ok = state_ok,
    state_active_not_found,
};

enum routing_protocol {
    prot_none = 0, //RTPROT_UNSPEC
    prot_direct, //RTPROT_KERNEL, RTPROT_BOOT
    prot_static, //RTPROT_STATIC
    prot_other, //RTPROT_REDIRECT, other protocols
};

/**
 * Route information
 */
struct route_state {
    struct route_key afi_prefix;
    enum nexthop_type nexthop_type;
    union nexthop nexthop;
    int has_priority;
    uint32_t priority;
    int is_active;
    enum routing_protocol protocol;
};

/**
 * Route information
 */
struct route_state_list {
    struct route_state route;
    struct route_state_list *next; //next element or NULL
};

/**
 * Get system routes for given address family.
 * The caller does not own returned memory.
 * Next call frees memory allocated by previous call (even for different afi).
 * Memory can be also cleared by calling free_routes
 * @param afi address family
 * @param pointer to pointer for returned values (all routes have afi
 *        according to the first parameter)
 * @return state_ok or state_err
 * @see free_routes
 */
enum routing_state_ret_val get_routes(const enum route_afi afi,
        const struct route_state_list **);

/**
 * Free memory (if any) for routes from last call to get_routes
 * @param afi address family
 * @return state_ok or state_err
 * @see get_routes
 */
enum routing_state_ret_val free_routes(const enum route_afi afi);


/**
 * Get active system route for given route address.
 * The caller owns allocated memory for nexthops
 * and must free it with free_route_nexthops function.
 * @param afi address family
 * @param route address (ipv4 or ipv6 according to afi)
 * @param pointer to route object to fill, this route object
 *        must not have allocated any nexthop memory!
 * @return state_active_ok, state_active_err or state_active_not_found
 * @see free_route_nexthops
 */
enum routing_active_ret_val get_active_route(const enum route_afi,
        const union route_addr, struct route_state *);

/**
 * Free route (state) memory allocated for nexthops.
 * For simple nexthop it frees interface string.
 * For list nexthop it frees all nexthops in list and sets list to NULL.
 * @param pointer to route
 * @return state_ok or state_err
 */
enum routing_state_ret_val free_route_nexthops(struct route_state *);

/**
 * Function used for development tests (not used in production)
 */
void routing_system_dummy_ip_cmd_test();
void trace_route_key(const struct route_key * const key);
void trace_route_state(const struct route_state * const route);

#endif
