/*
 * Copyright 2017 Tail-F Systems AB
 * Tail-F customers are permitted to redistribute in binary form, with
 * or without modification, for use in customer products.
 *
 * ietf_routing_system.c
 */

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <confd.h> // needed by "linuxcfg_api.h"
#include "linuxcfg_api.h"
#include "ietf_routing_system.h"

static struct external_cmd ip_cmd = { .descr_name = "ip", .cmd = NULL, .args =
NULL, .env_var = "IP_CMD",
        #ifdef USER_IP_CMD
        .cc_macro = USER_IP_CMD,
#else
        .cc_macro = NULL,
        #endif
        .binary_name = "ip", .paths = { "/usr/sbin/ip", "/sbin/ip", NULL } };

#define IP_CMD       GET_EXT_CMD(ip_cmd)
#define ROUTE_STATIC_CMD   "route replace proto static"

const char* get_ip_addr_string(const enum route_afi afi,
        const void* const ip_addr, char* addr)
{
    TRACE_ENTRY("");
    const char* result = NULL;
    int af = AF_INET;
    socklen_t addr_len = INET_ADDRSTRLEN;
    if (afi == ipv6) {
        af = AF_INET6;
        addr_len = INET6_ADDRSTRLEN;
    }
    result = inet_ntop(af, ip_addr, addr, addr_len);

    if (result == NULL) {
        error("Cannot convert IP address to string (errno=%d)", errno);
    }
    TRACE_EXIT("ip=%s", addr);
    return result;
}

static const char* get_route_addr_string(const enum route_afi afi,
        const union route_addr * const route_addr, char* addr)
{
    TRACE_ENTRY("");
    const void* ip_addr = &route_addr->ipv4_addr;
    if (afi == ipv6) {
        ip_addr = &route_addr->ipv6_addr;
    }
    const char* result = get_ip_addr_string(afi, ip_addr, addr);
    TRACE_EXIT("ip=%s", addr);
    return result;
}

void trace_route_key(const struct route_key * const key)
{
    char addr_str[128];
    get_route_addr_string(key->afi, &key->addr, addr_str);

    LOG("key=%s/%d", addr_str, key->prefix_len);
}

static const char* get_ip_opt(const enum route_afi afi)
{
    TRACE_ENTRY("afi=%i", afi);
    static const char* ipv4_opt = "-4";
    static const char* ipv6_opt = "-6";
    const char* ip_opt = ipv4_opt;
    if (afi == ipv6) {
        ip_opt = ipv6_opt;
    }
    TRACE_EXIT("ip_opt=%s", ip_opt);
    return ip_opt;
}

static const char* get_simple_nexthop_string(const enum route_afi afi,
        const void* const val, char* nexthp)
{
    TRACE_ENTRY("");
    const char* result = NULL;
    int has_addr;
    const void* ip_addr = NULL;
    char* iface = NULL;

    if (afi == ipv4) {
        has_addr = ((const struct ipv4_nexthop*) val)->has_addr;
        ip_addr = &((const struct ipv4_nexthop*) val)->addr;
        iface = ((const struct ipv4_nexthop*) val)->iface;
    } else {
        has_addr = ((const struct ipv6_nexthop*) val)->has_addr;
        ip_addr = &((const struct ipv6_nexthop*) val)->addr;
        iface = ((const struct ipv6_nexthop*) val)->iface;
    }
    LOG("has_addr=%i ip_addr=%p iface=%s", has_addr, ip_addr, iface);

    if (has_addr) {
        strcat(nexthp, "via ");
        result = get_ip_addr_string(afi, ip_addr, nexthp + strlen(nexthp));
        if (result == NULL) {
            goto term;
        }
    }
    LOG("nexthp=%s", nexthp);

    if (iface) {
        if (has_addr) {
            strcat(nexthp, " ");
        }
        strcat(nexthp, "dev ");
        strcat(nexthp, iface);
    }

    result = nexthp;
    term:
    TRACE_EXIT("result=%s", nexthp);
    return result;
}

static const char* get_special_nextop_string(const enum special_nexthop nexthop)
{
    TRACE_ENTRY("nexthop=%i", nexthop);
    const char* nexthp = NULL;
    switch (nexthop) {
    case blackhole:
        nexthp = "blackhole";
        break;
    case unreachable:
        nexthp = "unreachable";
        break;
    case prohibit:
        nexthp = "prohibit";
        break;
    default:
        break;
    }
    TRACE_EXIT("nexthp=%s", nexthp);
    return nexthp;
}

static const char* extract_ip_addr_string(const struct route_key* const key,
        char* addr)
{
    TRACE_ENTRY("");
    const char* result = NULL;
    result = get_route_addr_string(key->afi, &key->addr, addr);
    if (result == NULL) {
        error("Cannot convert IP address to string (errno=%d)", errno);
    }
    TRACE_EXIT("ip=%s key.prefix_len=%d", addr, key->prefix_len);
    return result;
}

static enum routing_ret_val create_or_update_route_ip_cmd(
        const struct route* const route)
{
    TRACE_ENTRY("route->key.afi=%i", route->key.afi);
    int rv = err;
    const char* ip_opt = get_ip_opt(route->key.afi);
    char addr[INET6_ADDRSTRLEN]; //fits ipv4 and ipv6
    if (NULL == extract_ip_addr_string(&route->key, addr)) {
        error("Failed to convert IP address!");
        goto term;
    }
// special nexthop
    if (route->nexthop_type == special) {
        const char* nexthp = get_special_nextop_string(route->nexthop.special);
        if (0
                != run(NULL, 0, "%s %s %s %s %s/%d", IP_CMD, ip_opt,
                ROUTE_STATIC_CMD, nexthp, addr, route->key.prefix_len)) {
            LOG("Failed to run ip command!");
            goto term;
        }
        goto term_ok;
    }

    char nexthp[BUFSIZ];
    nexthp[0] = '\0';
    const char *result = NULL;
    const void* val = NULL;
//simple nexthop
    if (route->nexthop_type == simple) {
        val = &route->nexthop.ipv4.val;
        if (route->key.afi == ipv6) {
            val = &route->nexthop.ipv6.val;
        }
        result = get_simple_nexthop_string(route->key.afi, val, nexthp);

        if (NULL == result) {
            error("Failed to process nexthop string!");
            goto term;
        }
    }
//list nexthop
    if (route->nexthop_type == list) {
        LOG("Processing nexthop list element");
        void *list = route->nexthop.ipv4.list;
        if (route->key.afi == ipv6) {
            list = route->nexthop.ipv6.list;
        }
        while (list) {
            strcat(nexthp, "nexthop ");
            char* nexthp_pos = nexthp + strlen(nexthp);
            LOG("list=%p", list);
            if (route->key.afi == ipv6) {
                val = &((struct ipv6_nexthop_list*) list)->val;
                list = ((struct ipv6_nexthop_list*) list)->next;
            } else {
                val = &((struct ipv4_nexthop_list*) list)->val;
                list = ((struct ipv4_nexthop_list*) list)->next;
            }
            result = get_simple_nexthop_string(route->key.afi, val, nexthp_pos);
            if (NULL == result) {
                error("Failed to process nexthop list string!");
                goto term;
            }
            strcat(nexthp, " weight 1"); //add default weight
            if (list) {
                strcat(nexthp, " ");
            }
        }
    }

    if (0
            != run(NULL, 0, "%s %s %s %s/%d %s", IP_CMD, ip_opt,
                    ROUTE_STATIC_CMD, addr, route->key.prefix_len, nexthp)) {
        LOG("Failed to run ip command!");
        goto term;
    }

    term_ok: rv = ok;
    term:
    TRACE_EXIT("rv %i", rv);
    return rv;
}

static enum routing_ret_val delete_route_ip_cmd(const struct route_key* key,
        const enum special_nexthop nexthop)
{
    TRACE_ENTRY("");
    int rv = err;

    const char* ip_opt = get_ip_opt(key->afi);
    const char* nexthp = get_special_nextop_string(nexthop);
    char addr[INET6_ADDRSTRLEN]; //fits ipv4 and ipv6
    if (NULL == extract_ip_addr_string(key, addr)) {
        error("Failed to convert IP address!");
        goto term;
    }
    if (0
            != run(NULL, 0, "%s %s route del %s%s%s/%d", IP_CMD, ip_opt,
                    nexthp ? nexthp : "", nexthp ? " " : "", addr,
                    key->prefix_len)) {
        LOG("Failed to run ip command!");
    }

    rv = ok;

    term:
    TRACE_EXIT("rv %i", rv);
    return rv;
}

enum routing_ret_val create_or_update_route(const struct route* const route)
{
    TRACE_ENTRY("");
    int rv = err;
    // here we use ip command, other ways (netlink socket) can be used
    rv = create_or_update_route_ip_cmd(route);
    TRACE_EXIT("rv %i", rv);
    return rv;
}

enum routing_ret_val delete_route(const struct route_key* const key,
        const enum special_nexthop nexthop)
{
    TRACE_ENTRY("key->afi=%i", key->afi);

    int rv = delete_route_ip_cmd(key, nexthop);

    TRACE_EXIT("rv %i", rv);
    return rv;
}

// TESTS
// used for development and debugging, run with -m (dummy mode)
// monitor logfile trace output
void routing_system_dummy_ip_cmd_test()
{
    TRACE_ENTRY("");

    struct route_key key;
    struct route route;

//ipv4
    key.afi = ipv4;
    key.prefix_len = 24;
    inet_pton(AF_INET, "10.0.0.0", &key.addr.ipv4_addr);
    route.key = key;
// simple nexthop ipv4
    route.nexthop_type = simple;
    route.nexthop.ipv4.val.has_addr = 1;
    inet_pton(AF_INET, "10.0.2.15", &route.nexthop.ipv4.val.addr);
    route.nexthop.ipv4.val.iface = NULL; // no interface
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, none);
    route.nexthop.ipv4.val.iface = "enp0s3"; // no interface
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, none);
    route.nexthop.ipv4.val.has_addr = 0;
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, none);

//list nexthop ipv4
    route.nexthop_type = list;
    route.nexthop.ipv4.list = malloc(sizeof(struct ipv4_nexthop_list));
    route.nexthop.ipv4.list->val.has_addr = 1;
    inet_pton(AF_INET, "10.0.2.15", &route.nexthop.ipv4.list->val.addr);
    route.nexthop.ipv4.list->val.iface = NULL;
    route.nexthop.ipv4.list->next = NULL;
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, none);
    route.nexthop.ipv4.list->val.iface = "enp0s3";
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, none);
    route.nexthop.ipv4.list->val.has_addr = 0;
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, none);
    route.nexthop.ipv4.list->val.has_addr = 1;
    route.nexthop.ipv4.list->val.iface = NULL;
    route.nexthop.ipv4.list->next = malloc(sizeof(struct ipv4_nexthop_list));
    route.nexthop.ipv4.list->next->val.has_addr = 1;
    inet_pton(AF_INET, "10.0.2.16", &route.nexthop.ipv4.list->next->val.addr);
    route.nexthop.ipv4.list->next->val.iface = NULL;
    route.nexthop.ipv4.list->next->next = NULL;
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, none);
    route.nexthop.ipv4.list->next->val.iface = "enp0s3";
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, none);
    route.nexthop.ipv4.list->next->val.has_addr = 0;
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, none);
    free(route.nexthop.ipv4.list->next);
    free(route.nexthop.ipv4.list);

// special nexthop ipv4
    route.nexthop_type = special;
    route.nexthop.special = blackhole;
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, route.nexthop.special);
    route.nexthop.special = unreachable;
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, route.nexthop.special);
    route.nexthop.special = prohibit;
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, route.nexthop.special);

// ipv6
    key.afi = ipv6;
    key.prefix_len = 64;
    inet_pton(AF_INET6, "6667::", &key.addr.ipv4_addr);
    route.key = key;
    route.nexthop_type = simple;

// simple nexthop ipv6
    route.nexthop_type = simple;
    route.nexthop.ipv4.val.has_addr = 1;
    inet_pton(AF_INET6, "7777::2", &route.nexthop.ipv6.val.addr);
    route.nexthop.ipv6.val.iface = NULL; // no interface
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, none);
    route.nexthop.ipv6.val.iface = "enp0s3"; // no interface
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, none);
    route.nexthop.ipv6.val.has_addr = 0;
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, none);

//list nexthop ipv6
    route.nexthop_type = list;
    route.nexthop.ipv6.list = malloc(sizeof(struct ipv6_nexthop_list));
    route.nexthop.ipv6.list->val.has_addr = 1;
    inet_pton(AF_INET6, "7777::2", &route.nexthop.ipv6.list->val.addr);
    route.nexthop.ipv6.list->val.iface = NULL;
    route.nexthop.ipv6.list->next = NULL;
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, none);
    route.nexthop.ipv6.list->val.iface = "enp0s3";
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, none);
    route.nexthop.ipv6.list->val.has_addr = 0;
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, none);
    route.nexthop.ipv6.list->val.has_addr = 1;
    route.nexthop.ipv6.list->val.iface = NULL;
    route.nexthop.ipv6.list->next = malloc(sizeof(struct ipv6_nexthop_list));
    route.nexthop.ipv6.list->next->val.has_addr = 1;
    inet_pton(AF_INET6, "7777::3", &route.nexthop.ipv6.list->next->val.addr);
    route.nexthop.ipv6.list->next->val.iface = NULL;
    route.nexthop.ipv6.list->next->next = NULL;
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, none);
    route.nexthop.ipv6.list->next->val.iface = "enp0s3";
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, none);
    route.nexthop.ipv6.list->next->val.has_addr = 0;
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, none);
    free(route.nexthop.ipv6.list->next);
    free(route.nexthop.ipv6.list);

// special nexthop ipv6
    route.nexthop_type = special;
    route.nexthop.special = blackhole;
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, route.nexthop.special);
    route.nexthop.special = unreachable;
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, route.nexthop.special);
    route.nexthop.special = prohibit;
    create_or_update_route(&route);
    delete_route_ip_cmd(&key, route.nexthop.special);

    TRACE_EXIT("");
}
