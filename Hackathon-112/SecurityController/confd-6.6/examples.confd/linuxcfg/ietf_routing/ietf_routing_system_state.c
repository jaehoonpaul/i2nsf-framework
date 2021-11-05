/*
 * Copyright 2017 Tail-F Systems AB
 * Tail-F customers are permitted to redistribute in binary form, with
 * or without modification, for use in customer products.
 *
 * ietf_routing_system_state.c
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <linux/rtnetlink.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <confd.h> // needed by "linuxcfg_api.h"
#include "linuxcfg_api.h"
#include "ietf_routing_system.h"

// extern from ietf_routing_system.c
extern const char* get_ip_addr_string(const enum route_afi afi,
        const void* const ip_addr, char* addr);

static struct route_state_list *g_internal_routes_ipv4 = NULL;
static struct route_state_list *g_internal_routes_ipv6 = NULL;

struct nl_req {
    struct nlmsghdr hdr;
    //struct rtgenmsg genmsg;
    struct rtmsg msg;
    char buf[BUFSIZ];
};

static void trace_rtp(const struct rtmsg * const rtp)
{
    TRACE_ENTRY("");
    LOG("rtm_family=%i rtm_dst_len=%i rtm_src_len=%i rtm_tos=%i",
            rtp->rtm_family, rtp->rtm_dst_len, rtp->rtm_src_len, rtp->rtm_tos);
    LOG("rtm_table=%i rtm_protocol=%i rtm_scope=%i rtm_type=%i rtm_flags=%i",
            rtp->rtm_table, rtp->rtm_protocol, rtp->rtm_scope,
            rtp->rtm_type, rtp->rtm_flags);
    TRACE_EXIT("");
}

/**
 * trace route_state structure
 * @param route
 */
void trace_route_state(const struct route_state * const route)
{
    trace_route_key(&(route->afi_prefix));
    LOG("protocol=%i has_priority=%i priority=%u is_active=%i"
            " nexthop_type=%i",
            route->protocol, route->has_priority, route->priority,
            route->is_active, route->nexthop_type);
    if (route->nexthop_type == special) {
        LOG("nexthop.special=%i", route->nexthop.special);
    }
    char addr_str[128];
    char* iface = NULL;
    strcpy(addr_str, "n/a");
    if (route->nexthop_type == simple) {
        if (route->afi_prefix.afi == ipv4) {
            if (route->nexthop.ipv4.val.has_addr) {
                get_ip_addr_string(route->afi_prefix.afi,
                        &(route->nexthop.ipv4.val.addr), addr_str);
            }
            iface = route->nexthop.ipv4.val.iface;
        } else {
            if (route->nexthop.ipv6.val.has_addr) {
                get_ip_addr_string(route->afi_prefix.afi,
                        &(route->nexthop.ipv6.val.addr), addr_str);
            }
            iface = route->nexthop.ipv6.val.iface;
        }
        LOG("nexthop addr_str=%s iface=%s", addr_str, iface);
    }
    if (route->nexthop_type == list) {
        if (route->afi_prefix.afi == ipv4) {
            LOG("IPv4 list nexthop list");
            struct ipv4_nexthop_list *nh = route->nexthop.ipv4.list;
            while (nh) {
                strcpy(addr_str, "n/a");
                if (nh->val.has_addr) {
                    get_ip_addr_string(route->afi_prefix.afi, &(nh->val.addr),
                            addr_str);
                }
                LOG("   nexthop addr_str=%s iface=%s", addr_str, nh->val.iface);
                nh = nh->next;
            }
        } else {
            LOG("IPv6 list nexthop list");
            struct ipv6_nexthop_list *nh = route->nexthop.ipv6.list;
            while (nh) {
                strcpy(addr_str, "n/a");
                if (nh->val.has_addr) {
                    get_ip_addr_string(route->afi_prefix.afi, &(nh->val.addr),
                            addr_str);
                }
                LOG("   nexthop addr_str=%s iface=%s", addr_str, nh->val.iface);
                nh = nh->next;
            }
        }
    }
}

/**
 * Get interface name to given interface index
 * @param if_index interface index
 * @param ifName pointer to store ifname
 * @return NULL on error or pointer to ifname
 */
char* get_ifname(int if_index)
{
    TRACE_ENTRY("if_index=%i", if_index);
    char *ret_val = NULL;
    int fd;
    struct ifreq ifr;

    fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd == -1) {
        error("error on AF_INET socket");
        goto term;
    }

    ifr.ifr_ifindex = if_index;

    if (ioctl(fd, SIOCGIFNAME, &ifr, sizeof(ifr))) {
        error("error on ioctl");
        goto term;
    }
    ret_val = strdup(ifr.ifr_name);

    term:
    TRACE_EXIT("ret_val=%s", ret_val);
    return ret_val;
}

/**
 * Structure holding processing state
 */
struct route_process_state {
    int nexthop_found;
    int dst_addr_found;
    int nexthop_if_found;
    int nexthop_addr_found;
    int protocol_found;
};

static void trace_route_process_state(const struct route_process_state *
        const state)
{
    TRACE_ENTRY("");
    LOG("nexthop_found=%i dst_addr_found=%i nexthop_if_found=%i "
            "nexthop_addr_found=%i protocol_found=%i",
            state->nexthop_found, state->dst_addr_found,
            state->nexthop_if_found, state->nexthop_addr_found,
            state->protocol_found);
    TRACE_EXIT("");
}

enum routing_state_ret_val free_route_nexthops(struct route_state * route)
{
    TRACE_ENTRY("route->afi_prefix.afi=%i", route->afi_prefix.afi);
    int rv = state_ok;

    if (route->nexthop_type == simple) {
        LOG("freeing simple nexthop");
        if (route->afi_prefix.afi == ipv4) {
            free(route->nexthop.ipv4.val.iface);
        } else {
            free(route->nexthop.ipv6.val.iface);
        }
    }

    if (route->nexthop_type == list) {
        LOG("freeing list nexthop");
        if (route->afi_prefix.afi == ipv4) {
            struct ipv4_nexthop_list* list_next = route->nexthop.ipv4.list;
            struct ipv4_nexthop_list* list;
            for (list = list_next; list;) {
                free(list->val.iface);
                list_next = list->next;
                free(list);
                list = list_next;
            }
            route->nexthop.ipv4.list = NULL;
        } else {
            struct ipv6_nexthop_list* list_next = route->nexthop.ipv6.list;
            struct ipv6_nexthop_list* list;
            for (list = list_next; list;) {
                free(list->val.iface);
                list_next = list->next;
                free(list);
                list = list_next;
            }
        }
        route->nexthop.ipv6.list = NULL;
    }

    TRACE_EXIT("rv=%i", rv);
    return rv;

}

/**
 * Fill in route information found in struct rtmsg structure
 * @param rtp pointer to struct rtmsg
 * @param route pointer to route being processed
 * @param state pointer to route processing state
 * @return state_ok or state_err
 */
static enum routing_state_ret_val process_rtp(struct rtmsg *rtp,
        struct route_state *route, struct route_process_state* state)
{
    TRACE_ENTRY("");
    int rv = state_ok;
    route->afi_prefix.prefix_len = rtp->rtm_dst_len;
    trace_route_process_state(state);

    state->protocol_found = 1;
    switch (rtp->rtm_protocol) {
    case RTPROT_UNSPEC:
        route->protocol = prot_none;
        break;
    case RTPROT_KERNEL:
        case RTPROT_BOOT:
        route->protocol = prot_direct;
        break;
    case RTPROT_STATIC:
        route->protocol = prot_static;
        break;
    default:
        route->protocol = prot_other;
        break;
    }

    if (rtp->rtm_type == RTN_BLACKHOLE || rtp->rtm_type == RTN_UNREACHABLE ||
            rtp->rtm_type == RTN_PROHIBIT) {
        route->nexthop_type = special;
        state->nexthop_found = 1;
        switch (rtp->rtm_type) {
        case RTN_BLACKHOLE:
            route->nexthop.special = blackhole;
            break;
        case RTN_UNREACHABLE:
            route->nexthop.special = unreachable;
            break;
        case RTN_PROHIBIT:
            route->nexthop.special = prohibit;
            break;
        }
    }

    if (!state->nexthop_found) {
        if (route->afi_prefix.afi == ipv4) {
            route->nexthop.ipv4.val.has_addr = 0;
            route->nexthop.ipv4.val.iface = NULL;
        } else {
            route->nexthop.ipv6.val.has_addr = 0;
            route->nexthop.ipv4.val.iface = NULL;
        }
    }

    TRACE_EXIT("rv=%i", rv);
    return rv;
}

static enum routing_state_ret_val process_nexthop_val(const enum route_afi afi,
        const int is_oif, struct rtattr *rtap, void *val,
        struct route_process_state* state)
{
    TRACE_ENTRY("is_oif=%i", is_oif);
    int rv = state_err;
    char* if_name = NULL;

    if (!is_oif) {
        LOG("nexthop addr");
        state->nexthop_addr_found = 1;
    } else {
        LOG("nexthop if");
        if (!(if_name = get_ifname(*((int *) RTA_DATA(rtap))))) {
            error("failed to resolve interface name");
            goto term;
        }
        state->nexthop_if_found = 1;
    }

    if (afi == ipv4) {
        if (!is_oif) {
            ((struct ipv4_nexthop*) val)->has_addr = 1;
            ((struct ipv4_nexthop*) val)->addr =
                    *(struct in_addr*) RTA_DATA(rtap);
        } else {
            ((struct ipv4_nexthop*) val)->iface = if_name;
        }
    } else {
        if (!is_oif) {
            ((struct ipv6_nexthop*) val)->has_addr = 1;
            ((struct ipv6_nexthop*) val)->addr =
                    *(struct in6_addr*) RTA_DATA(rtap);
        } else {
            ((struct ipv6_nexthop*) val)->iface = if_name;
        }
    }

    rv = state_ok;
    term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

static enum routing_state_ret_val process_gateway_or_oif(const int is_oif,
        struct rtattr *rtap, struct route_state *route,
        struct route_process_state* state)
{
    TRACE_ENTRY("is_oif=%i", is_oif);
    int rv = state_err;
    int other_found;

    if (route->nexthop_type == special) {
        if ((route->afi_prefix.afi == ipv6) && is_oif) {
            LOG("Interface processing for special nexthop and IPV6 skipped.");
            goto term_ok;
        }
        error("Nexthop already processed!");
        goto term;
    }

    if (!is_oif) {
        LOG("Processing GATEWAY");
        other_found = state->nexthop_if_found;
    } else {
        LOG("Processing OIF");
        other_found = state->nexthop_addr_found;
    }

    if (!state->nexthop_found
            || (other_found && route->nexthop_type == simple)) {

        void *nexthop_val = (void*) &(route->nexthop.ipv4.val);
        if (route->afi_prefix.afi == ipv6) {
            nexthop_val = (void*) &(route->nexthop.ipv6.val);
        }

        if (state_ok
                != process_nexthop_val(route->afi_prefix.afi, is_oif, rtap,
                        nexthop_val, state)) {
            error("failed to process simple nexthop value");
            goto term;
        }
        route->nexthop_type = simple;
        state->nexthop_found = 1;
    } else {
        error(
                "Incorrect nexthop state state->nexthop_found=%i,"
                        " other_found=%i route->nexthop_type=%i",
                state->nexthop_found, other_found, route->nexthop_type);
        goto term;
    }

    term_ok:
    rv = state_ok;
    term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

/**
 * Process RTA_MULTIPATH - multiple nexthops for ipv4
 * This function has meaning only for ipv4 address family.
 * @param rtap pointer to struct rtattr
 * @param route pointer to route being processed
 * @param state pointer to route processing state
 * @return state_ok or state_err
 */
static enum routing_state_ret_val process_multipath(
        struct rtattr *rtap, struct route_state *route,
        struct route_process_state* state)
{
    TRACE_ENTRY("");
    int rv = state_err;

    if (state->nexthop_found) {
        error("Nexthop already found!");
        goto term;
    }

    if (route->afi_prefix.afi != ipv4) {
        error("MULTIPATH valid only for ipv4!");
        goto term;
    }

    struct rtnexthop *nh = RTA_DATA(rtap);
    int len = RTA_PAYLOAD(rtap);
    int i = 0;
    struct ipv4_nexthop_list* curr_nexthop = NULL;
    for (;;) {
        if (len < sizeof(*nh)) {
            break;
        }
        if (nh->rtnh_len > len) {
            break;
        }
        int attrlen = len - sizeof(struct rtnexthop);
        struct in_addr addr;
        LOG("Processing nexthp %i nh->rtnh_ifindex=%i", i++, nh->rtnh_ifindex);

        struct ipv4_nexthop_list *new_nh = malloc(
                sizeof(struct ipv4_nexthop_list));
        new_nh->next = NULL;
        if (!curr_nexthop) {
            curr_nexthop = new_nh;
            route->nexthop.ipv4.list = curr_nexthop;
        } else {
            curr_nexthop->next = new_nh;
            curr_nexthop = new_nh;
        }
        curr_nexthop->val.iface = get_ifname(nh->rtnh_ifindex);
        curr_nexthop->val.has_addr = 0;

        if (attrlen) { // search for gateway in attributes
            struct rtattr *attr = RTNH_DATA(nh);
            while (RTA_OK(attr, attrlen)) {
                if (attr->rta_type == RTA_GATEWAY) {
                    addr = *(struct in_addr*) RTA_DATA(attr);
                    LOG("Found address %#08x", addr);
                    curr_nexthop->val.addr = addr;
                    curr_nexthop->val.has_addr = 1;
                    break;
                }
                attr = RTA_NEXT(attr, attrlen);
            }
        }
        len -= NLMSG_ALIGN(nh->rtnh_len);
        nh = RTNH_NEXT(nh);
    }
    state->nexthop_found = 1;
    route->nexthop_type = list;

    rv = state_ok;
    term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

/**
 * Fill in route information found in struct rtattr structure
 * @param rtap pointer to struct rtattr
 * @param length of rtm payload
 * @param route pointer to route being processed
 * @param state pointer to route processing state
 * @return state_ok or state_err
 */
static enum routing_state_ret_val process_rtap(struct rtattr *rtap,
        ssize_t len,
        struct route_state *route, struct route_process_state* state)
{
    TRACE_ENTRY("");
    int rv = state_err;

    for (; RTA_OK(rtap, len); rtap = RTA_NEXT(rtap, len)) {
        trace_route_process_state(state);
        LOG("rtap->rta_type=%i", rtap->rta_type);
        switch (rtap->rta_type) {
        case RTA_DST:
            LOG("Processing DST");
            if (state->dst_addr_found) {
                error("Destination address already processed!");
                goto term;
            }
            if (route->afi_prefix.afi == ipv4) {
                route->afi_prefix.addr.ipv4_addr =
                        *(struct in_addr*) RTA_DATA(rtap);
            } else {
                route->afi_prefix.addr.ipv6_addr =
                        *(struct in6_addr*) RTA_DATA(rtap);
            }
            state->dst_addr_found = 1;
            break;
        case RTA_GATEWAY:
            if (state_ok != process_gateway_or_oif(0, rtap, route, state)) {
                goto term;
            }
            break;
        case RTA_PREFSRC:
            LOG("Processing SRCADDR (todo)");
            // TODO
            break;
        case RTA_MULTIPATH:
            LOG("Processing MULTIPATH"); // multiple nexthop
            if (state_ok != process_multipath(rtap, route, state)) {
                goto term;
            }
            break;
        case RTA_OIF:
            if (state_ok != process_gateway_or_oif(1, rtap, route, state)) {
                goto term;
            }
            break;
        case RTA_PRIORITY:
            LOG("Processing PRIORITY");
            route->priority = *((uint32_t *) RTA_DATA(rtap));
            route->has_priority = 1;
            break;
        }
    }
    rv = state_ok;
    term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

/**
 * If given IPv6 route already exists in list, append nexthop
 * to the existing one and delete this route.
 * @param route      route to search for
 * @param route_list route list to search in
 * @return 1 found in list, 0 not found
 */
static enum routing_state_ret_val process_possible_ipv6_nexthop_list(
        const struct route_state * const route,
        struct route_state_list *route_list)
{
    TRACE_ENTRY("route->afi_prefix.afi=%i", route->afi_prefix.afi);
    int rv = 0;
    struct route_state_list* list_item = route_list;

    while (list_item) {
        LOG("item->route.afi_prefix.afi=%i", list_item->route.afi_prefix.afi);
        assert(list_item->route.afi_prefix.afi == ipv6);
        assert(route->afi_prefix.afi == ipv6);

        int addr_same = !memcmp(&(list_item->route.afi_prefix.addr.ipv6_addr),
                &(route->afi_prefix.addr.ipv6_addr),
                sizeof(struct in6_addr));

        if (addr_same
                && list_item->route.afi_prefix.prefix_len
                        == route->afi_prefix.prefix_len) {

            LOG("Found IPv6 route for merge");
            if (list_item->route.nexthop_type == simple) {
                LOG("converting simple IPv6 nexthop to list nexthop");
                struct ipv6_nexthop_list* new_list = malloc(
                        sizeof(struct ipv6_nexthop_list));
                new_list->val = list_item->route.nexthop.ipv6.val;
                new_list->next = NULL;
                list_item->route.nexthop.ipv6.list = new_list;
                list_item->route.nexthop_type = list;
            }

            LOG("Appending IPv6 route to the nexthop list_elem");
            struct ipv6_nexthop_list* last = list_item->route.nexthop.ipv6.list;
            for (; last->next; last = last->next) {
                LOG("IPV6 nexthop list go next last->next=%p", last->next);
            }
            struct ipv6_nexthop_list* list_elem = malloc(
                    sizeof(struct ipv6_nexthop_list));
            list_elem->next = NULL;
            list_elem->val = route->nexthop.ipv6.val;
            last->next = list_elem;
            trace_route_state(&list_item->route);
            LOG("ipv6 nexthop list found, deleting"
                    " new route after merge");
            rv = 1;
            break;
        }

        list_item = list_item->next;
    }

    TRACE_EXIT("rv=%i", rv);
    return rv;
}

/**
 * Process route from rtmsg, alocate and return pointer to the route list item
 * @param rtp pointer to rtmsg with route information
 * @param length of rtm payload
 * @param route pointer to pointer for route to return
 * @return state_ok or state_err
 */
static enum routing_state_ret_val process_route_item(const enum route_afi afi,
        struct rtmsg *rtp, ssize_t len, struct route_state_list **route_item)
{
    TRACE_ENTRY("afi=%i", afi);
    int rv = state_err;
    *route_item = malloc(sizeof(struct route_state_list));
    bzero(*route_item, sizeof(struct route_state_list));

    struct route_process_state state;
    bzero(&state, sizeof(struct route_process_state));

    struct rtattr* rtap = (struct rtattr *) RTM_RTA(rtp);

    LOG("rtp=%p (*route)=%p", rtp, *route_item);
    trace_rtp(rtp);

// process attributes
    (*route_item)->route.afi_prefix.afi =
            rtp->rtm_family == AF_INET ? ipv4 : ipv6;
    if ((*route_item)->route.afi_prefix.afi != afi) {
        error("Route afi is incorrect!");
        goto term_err;
    }

    if (state_ok != (rv = process_rtp(rtp, &((*route_item)->route), &state))) {
        error("Failed to process rtp data!");
        goto term_err;
    }

    if (state_ok != (rv = process_rtap(rtap, len, &((*route_item)->route),
            &state))) {
        error("Failed to process rtap data!");
        goto term_err;
    }

    trace_route_process_state(&state);
    if (state.protocol_found /*&& state.dst_addr_found && state.nexthop_found
     && state.nexthop_if_found*/) {
        (*route_item)->next = NULL;
        rv = state_ok;
        goto term;
    } else {
        error("Not all required route parameters found!");
    }

    term_err:
    free_route_nexthops(&((*route_item)->route));
    free(*route_item);
    term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

/**
 * Read data from netlink socket and process routes
 * @param soc netlink socket
 * @param afi address family we are using
 * @param route_list pointer to pointer to route list to fill results into
 * @param maximum routes to read (-1 .. no limit)
 * @return state-err or state_ok
 */
static enum routing_state_ret_val read_nldata_and_process_routes(
        const int soc, const enum route_afi afi,
        struct route_state_list **route_list, int max_count)
{
    TRACE_ENTRY("afi=% max_count=%i", afi, max_count);
    int rv = state_err;

    const static int reply_len = BUFSIZ;
    char reply[reply_len];

    int msg_count = 0;
    struct route_state_list *last_route = *route_list;
    for (;;) {
        if (max_count != -1 && msg_count == max_count) {
            LOG("msg_count=%i reached max_count=%i", msg_count, max_count);
            break;
        }
        LOG("Reading data from netlink socket, msg_count=%i", msg_count);
        msg_count++;
        ssize_t bytes = recv(soc, &reply, reply_len, 0);
        LOG("Read bytes=%i", bytes);
        if (bytes < 0) {
            error("Error in recv");
            goto term;
        }
        if (bytes == 0) {
            warn("EOF on netlink socket\n");
            // TODO
            break;
        }

        struct nlmsghdr * nlp = (struct nlmsghdr*) (&reply);
        for (; NLMSG_OK(nlp, bytes); nlp = NLMSG_NEXT(nlp, bytes)) {
            LOG("nlp->nlmsg_type=%i", nlp->nlmsg_type);
            if (nlp->nlmsg_type == NLMSG_DONE) {
                LOG("Found NLMSG_DONE");
                break;
            }
            if (nlp->nlmsg_type == NLMSG_ERROR) {
                error("Error in msg");
                goto term;
            }
            if (nlp->nlmsg_type == RTM_NEWROUTE
                    || nlp->nlmsg_type == RTM_GETROUTE) {
                LOG("Processing RTM");
                struct rtmsg *rtp = (struct rtmsg *) NLMSG_DATA(nlp);
                struct route_state_list *route_item = NULL;
                LOG("rtp->rtm_table=%i", rtp->rtm_table);
                if (rtp->rtm_table == RT_TABLE_MAIN) {
                    if (nlp->nlmsg_type == RTM_NEWROUTE) {
                        LOG("Processing RTM_NEWROUTE");
                        process_route_item(afi, rtp, RTM_PAYLOAD(nlp),
                                &route_item);
                    }
                    trace_route_state(&route_item->route);
                    int ipv6_nexthop_list = 0;
                    if (afi == ipv6 &&
                            route_item->route.nexthop_type == simple) {
                        ipv6_nexthop_list =
                                process_possible_ipv6_nexthop_list(
                                        &(route_item->route), *route_list);
                    }
                    if (!ipv6_nexthop_list) {
                        LOG("Adding new route to the list");
                        if (!last_route) {
                            last_route = route_item;
                            *route_list = last_route;
                        } else {
                            last_route->next = route_item;
                            last_route = last_route->next;
                        }
                    } else {
                        // not free_route() as we copied all nexthops
                        // to existing route
                        free(route_item);
                    }
                }
            }
        }

        if ((nlp->nlmsg_type == NLMSG_DONE)) {
            LOG("Finishing reading on NLMSG_DONE");
            break;
        }
    }
    rv = state_ok;

    term:
    TRACE_EXIT("rv=%i", rv);
    return rv;

}

static void fill_req(const enum route_afi afi, struct nl_req *req)
{
    TRACE_ENTRY("afi=%i", afi);
    bzero(req, sizeof(struct nl_req));
    req->hdr.nlmsg_len = NLMSG_LENGTH(sizeof(struct rtmsg));
    req->hdr.nlmsg_type = RTM_GETROUTE;
    req->hdr.nlmsg_flags = NLM_F_REQUEST; //request routes
    req->msg.rtm_family = afi == ipv4 ? AF_INET : AF_INET6;
    TRACE_EXIT("");
}

/**
 * Create netlink socket and send request for routes
 * @param afi address family we are using
 * @return -1 o error, socket value > 0
 */
static int create_nlsocket_and_send_req(const enum route_afi afi)
{
    TRACE_ENTRY("");
    int soc = -1;

    soc = socket(AF_NETLINK, SOCK_RAW, NETLINK_ROUTE);
    if (soc > 0) {
        struct nl_req req;
        fill_req(afi, &req);
        req.hdr.nlmsg_flags |= NLM_F_DUMP; // dump all routes, not just one
        req.msg.rtm_table = RT_TABLE_MAIN;
        if (0 > send(soc, &req, sizeof(struct nl_req), 0)) {
            error("send request to netlink failed");
            close(soc);
            soc = -1;
            goto term;
        }
    } else {
        error("failed to create socket");
    }

    term:
    TRACE_EXIT("soc=%i", soc);
    return soc;
}

#define NLMSG_TAIL(nmsg) \
        ((struct rtattr *) (((void *) (nmsg)) + NLMSG_ALIGN((nmsg)->nlmsg_len)))

static enum routing_state_ret_val add_addr_attr(const enum route_afi afi,
        const union route_addr addr, struct nlmsghdr *hdr, int maxlen)
{
    TRACE_ENTRY("afi=%i hdr->nlmsg_len=%i", afi, hdr->nlmsg_len);
    int rv = state_err;
    int alen = afi == ipv4 ? 4 : 16;
    void *data =
            afi == ipv4 ? (void*) &addr.ipv4_addr : (void*) &addr.ipv6_addr;
    int len = RTA_LENGTH(alen);
    struct rtattr *rta;

    if (NLMSG_ALIGN(hdr->nlmsg_len) + RTA_ALIGN(len) > maxlen) {
        error("Address attributes exceeded  %d", maxlen);
        goto term;
    }
    rta = NLMSG_TAIL(hdr);
    rta->rta_type = RTA_DST;
    rta->rta_len = len;
    memcpy(RTA_DATA(rta), data, alen);
    char* dat = data;
    LOG("data (first 4 bytes) %02X:%02X:%02X:%02X", dat[0], dat[1], dat[2],
            dat[3]);
    hdr->nlmsg_len = NLMSG_ALIGN(hdr->nlmsg_len) + RTA_ALIGN(len);
    rv = state_ok;
    term:
    TRACE_EXIT("rv=%i hdr->nlmsg_len=%i", rv, hdr->nlmsg_len);
    return rv;
}

/**
 * Create netlink socket and send request for routes
 * @param afi address family we are using
 * @return -1 o error, socket value > 0
 */
static int create_nlsocket_and_send_req_with_addr_filter(
        const enum route_afi afi, const union route_addr addr)
{
    TRACE_ENTRY("afi=%i", afi);
    int soc = -1;

    soc = socket(AF_NETLINK, SOCK_RAW, NETLINK_ROUTE);
    if (soc > 0) {
        struct nl_req req;
        fill_req(afi, &req);
        req.msg.rtm_flags = RTM_F_LOOKUP_TABLE;
        if (state_ok != add_addr_attr(afi, addr, &req.hdr, sizeof(req))) {
            soc = -1;
            error("cannot add address attribute to netlink request");
            goto term;
        }
        if (0 > send(soc, &req, sizeof(struct nl_req), 0)) {
            error("send request to netlink failed");
            close(soc);
            soc = -1;
            goto term;
        }
    } else {
        error("failed to create socket");
    }

    term:
    TRACE_EXIT("soc=%i", soc);
    return soc;
}

/**
 * Get system routes for given address family via NETLINK.
 * Routes are filled to g_internal_routes list.
 * @param afi address family
 * @param route_list pointer to pointer to route list to fill results into
 * @return state_ok or state_err
 * @see get_routes
 */
static enum routing_state_ret_val get_routes_netlink(
        const enum route_afi afi, struct route_state_list **route_list)
{
    TRACE_ENTRY("afi=%i", afi);
    int soc, rv = state_err;

    if (*route_list) {
        error("internal_routes are not empty!");
        goto term;
    }

    if (0 >= (soc = create_nlsocket_and_send_req(afi))) {
        error("Failed to open socket or to send request!");
        goto term;
    }

    rv = read_nldata_and_process_routes(soc, afi, route_list, -1);

    close(soc);
    term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

enum routing_active_ret_val get_active_route_netlink(const enum route_afi afi,
        const union route_addr addr, struct route_state * route)
{
    TRACE_ENTRY("afi=%i", afi);
    int rv = state_active_err;
    int soc;
    struct route_state_list *route_list = NULL;

    if (0 >= (soc =
            create_nlsocket_and_send_req_with_addr_filter(afi, addr))) {
        error("Failed to open socket or to send request for active route!");
        goto term;
    }

    int rv_read = read_nldata_and_process_routes(soc, afi, &route_list, 1);
    close(soc);
    if (state_ok != rv_read) {
        error("FAILED to process routes while searching for active route");
        goto term;
    }

    if (route_list) {
        LOG("Route found");
        *route = route_list->route;
        if (route_list->next) {
            error("More than one active route found (first used)!");
            goto term;
        }
        // only free, do not delete possible allocated nexthops
        free(route_list);
        rv = state_active_ok;
    } else {
        warn("Active route not found!");
        rv = state_active_not_found;
    }

    term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

enum routing_state_ret_val get_routes(const enum route_afi afi,
        const struct route_state_list **routes)
{
    TRACE_ENTRY("afi=%i", afi);
    int rv = free_routes(afi);
    if (state_ok != rv) {
        error("Failed to free routes!");
        goto term;
    }

    struct route_state_list* internal_routes = NULL;
    if (afi == ipv4) {
        assert(g_internal_routes_ipv4 == NULL);
    } else {
        assert(g_internal_routes_ipv6 == NULL);
    }
    if (state_ok != (rv = get_routes_netlink(afi, &internal_routes))) {
        error("Failed to get routes via netlink!");
        goto term;
    }
    if (afi == ipv4) {
        g_internal_routes_ipv4 = internal_routes;
    } else {
        g_internal_routes_ipv6 = internal_routes;
    }
    *routes = internal_routes;
    int n_routes = 0;
    struct route_state_list* route = internal_routes;
    LOG("Route pointers for afi=%i", afi);
    for (; route; route = route->next) {
        n_routes++;
        LOG("route %i: %p (%lu)", n_routes, route, route);
    }
    term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

enum routing_state_ret_val free_routes(const enum route_afi afi)
{
    TRACE_ENTRY("");
    int rv = state_ok;

    struct route_state_list* internal_routes = NULL;
    if (afi == ipv4) {
        internal_routes = g_internal_routes_ipv4;
        g_internal_routes_ipv4 = NULL;
    } else {
        internal_routes = g_internal_routes_ipv6;
        g_internal_routes_ipv6 = NULL;
    }
    while (internal_routes) {
        struct route_state_list *item = internal_routes;
        internal_routes = internal_routes->next;
        if (state_err == (rv = free_route_nexthops(&item->route))) {
            error("Failed to free route_state");
            break;
        }
        free(item);
    }

    TRACE_EXIT("rv=%i", rv);
    return rv;
}

enum routing_active_ret_val get_active_route(const enum route_afi afi,
        const union route_addr addr, struct route_state * route)
{
    TRACE_ENTRY("afi=%i", afi);
    int rv = get_active_route_netlink(afi, addr, route);
    trace_route_state(route);
    TRACE_EXIT("rv=%i", rv);
    return rv;
}
