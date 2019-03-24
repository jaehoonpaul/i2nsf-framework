/*
 * Copyright 2017 Tail-F Systems AB
 * Tail-F customers are permitted to redistribute in binary form, with
 * or without modification, for use in customer products.
 */

#include <errno.h>
#include <fcntl.h>
#include <malloc.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>

#include <confd.h>
#include <confd_maapi.h>
#include <confd_cdb.h>

#include "ietf-routing.h"
//#include "iana-if-type.h"

#include "linuxcfg_api.h"
#include "linuxcfg_util.h"
#include "ietf_routing_system.h"
#include "ietf_routing_subscriber.h"
#include "ietf_routing_provider.h"
#include "ietf_routing_active_route.h"
#include "ietf_routing_provider_utils.h"

static void setup(struct confd_daemon_ctx *dctx, int rsock);

static struct confd_data_cbs transpoints[] = {
    {   .callpoint = rt__callpointid_routing_state_dp,
        .get_elem = routing_get_elem,
        .get_next = routing_get_next,
        .get_case = routing_get_case,
        .exists_optional = routing_exists_optional
    },
    { .callpoint = "" }
};

static struct confd_action_cbs actionpoints[] = {
    { .actionpoint = rt__actionpointid_routing_state_active_route,
      .action = routing_active_route
    },
    { .actionpoint = "" }
};

static int init_data(struct confd_trans_ctx *tctx)
{
    TRACE_ENTRY("");
    route_data_ptr_set(tctx->t_opaque, routing_alloc_dp_data());
    TRACE_EXIT("");
    return CONFD_OK;
}

static void finish_data(struct confd_trans_ctx *tctx)
{
    struct system_route_data *ptr = route_data_ptr_get(tctx->t_opaque);
    TRACE_ENTRY("opaque_data=%p", ptr);
    routing_clean_opaque(ptr);
    route_data_ptr_set(tctx->t_opaque, NULL);
    ptr = route_data_ptr_get(tctx->t_opaque);
    TRACE_EXIT("opaque_data=%p", ptr);
}

const struct component ietf_routing = {
    NULL,         /* init */
    NULL,         /* setup0 */
    setup,        /* setup */
    NULL,         /* valpoints */
    transpoints,  /* transpoints */
    actionpoints, /* actionpoints */
    NULL,         /* init_validation */
    NULL,         /* stop_validation */
    init_data,    /* init_data */
    finish_data,  /* finish_data */
};

/* Setup this component on startup */
static void setup(struct confd_daemon_ctx *dctx, int rsock)
{
    /* Test
    const struct route_state_list *routes;
    get_routes(ipv4, &routes);
    free_routes(ipv4);
    get_routes(ipv6, &routes);
    free_routes(ipv6);
    struct route_state route;
    union route_addr addr;
    inet_pton(AF_INET, "16.0.6.33", &addr.ipv4_addr);
    get_active_route(ipv4, addr, &route);
    free_route_nexthops(&route);
    inet_pton(AF_INET6, "444c::33", &addr.ipv6_addr);
    get_active_route(ipv6, addr, &route);
    free_route_nexthops(&route);
    Test end */
    routing_subs_setup(dctx, rsock);
}
