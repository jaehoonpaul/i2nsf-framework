#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <malloc.h>

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netinet/tcp.h>

#include <confd.h>
#include <confd_maapi.h>

#include "linuxcfg_api.h"
#include "caches.h"
#include "ipmibs.h"

#include "udp.h"
#include "UDP-MIB.h"
#define UDP_ELEM(el) _NSCONCAT(udp_mib, el)

#include "tcp.h"
#include "TCP-MIB.h"
#define TCP_ELEM(el) _NSCONCAT(tcp_mib, el)

#include "if.h"
#include "IF-MIB.h"
#define IF_ELEM(el) _NSCONCAT(if_mib, el)

#include "ip.h"
#include "IP-MIB.h"
#define IP_ELEM(el) _NSCONCAT(ip_mib, el)

#define ADDR_EQ(addr, v) (strlen(addr) == CONFD_GET_BUFSIZE(v) && \
                          strncmp(addr, CONFD_GET_CBUFPTR(v),     \
                                  CONFD_GET_BUFSIZE(v)) == 0)
#define BIN_ADDR_EQ(addr, v) (addr.addr_len == CONFD_GET_BINARY_SIZE(v) && \
                              memcmp(addr.address,                      \
                                     CONFD_GET_BINARY_PTR(v),           \
                                     addr.addr_len) == 0)
#define CONFD_SET_ADDR(v, addr) CONFD_SET_BINARY(v, addr.address, addr.addr_len)
static void ipmibs_setup0(struct confd_daemon_ctx *dctx,
                          struct confd_trans_ctx *tctx, int rsock);
static void ipmibs_setup(struct confd_daemon_ctx *dctx, int rsock);
static void ipmibs_init();

struct component ipmibs = {
    .init = ipmibs_init,
    .setup0 = ipmibs_setup0,
    .setup = ipmibs_setup
};

/*
UDP statistics tables
*/

CACHE_TABLE(udp_stats_t) udp_table;
CACHE_TABLE(udp_endpoint_list_t) udp_endpoints;

udp_endpoint_list_t *udp_ep_list;

static int udp_status_get_elem(struct confd_trans_ctx *tctx,
                               confd_hkeypath_t * kp);
static int udp_status_get_next(struct confd_trans_ctx *tctx,
                               confd_hkeypath_t * keypath, long next);

/*
  TCP stats tables
*/

static int tcp_status_get_elem(struct confd_trans_ctx *tctx,
                               confd_hkeypath_t * kp);
static int tcp_status_get_next(struct confd_trans_ctx *tctx,
                               confd_hkeypath_t * keypath, long next);

//struct TcpStats global_tcp_stats;

CACHE_TABLE(TCP_STATS) tcp_table;
CACHE_TABLE(TCP_ENDPOINT_STATS) tcp_endpoints;

TCP_ENDPOINT_STATS *tcp_ep_table;
TCP_STATS *tcp_st_table;


/*
IF stats tables
*/
CACHE_TABLE(if_entry_list_t) if_entries;

if_entry_list_t *if_entry_list;

static int if_status_get_elem(struct confd_trans_ctx *tctx,
                              confd_hkeypath_t * kp);
static int if_status_get_next(struct confd_trans_ctx *tctx,
                              confd_hkeypath_t * keypath, long next);

/*
 IP stats tables
*/

CACHE_TABLE(ip_basic_stats_t) ip_basic_stats;

CACHE_TABLE(ipDefaultRouterEntry_list_t) ip_def_router_stats;
CACHE_TABLE(ipv6ScopeZoneIndexEntry_list_t) ip6_scope_zone_idx_stats;
CACHE_TABLE(ipSystemStatsEntry_table_t) ip_system_stats;
CACHE_TABLE(ipIfStatsEntry_list_t) ip_if_stats;
CACHE_TABLE(ip_net_phys_list_t) ip_net_phys_entries;

CACHE_TABLE(ipAddressPrefixEntry_list_t) ip_addr_prefix_stats;
CACHE_TABLE(ipAddressEntry_list_t) ip_addr_stats;

CACHE_TABLE(icmpStatsEntry_list_t) ip_icmp_stats;
CACHE_TABLE(icmpMsgStatsEntry_list_t) ip_icmp_msg_stats;

CACHE_TABLE(ipv6RouterAdvertEntry_list_t) ip6_router_advert_stats;

ipv6ScopeZoneIndexEntry_list_t *ip6_scope_zone_idx_stats_list;
ipDefaultRouterEntry_list_t *ip_def_router_stats_list;
ipIfStatsEntry_list_t *ip_if_stats_list;
ipv6ScopeZoneIndexEntry_list_t *ip6_scope_zone_idx_stats_list;
ip_net_phys_list_t *ip_net_phys_list;
ipAddressPrefixEntry_list_t *ip_addr_prefix_stats_list;
ipAddressEntry_list_t *ip_addr_stats_list;

icmpStatsEntry_list_t *ip_icmp_stats_list;
icmpMsgStatsEntry_list_t *ip_icmp_msg_stats_list;

ipv6RouterAdvertEntry_list_t *ip6_router_advert_list;

static int ip_status_get_elem(struct confd_trans_ctx *tctx,
                              confd_hkeypath_t * kp);
static int ipv4_if_status_get_elem(struct confd_trans_ctx *tctx,
                                   confd_hkeypath_t * kp);
static int ipv6_if_status_get_elem(struct confd_trans_ctx *tctx,
                                   confd_hkeypath_t * kp);

static int ip_def_router_get_elem(struct confd_trans_ctx *tctx,
                                  confd_hkeypath_t * kp);
static int ip_def_router_get_next(struct confd_trans_ctx *tctx,
                                  confd_hkeypath_t * kp, long next);

static int ip_traf_system_stats_get_elem(struct confd_trans_ctx *tctx,
                                         confd_hkeypath_t * kp);
static int ip_traf_system_stats_get_next(struct confd_trans_ctx *tctx,
                                         confd_hkeypath_t * kp, long next);

static int ip_traf_if_stats_get_elem(struct confd_trans_ctx *tctx,
                                     confd_hkeypath_t * kp);
static int ip_traf_if_stats_get_next(struct confd_trans_ctx *tctx,
                                     confd_hkeypath_t * kp, long next);

static int ip_traf_status_get_elem(struct confd_trans_ctx *tctx,
                                   confd_hkeypath_t * kp);
static int ip_traf_status_get_next(struct confd_trans_ctx *tctx,
                                   confd_hkeypath_t * kp, long next);

static int ip6_scope_zone_idx_get_elem(struct confd_trans_ctx *tctx,
                                       confd_hkeypath_t * kp);
static int ip6_scope_zone_idx_get_next(struct confd_trans_ctx *tctx,
                                       confd_hkeypath_t * kp, long next);

static int ip_net_phys_get_elem(struct confd_trans_ctx *tctx,
                                confd_hkeypath_t * kp);
static int ip_net_phys_get_next(struct confd_trans_ctx *tctx,
                                confd_hkeypath_t * kp, long next);

static int ip_address_prefix_get_elem(struct confd_trans_ctx *tctx,
                                      confd_hkeypath_t * kp);
static int ip_address_prefix_get_next(struct confd_trans_ctx *tctx,
                                      confd_hkeypath_t * kp, long next);

static int ip_address_get_elem(struct confd_trans_ctx *tctx,
                               confd_hkeypath_t * kp);
static int ip_address_get_next(struct confd_trans_ctx *tctx,
                               confd_hkeypath_t * kp, long next);

static int ip_icmp_stat_get_elem(struct confd_trans_ctx *tctx,
                                 confd_hkeypath_t * kp);
static int ip_icmp_stat_get_next(struct confd_trans_ctx *tctx,
                                 confd_hkeypath_t * kp, long next);

static int ip_icmp_msg_stat_get_elem(struct confd_trans_ctx *tctx,
                                     confd_hkeypath_t * kp);
static int ip_icmp_msg_stat_get_next(struct confd_trans_ctx *tctx,
                                     confd_hkeypath_t * kp, long next);

static int ip6_router_advert_get_elem(struct confd_trans_ctx *tctx,
                                      confd_hkeypath_t * kp);
static int ip6_router_advert_get_next(struct confd_trans_ctx *tctx,
                                      confd_hkeypath_t * kp, long next);

static int dummy_get_elem(struct confd_trans_ctx *tctx,
                          confd_hkeypath_t * kp);
static int dummy_get_next(struct confd_trans_ctx *tctx,
                          confd_hkeypath_t * kp, long next);

/*
General ConfD/LinuxCfg related stuff
*/

void ipmibs_init()
{
    LOG("Starting");

    INIT_TABLE(&udp_table, update_udp_stats);
    memset(_GET_TABLE(&udp_table), 0, sizeof(udp_stats_t));
    INIT_LIST_TABLE(&udp_endpoints, update_udp_endpoints,
                    compare_udp_endpoints);
    INIT_TABLE(&tcp_table, update_tcp_table);
    INIT_TABLE(&tcp_endpoints, update_tcp_stats);
    INIT_LIST(&_GET_TABLE(&tcp_endpoints)->connections,
              compare_tcp_connections);
    INIT_LIST(&_GET_TABLE(&tcp_endpoints)->listeners, compare_tcp_listeners);

    INIT_LIST_TABLE(&if_entries, update_if_entries, compare_if_entries);

    INIT_TABLE(&ip_basic_stats, update_ip_basic_stats);

    INIT_LIST_TABLE(&ip_def_router_stats, update_ip_def_router,
                    compare_ip_def_router);
    INIT_LIST_TABLE(&ip6_scope_zone_idx_stats, update_ip6_scope_zone_index,
                    compare_ip6_scope_zone_index);

    memset(&ip_system_stats, 0, sizeof(ip_system_stats));
    INIT_TABLE(&ip_system_stats, update_ip_system_stats);
    INIT_LIST_TABLE(&ip_if_stats, update_ip_if_stats, compare_ip_if_stats);
    INIT_LIST_TABLE(&ip_net_phys_entries, update_net_phys_list,
                    compare_net_phys_entries);
    INIT_LIST_TABLE(&ip_addr_prefix_stats, update_ip_address_prefix,
                    compare_ip_address_prefix);
    INIT_LIST_TABLE(&ip_addr_stats, update_ip_address, compare_ip_address);
    INIT_LIST_TABLE(&ip_icmp_stats, update_ip_icmp_stats,
                    compare_ip_icmp_stats);
    INIT_LIST_TABLE(&ip_icmp_msg_stats, update_ip_icmp_msg_stats,
                    compare_ip_icmp_msg_stats);

    INIT_LIST_TABLE(&ip6_router_advert_stats, update_ipv6_router_advert_stats,
                    compare_ipv6_router_advert_stats);
}

void ipmibs_setup(struct confd_daemon_ctx *dctx, int rsock)
{
    // do nothing
}

#define CALLPOINT_ID(pref, id) _CONCAT(pref, __callpointid_, id)
#define IP_CALLPOINT(id) CALLPOINT_ID(ip_mib, id)
#define CALLPOINT_DECL(mib, cp_id, get_elem_cb, get_next_cb)    \
    {.callpoint = CALLPOINT_ID(mib, cp_id),                        \
            .get_elem = get_elem_cb,                            \
            .get_next = get_next_cb}
#define CALLPOINT_IP_DECL(cp_id, get_elem_cb, get_next_cb)    \
    CALLPOINT_DECL(ip_mib, cp_id, get_elem_cb, get_next_cb)
void ipmibs_setup0(struct confd_daemon_ctx *dctx, struct confd_trans_ctx *tctx,
                   int rsock)
{
    struct confd_data_cbs dcb[] = {
        CALLPOINT_DECL(udp_mib, udp_status, udp_status_get_elem,
                       udp_status_get_next),
        CALLPOINT_DECL(tcp_mib, tcp_status, tcp_status_get_elem,
                       tcp_status_get_next),
        CALLPOINT_DECL(if_mib, if_status, if_status_get_elem,
                       if_status_get_next),
        CALLPOINT_IP_DECL(ipv4_if_status, ipv4_if_status_get_elem,
                          if_status_get_next),
        CALLPOINT_IP_DECL(ipv6_if_status, ipv6_if_status_get_elem,
                          if_status_get_next),
        CALLPOINT_IP_DECL(ip6_scope_zone_idx, ip6_scope_zone_idx_get_elem,
                          ip6_scope_zone_idx_get_next),
        CALLPOINT_IP_DECL(ip_def_router, ip_def_router_get_elem,
                          ip_def_router_get_next),
        CALLPOINT_IP_DECL(ip_status, ip_status_get_elem, NULL),
        CALLPOINT_IP_DECL(ip_traf_status, ip_traf_status_get_elem,
                          ip_traf_status_get_next),
        CALLPOINT_IP_DECL(ip_traf_system_stats, ip_traf_system_stats_get_elem,
                          ip_traf_system_stats_get_next),
        CALLPOINT_IP_DECL(ip_traf_if_stats, ip_traf_if_stats_get_elem,
                          ip_traf_if_stats_get_next),
        CALLPOINT_IP_DECL(ip_net_to_physical_stats, ip_net_phys_get_elem,
                          ip_net_phys_get_next),
        CALLPOINT_IP_DECL(ip_address_prefix_status, ip_address_prefix_get_elem,
                          ip_address_prefix_get_next),
        CALLPOINT_IP_DECL(ip_address_status, ip_address_get_elem,
                          ip_address_get_next),
        CALLPOINT_IP_DECL(ipv6_router_status, ip6_router_advert_get_elem,
                          ip6_router_advert_get_next),
        CALLPOINT_IP_DECL(icmp_status, dummy_get_elem, dummy_get_next),
        CALLPOINT_IP_DECL(icmp_stat_status, ip_icmp_stat_get_elem,
                          ip_icmp_stat_get_next),
        CALLPOINT_IP_DECL(icmp_msg_status, ip_icmp_msg_stat_get_elem,
                          ip_icmp_msg_stat_get_next),
    };
    int i;

    /* Register data callbacks */
    for (i = 0; i < sizeof(dcb) / sizeof(struct confd_data_cbs); i++) {
        if (confd_register_data_cb(dctx, &dcb[i]) != CONFD_OK)
            fail("Failed to register callpoint \"%s\"", dcb[i].callpoint);
    }
}

static int get_udp_endpoint_stats(confd_hkeypath_t * kp, confd_value_t * v)
{
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);
    int ix;
    udp_endpointstats_t *stats = NULL;

    for (ix = 0; ix < LIST_SIZE(udp_ep_list); ix++) {
        confd_value_t *v_p = kp->v[1], *v_pp;

        stats = GET_ENTRY(udp_ep_list, ix);
        if ((v_pp = v_p++,
             stats->udpEndpointLocalAddressType == CONFD_GET_ENUM_HASH(v_pp))
            && (v_pp = v_p++, BIN_ADDR_EQ(stats->udpEndpointLocalAddress, v_pp))
            && (v_pp = v_p++,
                stats->udpEndpointLocalPort == CONFD_GET_UINT32(v_pp))
            && (v_pp = v_p++,
                stats->udpEndpointRemoteAddressType ==
                CONFD_GET_ENUM_HASH(v_pp))
            && (v_pp = v_p++,
                BIN_ADDR_EQ(stats->udpEndpointRemoteAddress, v_pp))
            && (v_pp = v_p++,
                stats->udpEndpointRemotePort == CONFD_GET_UINT32(v_pp))
            && (v_pp = v_p++,
                stats->udpEndpointInstance == CONFD_GET_UINT32(v_pp)))
            break;
    }
    if (stats == NULL)
        return CONFD_ERR;

    switch (tag) {
    case UDP_ELEM(udpEndpointProcess):
        CONFD_SET_UINT32(v, stats->udpEndpointProcess);
        break;
    default:
        error("%s - Unknown xmltag: %s", __FUNCTION__, confd_hash2str(tag));
        return CONFD_ERR;
    }
    return CONFD_OK;
}

static int get_udp_global_stats(confd_hkeypath_t * kp, confd_value_t * v)
{
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);
    udp_stats_t *stats = GET_TABLE(&udp_table);

    switch (tag) {
    case UDP_ELEM(udpInDatagrams):
        CONFD_SET_UINT32(v, stats->udpInDatagrams);
        break;
    case UDP_ELEM(udpNoPorts):
        CONFD_SET_UINT32(v, stats->udpNoPorts);
        break;
    case UDP_ELEM(udpInErrors):
        CONFD_SET_UINT32(v, stats->udpInErrors);
        break;
    case UDP_ELEM(udpOutDatagrams):
        CONFD_SET_UINT32(v, stats->udpOutDatagrams);
        break;
    case UDP_ELEM(udpHCInDatagrams):
        CONFD_SET_UINT64(v, stats->udpHCInDatagrams);
        break;
    case UDP_ELEM(udpHCOutDatagrams):
        CONFD_SET_UINT64(v, stats->udpHCOutDatagrams);
        break;
    default:
        error("Unknown xmltag: %s", confd_hash2str(tag));
        return CONFD_ERR;
    }
    return CONFD_OK;
}


int udp_status_get_elem(struct confd_trans_ctx *tctx, confd_hkeypath_t * kp)
{
    confd_value_t v;
    int rv;

    if (kp->len > 3 && kp->v[2][0].type == C_XMLTAG
        && CONFD_GET_XMLTAG(&kp->v[2][0]) == UDP_ELEM(udpEndpointEntry))
        rv = get_udp_endpoint_stats(kp, &v);
    else
        rv = get_udp_global_stats(kp, &v);

    if (rv == CONFD_OK)
        confd_data_reply_value(tctx, &v);
    return rv;
}

int udp_status_get_next(struct confd_trans_ctx *tctx,
                        confd_hkeypath_t * keypath, long next)
{
    confd_value_t v[7], *v_p = v;

    if (next == -1) {
        udp_ep_list = GET_TABLE(&udp_endpoints);
        next = 0;
    }
    if (next >= LIST_SIZE(udp_ep_list))
        confd_data_reply_next_key(tctx, NULL, 0, 0);
    else {
        udp_endpointstats_t *stats = GET_ENTRY(udp_ep_list, next);

        CONFD_SET_ENUM_HASH(v_p++, stats->udpEndpointLocalAddressType);
        CONFD_SET_ADDR(v_p++, stats->udpEndpointLocalAddress);
        CONFD_SET_UINT32(v_p++, stats->udpEndpointLocalPort);
        CONFD_SET_ENUM_HASH(v_p++, stats->udpEndpointRemoteAddressType);
        CONFD_SET_ADDR(v_p++, stats->udpEndpointRemoteAddress);
        CONFD_SET_UINT32(v_p++, stats->udpEndpointRemotePort);
        CONFD_SET_UINT32(v_p++, stats->udpEndpointInstance);
        confd_data_reply_next_key(tctx, v, v_p - v, next + 1);
    }
    return CONFD_OK;
}

static int tcp_get_listener_elem(confd_hkeypath_t * kp, confd_value_t * v,
                          TCP_LISTENER_LIST * listeners)
{
    TCP_LISTENER *listener;
    int i;
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);

    FOR_ENTRIES(listeners, listener, i) {
        confd_value_t *v_p = kp->v[1], *v_pp;

        if (v_pp = v_p++,
            listener->tcpListenerLocalAddressType != CONFD_GET_ENUM_HASH(v_pp))
            continue;
        if (v_pp = v_p++, !BIN_ADDR_EQ(listener->tcpListenerLocalAddress, v_pp))
            continue;
        if (v_pp = v_p++,
            listener->tcpListenerLocalPort != CONFD_GET_UINT32(v_pp))
            continue;
        break;
    }
    switch (tag) {
    case TCP_ELEM(tcpListenerProcess):
        CONFD_SET_UINT32(v, listener->tcpListenerProcess);
        break;
    }
    return CONFD_OK;
}

static int tcp_get_connection_elem(confd_hkeypath_t * kp, confd_value_t * v,
                            TCP_CONNECTION_LIST * connections)
{
    int i;
    TCP_CONNECTION *connection;
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);
    u_int32_t state = 1;

    FOR_ENTRIES(connections, connection, i) {
        confd_value_t *v_p = kp->v[1], *v_pp;

        if (v_pp =
            v_p++,
            connection->tcpConnectionLocalAddressType !=
            CONFD_GET_ENUM_HASH(v_pp))
            continue;
        if (v_pp = v_p++,
            !BIN_ADDR_EQ(connection->tcpConnectionLocalAddress, v_pp))
            continue;
        if (v_pp = v_p++,
            connection->tcpConnectionLocalPort != CONFD_GET_UINT32(v_pp))
            continue;
        if (v_pp = v_p++,
            connection->tcpConnectionRemAddressType !=
            CONFD_GET_ENUM_HASH(v_pp))
            continue;
        if (v_pp =
            v_p++,
            !BIN_ADDR_EQ(connection->tcpConnectionRemAddress, v_pp))
            continue;
        if (v_pp = v_p++,
            connection->tcpConnectionRemPort != CONFD_GET_UINT32(v_pp))
            continue;
        break;
    }

    switch (tag) {
    case TCP_ELEM(tcpConnectionState):
        switch (connection->tcpConnectionState) {
        case TCP_ESTABLISHED:
            state = TCP_ELEM(established);
            break;
        case TCP_SYN_SENT:
            state = TCP_ELEM(synSent);
            break;
        case TCP_SYN_RECV:
            state = TCP_ELEM(synReceived);
            break;
        case TCP_FIN_WAIT1:
            state = TCP_ELEM(finWait1);
            break;
        case TCP_FIN_WAIT2:
            state = TCP_ELEM(finWait2);
            break;
        case TCP_TIME_WAIT:
            state = TCP_ELEM(timeWait);
            break;
        case TCP_CLOSE:
            state = TCP_ELEM(closed);
            break;
        case TCP_CLOSE_WAIT:
            state = TCP_ELEM(closeWait);
            break;
        case TCP_LAST_ACK:
            state = TCP_ELEM(lastAck);
            break;
        case TCP_LISTEN:
            state = TCP_ELEM(listen);
            break;
        case TCP_CLOSING:
            state = TCP_ELEM(closing);
            break;
        }
        CONFD_SET_ENUM_HASH(v, state);
        break;
    case TCP_ELEM(tcpConnectionProcess):
        CONFD_SET_UINT32(v, connection->tcpConnectionProcess);
        break;
    }
    return CONFD_OK;
}

static int tcp_get_global_stats_elem(confd_hkeypath_t * kp, confd_value_t * v)
{
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);
    TCP_STATS *stats = GET_TABLE(&tcp_table);

    switch (tag) {
    case TCP_ELEM(tcpRtoAlgorithm):
        CONFD_SET_ENUM_HASH(v, stats->tcpRtoAlgorithm);
        break;
    case TCP_ELEM(tcpRtoMin):
        CONFD_SET_INT32(v, stats->tcpRtoMin);
        break;
    case TCP_ELEM(tcpRtoMax):
        CONFD_SET_INT32(v, stats->tcpRtoMax);
        break;
    case TCP_ELEM(tcpMaxConn):
        CONFD_SET_INT32(v, stats->tcpMaxConn);
        break;
    case TCP_ELEM(tcpActiveOpens):
        CONFD_SET_UINT32(v, stats->tcpActiveOpens);
        break;
    case TCP_ELEM(tcpPassiveOpens):
        CONFD_SET_UINT32(v, stats->tcpPassiveOpens);
        break;
    case TCP_ELEM(tcpAttemptFails):
        CONFD_SET_UINT32(v, stats->tcpAttemptFails);
        break;
    case TCP_ELEM(tcpEstabResets):
        CONFD_SET_UINT32(v, stats->tcpEstabResets);
        break;
    case TCP_ELEM(tcpCurrEstab):
        CONFD_SET_UINT32(v, stats->tcpCurrEstab);
        break;
    case TCP_ELEM(tcpInSegs):
        CONFD_SET_UINT32(v, stats->tcpInSegs);
        break;
    case TCP_ELEM(tcpOutSegs):
        CONFD_SET_UINT32(v, stats->tcpOutSegs);
        break;
    case TCP_ELEM(tcpRetransSegs):
        CONFD_SET_UINT32(v, stats->tcpRetransSegs);
        break;
    case TCP_ELEM(tcpInErrs):
        CONFD_SET_UINT32(v, stats->tcpInErrs);
        break;
    case TCP_ELEM(tcpOutRsts):
        CONFD_SET_UINT32(v, stats->tcpOutRsts);
        break;
    case TCP_ELEM(tcpHCInSegs):
        CONFD_SET_UINT64(v, stats->tcpHCInSegs);
        break;
    case TCP_ELEM(tcpHCOutSegs):
        CONFD_SET_UINT64(v, stats->tcpHCOutSegs);
        break;

    default:
        error("Unknown xmltag: %s", confd_hash2str(tag));
        return CONFD_ERR;
    }
    return CONFD_OK;
}


int tcp_status_get_elem(struct confd_trans_ctx *tctx, confd_hkeypath_t * kp)
{
    confd_value_t v;
    int rv;

    if (kp->len > 3
        && CONFD_GET_XMLTAG(&kp->v[2][0]) == TCP_ELEM(tcpListenerEntry))
        rv = tcp_get_listener_elem(kp, &v, &tcp_ep_table->listeners);
    else if (kp->len > 3
             && CONFD_GET_XMLTAG(&kp->v[2][0]) == TCP_ELEM(tcpConnectionEntry))
        rv = tcp_get_connection_elem(kp, &v, &tcp_ep_table->connections);
    else
        rv = tcp_get_global_stats_elem(kp, &v);

    confd_data_reply_value(tctx, &v);
    return rv;
}


int tcp_status_get_next(struct confd_trans_ctx *tctx,
                        confd_hkeypath_t * keypath, long next)
{
    confd_value_t v[6], *v_p = v;

    if (next == -1) {
        tcp_ep_table = GET_TABLE(&tcp_endpoints);
        next = 0;
    }
    if (CONFD_GET_XMLTAG(&keypath->v[0][0]) == TCP_ELEM(tcpListenerEntry)) {
        if (next >= LIST_SIZE(&tcp_ep_table->listeners))
            confd_data_reply_next_key(tctx, NULL, 0, 0);
        else {
            TCP_LISTENER *listener = GET_ENTRY(&tcp_ep_table->listeners, next);

            CONFD_SET_ENUM_HASH(v_p++, listener->tcpListenerLocalAddressType);
            CONFD_SET_ADDR(v_p++, listener->tcpListenerLocalAddress);
            CONFD_SET_UINT32(v_p++, listener->tcpListenerLocalPort);
            confd_data_reply_next_key(tctx, v, v_p - v, next + 1);
        }
    } else {
        if (next >= LIST_SIZE(&tcp_ep_table->connections))
            confd_data_reply_next_key(tctx, NULL, 0, 0);
        else {
            TCP_CONNECTION *connection =
                GET_ENTRY(&tcp_ep_table->connections, next);
            CONFD_SET_ENUM_HASH(v_p++,
                                connection->tcpConnectionLocalAddressType);
            CONFD_SET_ADDR(v_p++, connection->tcpConnectionLocalAddress);
            CONFD_SET_UINT32(v_p++, connection->tcpConnectionLocalPort);
            CONFD_SET_ENUM_HASH(v_p++, connection->tcpConnectionRemAddressType);
            CONFD_SET_ADDR(v_p++, connection->tcpConnectionRemAddress);
            CONFD_SET_UINT32(v_p++, connection->tcpConnectionRemPort);
            confd_data_reply_next_key(tctx, v, v_p - v, next + 1);
        }
    }
    return CONFD_OK;
}

/**************** IF Acquisition Begin ****************/
if_entry_t *get_if_entry_n(char *ifName) {
    int ix;
    if_entry_t *entry = NULL;

    if_entry_list = GET_TABLE(&if_entries);

    for (ix = 0; ix < LIST_SIZE(if_entry_list); ix++) {
        entry = GET_ENTRY(if_entry_list, ix);
        if (strcmp(entry->ifName, ifName) == 0)
            return entry;
    }

    return NULL;
}

if_entry_list_t *get_if_entry_list() {
    if_entry_list = GET_TABLE(&if_entries);
    return if_entry_list;
}

static int get_if_endpoint_stats(struct confd_trans_ctx *ctx,
                                 confd_hkeypath_t * kp, confd_value_t * v)
{
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);
    int ix;
    if_entry_t *entry = NULL;

    if_entry_list = GET_TABLE(&if_entries);

    for (ix = 0; ix < LIST_SIZE(if_entry_list); ix++) {
        confd_value_t *v_p = kp->v[1], *v_pp;

        entry = GET_ENTRY(if_entry_list, ix);
        if ((v_pp = v_p++, entry->ifIndex == CONFD_GET_INT32(v_pp))) {
            break;
        }
    }
    if (entry == NULL)
        return CONFD_ERR;

    switch (tag) {

    case IF_ELEM(ifIndex):
        CONFD_SET_INT32(v, entry->ifIndex);
        break;
    case IF_ELEM(ifDescr):
        CONFD_SET_STR(v, entry->ifDescr);
        break;
    case IF_ELEM(ifType):
        CONFD_SET_ENUM_HASH(v, entry->ifType);
        break;
    case IF_ELEM(ifMtu):
        CONFD_SET_INT32(v, entry->ifMtu);
        break;
    case IF_ELEM(ifSpeed):
        CONFD_SET_UINT32(v, entry->ifSpeed);
        break;
    case IF_ELEM(ifPhysAddress):
        CONFD_SET_BINARY(v, entry->ifPhysAddress, entry->ifPhysAddress_len);
        break;
    case IF_ELEM(ifAdminStatus):
        CONFD_SET_ENUM_HASH(v, entry->ifAdminStatus);
        break;
    case IF_ELEM(ifOperStatus):
        CONFD_SET_ENUM_HASH(v, entry->ifOperStatus);
        break;
    case IF_ELEM(ifLastChange):
        CONFD_SET_UINT32(v, entry->ifLastChange);
        break;
    case IF_ELEM(ifInOctets):
        CONFD_SET_UINT32(v, (uint32_t) entry->ifInOctets);
        break;
    case IF_ELEM(ifInUcastPkts):
        CONFD_SET_UINT32(v, (uint32_t) entry->ifInUcastPkts);
        break;
    case IF_ELEM(ifInDiscards):
        CONFD_SET_UINT32(v, (uint32_t) entry->ifInDiscards);
        break;
    case IF_ELEM(ifInErrors):
        CONFD_SET_UINT32(v, (uint32_t) entry->ifInErrors);
        break;
    case IF_ELEM(ifInUnknownProtos):
        CONFD_SET_UINT32(v, entry->ifInUnknownProtos);
        break;
    case IF_ELEM(ifOutOctets):
        CONFD_SET_UINT32(v, (uint32_t) entry->ifOutOctets);
        break;
    case IF_ELEM(ifOutUcastPkts):
        CONFD_SET_UINT32(v, (uint32_t) entry->ifOutUcastPkts);
        break;
    case IF_ELEM(ifOutDiscards):
        CONFD_SET_UINT32(v, (uint32_t) entry->ifOutDiscards);
        break;
    case IF_ELEM(ifOutErrors):
        CONFD_SET_UINT32(v, (uint32_t) entry->ifOutErrors);
        break;
    case IF_ELEM(ifName):
        CONFD_SET_STR(v, entry->ifName);
        break;
    case IF_ELEM(ifInMulticastPkts):
        CONFD_SET_UINT32(v, (uint32_t) entry->ifInMulticastPkts);
        break;
    case IF_ELEM(ifInBroadcastPkts):
        CONFD_SET_UINT32(v, (uint32_t) entry->ifInBroadcastPkts);
        break;
    case IF_ELEM(ifOutMulticastPkts):
        CONFD_SET_UINT32(v, (uint32_t) entry->ifOutMulticastPkts);
        break;
    case IF_ELEM(ifOutBroadcastPkts):
        CONFD_SET_UINT32(v, (uint32_t) entry->ifOutBroadcastPkts);
        break;
    case IF_ELEM(ifHCInOctets):
        CONFD_SET_UINT64(v, entry->ifHCInOctets);
        break;
    case IF_ELEM(ifHCInUcastPkts):
        CONFD_SET_UINT64(v, entry->ifHCInUcastPkts);
        break;
    case IF_ELEM(ifHCInMulticastPkts):
        CONFD_SET_UINT64(v, entry->ifHCInMulticastPkts);
        break;
    case IF_ELEM(ifHCInBroadcastPkts):
        CONFD_SET_UINT64(v, entry->ifHCInBroadcastPkts);
        break;
    case IF_ELEM(ifHCOutOctets):
        CONFD_SET_UINT64(v, entry->ifHCOutOctets);
        break;
    case IF_ELEM(ifHCOutUcastPkts):
        CONFD_SET_UINT64(v, entry->ifHCOutUcastPkts);
        break;
    case IF_ELEM(ifHCOutMulticastPkts):
        CONFD_SET_UINT64(v, entry->ifHCOutMulticastPkts);
        break;
    case IF_ELEM(ifHCOutBroadcastPkts):
        CONFD_SET_UINT64(v, entry->ifHCOutBroadcastPkts);
        break;
    case IF_ELEM(ifLinkUpDownTrapEnable):
        CONFD_SET_ENUM_HASH(v, entry->ifLinkUpDownTrapEnable);
        break;
    case IF_ELEM(ifHighSpeed):
        CONFD_SET_UINT32(v, entry->ifHighSpeed);
        break;
    case IF_ELEM(ifPromiscuousMode):
        CONFD_SET_ENUM_HASH(v, entry->ifPromiscuousMode);
        break;
    case IF_ELEM(ifConnectorPresent):
        CONFD_SET_ENUM_HASH(v, entry->ifConnectorPresent);
        break;
    case IF_ELEM(ifAlias):
#ifdef LINUXCFG_INTERFACES
        if (maapi_get_elem(get_msock_from_opaq(ctx->t_opaque), ctx->thandle, v,
                           "/interfaces/interface{%s}/description",
                           entry->ifName) != CONFD_OK) {
            CONFD_SET_STR(v, entry->ifAlias);
        }

        break;
#else
        CONFD_SET_STR(v, entry->ifAlias);
#endif
        break;
    case IF_ELEM(ifCounterDiscontinuityTime):
        CONFD_SET_UINT32(v, entry->ifCounterDiscontinuityTime);
        break;
    default:
        error("%s - Unknown xmltag: %s", __FUNCTION__, confd_hash2str(tag));
        return CONFD_ERR;
    }
    return CONFD_OK;
}


static int get_if_global_stats(confd_hkeypath_t * kp, confd_value_t * v)
{
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);
    if_entry_list_t *ift = GET_TABLE(&if_entries);

    switch (tag) {
    case IF_ELEM(ifNumber):
        CONFD_SET_INT32(v, LIST_SIZE(ift));
        break;
    case IF_ELEM(ifTableLastChange):
    case IF_ELEM(ifStackLastChange):
        CONFD_SET_UINT32(v, 0);
        break;
    default:
        error("Unknown xmltag: %s", confd_hash2str(tag));
        return CONFD_ERR;
    }
    return CONFD_OK;
}


int if_status_get_elem(struct confd_trans_ctx *tctx, confd_hkeypath_t * kp)
{
    confd_value_t v;
    int rv;

    if (kp->len > 3 && kp->v[2][0].type == C_XMLTAG
        && (CONFD_GET_XMLTAG(&kp->v[2][0]) == IF_ELEM(ifEntry)
            || CONFD_GET_XMLTAG(&kp->v[2][0]) == IF_ELEM(ifXEntry)))
        rv = get_if_endpoint_stats(tctx, kp, &v);
    else
        rv = get_if_global_stats(kp, &v);

    if (rv == CONFD_OK)
        confd_data_reply_value(tctx, &v);
    return rv;
}

static int if_status_get_next(struct confd_trans_ctx *tctx,
                              confd_hkeypath_t * keypath,
                              long next)
{
    confd_value_t v[20], *v_p = v;

    if (next == -1) {
        if_entry_list = GET_TABLE(&if_entries);
        next = 0;
    }
    if (next >= LIST_SIZE(if_entry_list)) {
        confd_data_reply_next_key(tctx, NULL, 0, 0);
    } else {
        if_entry_t *entry = GET_ENTRY(if_entry_list, next);

        CONFD_SET_INT32(v_p++, entry->ifIndex);
        confd_data_reply_next_key(tctx, v, v_p - v, next + 1);
    }
    return CONFD_OK;
}

int if_status_get_next_name(struct confd_trans_ctx *tctx,
                            confd_hkeypath_t * keypath,
                            long next)
{
    confd_value_t v[20], *v_p = v;

    if (next == -1) {
        if_entry_list = GET_TABLE_NOCACHE(&if_entries);
        next = 0;
    }
    if (next >= LIST_SIZE(if_entry_list)) {
        confd_data_reply_next_key(tctx, NULL, 0, 0);
    } else {
        if_entry_t *entry = GET_ENTRY(if_entry_list, next);

        CONFD_SET_STR(v_p++, entry->ifName);
        confd_data_reply_next_key(tctx, v, v_p - v, next + 1);
    }
    return CONFD_OK;
}

/**************** IF Acquisition End ****************/

/** IP-MIB stuff **/

int ip_status_get_elem(struct confd_trans_ctx *tctx, confd_hkeypath_t * kp)
{
    confd_value_t v;
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);

    switch (tag) {
    case IP_ELEM(ipForwarding):
        CONFD_SET_ENUM_HASH(&v,
                            GET_TABLE(&ip_basic_stats)->ipForwarding == 1
                            ? IP_ELEM(forwarding) : IP_ELEM(notForwarding));
        break;
    case IP_ELEM(ipDefaultTTL):
        CONFD_SET_INT32(&v, GET_TABLE(&ip_basic_stats)->ipDefaultTTL);
        break;
    case IP_ELEM(ipReasmTimeout):
        CONFD_SET_INT32(&v, GET_TABLE(&ip_basic_stats)->ipReasmTimeout);
        break;
    case IP_ELEM(ipv6IpForwarding):
        CONFD_SET_ENUM_HASH(&v,
                            GET_TABLE(&ip_basic_stats)->ipv6IpForwarding == 1
                            ? IP_ELEM(forwarding) : IP_ELEM(notForwarding));
        break;
    case IP_ELEM(ipv6IpDefaultHopLimit):
        CONFD_SET_INT32(&v, GET_TABLE(&ip_basic_stats)->ipv6IpDefaultHopLimit);
        break;
    case IP_ELEM(ipv4InterfaceTableLastChange):
    case IP_ELEM(ipv6InterfaceTableLastChange):
        CONFD_SET_UINT32(&v, 0);
        break;
    case IP_ELEM(ipAddressSpinLock):
    case IP_ELEM(ipv6RouterAdvertSpinLock):
        CONFD_SET_INT32(&v, 0);
        break;
    }
    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}

int ipv4_if_status_get_elem(struct confd_trans_ctx *tctx, confd_hkeypath_t * kp)
{
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);
    int ix;
    if_entry_t *entry;
    confd_value_t v;

    FOR_ENTRIES(if_entry_list, entry, ix) {
        if (entry->ifIndex == CONFD_GET_INT32(&kp->v[1][0]))
            break;
    }
    switch (tag) {
    case IP_ELEM(ipv4InterfaceReasmMaxSize):
        CONFD_SET_INT32(&v, 65535);
        break;
    case IP_ELEM(ipv4InterfaceEnableStatus):
        CONFD_SET_ENUM_HASH(&v,
                            entry->ifAdminStatus == 2
                            ? IP_ELEM(down) : IP_ELEM(up));
        break;
    case IP_ELEM(ipv4InterfaceRetransmitTime):
        CONFD_SET_UINT32(&v, entry->if4RetransmitTime);
        break;
    }
    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}

int ipv6_if_status_get_elem(struct confd_trans_ctx *tctx, confd_hkeypath_t * kp)
{
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);
    int ix;
    if_entry_t *entry;
    u_int8_t *addr = NULL;
    confd_value_t v;

    FOR_ENTRIES(if_entry_list, entry, ix) {
        if (entry->ifIndex == CONFD_GET_INT32(&kp->v[1][0]))
            break;
    }
    switch (tag) {
    case IP_ELEM(ipv6InterfaceReasmMaxSize):
        CONFD_SET_UINT32(&v, 65535);
        break;
    case IP_ELEM(ipv6InterfaceIdentifier):
        if (entry->ifPhysAddress_len == 0)
            CONFD_SET_BINARY(&v, malloc(1), 0);
        else {
            addr = malloc(entry->ifPhysAddress_len + 2);
            memcpy(addr, entry->ifPhysAddress, 3);
            addr[0] ^= 2;
            memcpy(addr + 5, entry->ifPhysAddress + 3, 3);
            addr[3] = 0xff;
            addr[4] = 0xfe;
            CONFD_SET_BINARY(&v, addr, entry->ifPhysAddress_len + 2);
        }
        break;
    case IP_ELEM(ipv6InterfaceEnableStatus):
        CONFD_SET_ENUM_HASH(&v,
                            entry->ifAdminStatus ==
                            2 ? IP_ELEM(down) : IP_ELEM(up));
        break;
    case IP_ELEM(ipv6InterfaceReachableTime):
        CONFD_SET_UINT32(&v, entry->if6ReachableTime);
        break;
    case IP_ELEM(ipv6InterfaceRetransmitTime):
        CONFD_SET_UINT32(&v, entry->if6RetransmitTime);
        break;
    case IP_ELEM(ipv6InterfaceForwarding):
        CONFD_SET_ENUM_HASH(&v,
                            entry->if6Forwarding ==
                            1 ? IP_ELEM(forwarding) : IP_ELEM(notForwarding));
        break;
    }
    confd_data_reply_value(tctx, &v);
    if (addr != NULL)
        free(addr);
    return CONFD_OK;
}

int ip_traf_system_stats_get_elem(struct confd_trans_ctx *tctx,
                                  confd_hkeypath_t * kp)
{
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);
    ipSystemStatsEntry_t *entry;
    confd_value_t v;

    entry = CONFD_GET_ENUM_HASH(&kp->v[1][0]) == INET(ipv4) ?
        &GET_TABLE(&ip_system_stats)->entries[0] :
        &GET_TABLE(&ip_system_stats)->entries[1];
    switch (tag) {
    case IP_ELEM(ipSystemStatsIPVersion):
        CONFD_SET_ENUM_HASH(&v, entry->ipSystemStatsIPVersion);
        break;
    case IP_ELEM(ipSystemStatsInReceives):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsInReceives);
        break;
    case IP_ELEM(ipSystemStatsHCInReceives):
        CONFD_SET_UINT64(&v, entry->ipSystemStatsHCInReceives);
        break;
    case IP_ELEM(ipSystemStatsInOctets):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsInOctets);
        break;
    case IP_ELEM(ipSystemStatsHCInOctets):
        CONFD_SET_UINT64(&v, entry->ipSystemStatsHCInOctets);
        break;
    case IP_ELEM(ipSystemStatsInHdrErrors):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsInHdrErrors);
        break;
    case IP_ELEM(ipSystemStatsInNoRoutes):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsInNoRoutes);
        break;
    case IP_ELEM(ipSystemStatsInAddrErrors):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsInAddrErrors);
        break;
    case IP_ELEM(ipSystemStatsInUnknownProtos):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsInUnknownProtos);
        break;
    case IP_ELEM(ipSystemStatsInTruncatedPkts):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsInTruncatedPkts);
        break;
    case IP_ELEM(ipSystemStatsInForwDatagrams):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsInForwDatagrams);
        break;
    case IP_ELEM(ipSystemStatsHCInForwDatagrams):
        CONFD_SET_UINT64(&v, entry->ipSystemStatsInForwDatagrams);
        break;
    case IP_ELEM(ipSystemStatsReasmReqds):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsReasmReqds);
        break;
    case IP_ELEM(ipSystemStatsReasmOKs):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsReasmOKs);
        break;
    case IP_ELEM(ipSystemStatsReasmFails):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsReasmFails);
        break;
    case IP_ELEM(ipSystemStatsInDiscards):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsInDiscards);
        break;
    case IP_ELEM(ipSystemStatsInDelivers):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsInDelivers);
        break;
    case IP_ELEM(ipSystemStatsHCInDelivers):
        CONFD_SET_UINT64(&v, entry->ipSystemStatsHCInDelivers);
        break;
    case IP_ELEM(ipSystemStatsOutRequests):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsOutRequests);
        break;
    case IP_ELEM(ipSystemStatsHCOutRequests):
        CONFD_SET_UINT64(&v, entry->ipSystemStatsHCOutRequests);
        break;
    case IP_ELEM(ipSystemStatsOutNoRoutes):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsOutNoRoutes);
        break;
    case IP_ELEM(ipSystemStatsOutForwDatagrams):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsOutForwDatagrams);
        break;
    case IP_ELEM(ipSystemStatsHCOutForwDatagrams):
        CONFD_SET_UINT64(&v, entry->ipSystemStatsHCOutForwDatagrams);
        break;
    case IP_ELEM(ipSystemStatsOutDiscards):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsOutDiscards);
        break;
    case IP_ELEM(ipSystemStatsOutFragReqds):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsOutFragReqds);
        break;
    case IP_ELEM(ipSystemStatsOutFragOKs):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsOutFragOKs);
        break;
    case IP_ELEM(ipSystemStatsOutFragFails):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsOutFragFails);
        break;
    case IP_ELEM(ipSystemStatsOutFragCreates):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsOutFragCreates);
        break;
    case IP_ELEM(ipSystemStatsOutTransmits):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsOutTransmits);
        break;
    case IP_ELEM(ipSystemStatsHCOutTransmits):
        CONFD_SET_UINT64(&v, entry->ipSystemStatsOutTransmits);
        break;
    case IP_ELEM(ipSystemStatsOutOctets):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsOutOctets);
        break;
    case IP_ELEM(ipSystemStatsHCOutOctets):
        CONFD_SET_UINT64(&v, entry->ipSystemStatsHCOutOctets);
        break;
    case IP_ELEM(ipSystemStatsInMcastPkts):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsInMcastPkts);
        break;
    case IP_ELEM(ipSystemStatsHCInMcastPkts):
        CONFD_SET_UINT64(&v, entry->ipSystemStatsHCInMcastPkts);
        break;
    case IP_ELEM(ipSystemStatsInMcastOctets):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsInMcastOctets);
        break;
    case IP_ELEM(ipSystemStatsHCInMcastOctets):
        CONFD_SET_UINT64(&v, entry->ipSystemStatsHCInMcastOctets);
        break;
    case IP_ELEM(ipSystemStatsOutMcastPkts):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsOutMcastPkts);
        break;
    case IP_ELEM(ipSystemStatsHCOutMcastPkts):
        CONFD_SET_UINT64(&v, entry->ipSystemStatsHCOutMcastPkts);
        break;
    case IP_ELEM(ipSystemStatsOutMcastOctets):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsOutMcastOctets);
        break;
    case IP_ELEM(ipSystemStatsHCOutMcastOctets):
        CONFD_SET_UINT64(&v, entry->ipSystemStatsHCOutMcastOctets);
        break;
    case IP_ELEM(ipSystemStatsInBcastPkts):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsInBcastPkts);
        break;
    case IP_ELEM(ipSystemStatsHCInBcastPkts):
        CONFD_SET_UINT64(&v, entry->ipSystemStatsHCInBcastPkts);
        break;
    case IP_ELEM(ipSystemStatsOutBcastPkts):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsOutBcastPkts);
        break;
    case IP_ELEM(ipSystemStatsHCOutBcastPkts):
        CONFD_SET_UINT64(&v, entry->ipSystemStatsHCOutBcastPkts);
        break;
    case IP_ELEM(ipSystemStatsDiscontinuityTime):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipSystemStatsDiscontinuityTime);
        break;
    case IP_ELEM(ipSystemStatsRefreshRate):
        CONFD_SET_UINT32(&v, (UPDATE_INTERVAL + 1) * 1000);
        break;
    }
    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}
int ip_traf_system_stats_get_next(struct confd_trans_ctx *tctx,
                                  confd_hkeypath_t * kp, long next)
{
    confd_value_t v;

    CONFD_SET_ENUM_HASH(&v, next == -1 ? INET(ipv4) : INET(ipv6));
    if (next == 1) {
        confd_data_reply_next_key(tctx, NULL, 0, 0);
    } else {
        confd_data_reply_next_key(tctx, &v, 1, next + 1);
    }
    return CONFD_OK;
}

int ip_traf_if_stats_get_elem(struct confd_trans_ctx *tctx,
                              confd_hkeypath_t * kp)
{
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);
    int ix;
    ipIfStatsEntry_t *entry;
    confd_value_t v;

    FOR_ENTRIES(ip_if_stats_list, entry, ix) {
        confd_value_t *v_p = kp->v[1], *v_pp;

        if ((v_pp = v_p++,
             entry->ipIfStatsIPVersion == CONFD_GET_ENUM_HASH(v_pp))
            && (v_pp = v_p++,
                entry->ipIfStatsIfIndex == CONFD_GET_INT32(v_pp))) {
            break;
        }
    }
    switch (tag) {
    case IP_ELEM(ipIfStatsIPVersion):
        CONFD_SET_ENUM_HASH(&v, entry->ipIfStatsIPVersion);
        break;
    case IP_ELEM(ipIfStatsIfIndex):
        CONFD_SET_INT32(&v, (uint32_t) entry->ipIfStatsIfIndex);
        break;
    case IP_ELEM(ipIfStatsInReceives):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsInReceives);
        break;
    case IP_ELEM(ipIfStatsHCInReceives):
        CONFD_SET_UINT64(&v, entry->ipIfStatsHCInReceives);
        break;
    case IP_ELEM(ipIfStatsInOctets):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsInOctets);
        break;
    case IP_ELEM(ipIfStatsHCInOctets):
        CONFD_SET_UINT64(&v, entry->ipIfStatsInOctets);
        break;
    case IP_ELEM(ipIfStatsInHdrErrors):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsInHdrErrors);
        break;
    case IP_ELEM(ipIfStatsInNoRoutes):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsInNoRoutes);
        break;
    case IP_ELEM(ipIfStatsInAddrErrors):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsInAddrErrors);
        break;
    case IP_ELEM(ipIfStatsInUnknownProtos):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsInUnknownProtos);
        break;
    case IP_ELEM(ipIfStatsInTruncatedPkts):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsInTruncatedPkts);
        break;
    case IP_ELEM(ipIfStatsInForwDatagrams):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsInForwDatagrams);
        break;
    case IP_ELEM(ipIfStatsHCInForwDatagrams):
        CONFD_SET_UINT64(&v, entry->ipIfStatsInForwDatagrams);
        break;
    case IP_ELEM(ipIfStatsReasmReqds):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsReasmReqds);
        break;
    case IP_ELEM(ipIfStatsReasmOKs):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsReasmOKs);
        break;
    case IP_ELEM(ipIfStatsReasmFails):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsReasmFails);
        break;
    case IP_ELEM(ipIfStatsInDiscards):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsInDiscards);
        break;
    case IP_ELEM(ipIfStatsInDelivers):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsInDelivers);
        break;
    case IP_ELEM(ipIfStatsHCInDelivers):
        CONFD_SET_UINT64(&v, entry->ipIfStatsHCInDelivers);
        break;
    case IP_ELEM(ipIfStatsOutRequests):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsOutRequests);
        break;
    case IP_ELEM(ipIfStatsHCOutRequests):
        CONFD_SET_UINT64(&v, entry->ipIfStatsHCOutRequests);
        break;
    case IP_ELEM(ipIfStatsOutForwDatagrams):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsOutForwDatagrams);
        break;
    case IP_ELEM(ipIfStatsHCOutForwDatagrams):
        CONFD_SET_UINT64(&v, entry->ipIfStatsHCOutForwDatagrams);
        break;
    case IP_ELEM(ipIfStatsOutDiscards):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsOutDiscards);
        break;
    case IP_ELEM(ipIfStatsOutFragReqds):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsOutFragReqds);
        break;
    case IP_ELEM(ipIfStatsOutFragOKs):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsOutFragOKs);
        break;
    case IP_ELEM(ipIfStatsOutFragFails):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsOutFragFails);
        break;
    case IP_ELEM(ipIfStatsOutFragCreates):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsOutFragCreates);
        break;
    case IP_ELEM(ipIfStatsOutTransmits):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsOutTransmits);
        break;
    case IP_ELEM(ipIfStatsHCOutTransmits):
        CONFD_SET_UINT64(&v, entry->ipIfStatsOutTransmits);
        break;
    case IP_ELEM(ipIfStatsOutOctets):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsOutOctets);
        break;
    case IP_ELEM(ipIfStatsHCOutOctets):
        CONFD_SET_UINT64(&v, entry->ipIfStatsOutOctets);
        break;
    case IP_ELEM(ipIfStatsInMcastPkts):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsInMcastPkts);
        break;
    case IP_ELEM(ipIfStatsHCInMcastPkts):
        CONFD_SET_UINT64(&v, entry->ipIfStatsHCInMcastPkts);
        break;
    case IP_ELEM(ipIfStatsInMcastOctets):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsInMcastOctets);
        break;
    case IP_ELEM(ipIfStatsHCInMcastOctets):
        CONFD_SET_UINT64(&v, entry->ipIfStatsInMcastOctets);
        break;
    case IP_ELEM(ipIfStatsOutMcastPkts):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsOutMcastPkts);
        break;
    case IP_ELEM(ipIfStatsHCOutMcastPkts):
        CONFD_SET_UINT64(&v, entry->ipIfStatsHCOutMcastPkts);
        break;
    case IP_ELEM(ipIfStatsOutMcastOctets):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsOutMcastOctets);
        break;
    case IP_ELEM(ipIfStatsHCOutMcastOctets):
        CONFD_SET_UINT64(&v, entry->ipIfStatsOutMcastOctets);
        break;
    case IP_ELEM(ipIfStatsInBcastPkts):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsInBcastPkts);
        break;
    case IP_ELEM(ipIfStatsHCInBcastPkts):
        CONFD_SET_UINT64(&v, entry->ipIfStatsInBcastPkts);
        break;
    case IP_ELEM(ipIfStatsOutBcastPkts):
        CONFD_SET_UINT32(&v, (uint32_t) entry->ipIfStatsOutBcastPkts);
        break;
    case IP_ELEM(ipIfStatsHCOutBcastPkts):
        CONFD_SET_UINT64(&v, entry->ipIfStatsOutBcastPkts);
        break;
    case IP_ELEM(ipIfStatsDiscontinuityTime):
        CONFD_SET_UINT32(&v, 0);
        break;
    case IP_ELEM(ipIfStatsRefreshRate):
        CONFD_SET_UINT32(&v, (UPDATE_INTERVAL + 1) * 1000);
        break;
    }
    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}

int ip_traf_if_stats_get_next(struct confd_trans_ctx *tctx,
                              confd_hkeypath_t * kp, long next)
{
    confd_value_t v[8], *v_p = v;

    if (next == -1) {
        ip_if_stats_list = GET_TABLE(&ip_if_stats);
        next = 0;
    }
    if (next >= LIST_SIZE(ip_if_stats_list)) {
        confd_data_reply_next_key(tctx, NULL, 0, 0);
    } else {
        ipIfStatsEntry_t *entry = GET_ENTRY(ip_if_stats_list, next);

        CONFD_SET_ENUM_HASH(v_p++, entry->ipIfStatsIPVersion);
        CONFD_SET_INT32(v_p++, entry->ipIfStatsIfIndex);
        confd_data_reply_next_key(tctx, v, v_p - v, next + 1);
    }
    return CONFD_OK;
}
int ip_traf_status_get_next(struct confd_trans_ctx *tctx, confd_hkeypath_t * kp,
                            long next)
{
    return CONFD_OK;
}


int ip_traf_status_get_elem(struct confd_trans_ctx *tctx, confd_hkeypath_t * kp)
{
    confd_value_t v;

    // used for ipIfStatsTableLastChange
    CONFD_SET_UINT32(&v, 0);
    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}


int ip_def_router_get_elem(struct confd_trans_ctx *tctx, confd_hkeypath_t * kp)
{
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);
    int ix;
    ipDefaultRouterEntry_t *entry;
    confd_value_t v;

    FOR_ENTRIES(ip_def_router_stats_list, entry, ix) {
        confd_value_t *v_p = kp->v[1], *v_pp;

        if ((v_pp = v_p++,
             entry->ipDefaultRouterAddressType == CONFD_GET_ENUM_HASH(v_pp))
            && (v_pp = v_p++,
                BIN_ADDR_EQ(entry->ipDefaultRouterAddress, v_pp))
            && (v_pp = v_p++,
                entry->ipDefaultRouterIfIndex == CONFD_GET_INT32(v_pp))) {
            break;
        }
    }
    switch (tag) {
    case IP_ELEM(ipDefaultRouterAddressType):
        CONFD_SET_ENUM_HASH(&v,
                            entry->ipDefaultRouterAddressType);
        break;
    case IP_ELEM(ipDefaultRouterAddress):
        CONFD_SET_ADDR(&v, entry->ipDefaultRouterAddress);
        break;
    case IP_ELEM(ipDefaultRouterIfIndex):
        CONFD_SET_INT32(&v, entry->ipDefaultRouterIfIndex);
        break;
    case IP_ELEM(ipDefaultRouterLifetime):
        CONFD_SET_UINT32(&v, entry->ipDefaultRouterLifetime);
        break;
    case IP_ELEM(ipDefaultRouterPreference):
        CONFD_SET_ENUM_HASH(&v, entry->ipDefaultRouterPreference);
        break;
    }
    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}

int ip_def_router_get_next(struct confd_trans_ctx *tctx, confd_hkeypath_t * kp,
                           long next)
{
    confd_value_t v[8], *v_p = v;

    if (next == -1) {
        ip_def_router_stats_list = GET_TABLE(&ip_def_router_stats);
        next = 0;
    }
    if (next >= LIST_SIZE(ip_def_router_stats_list)) {
        confd_data_reply_next_key(tctx, NULL, 0, 0);
    } else {
        ipDefaultRouterEntry_t *entry =
            GET_ENTRY(ip_def_router_stats_list, next);
        CONFD_SET_ENUM_HASH(v_p++, entry->ipDefaultRouterAddressType);
        CONFD_SET_ADDR(v_p++, entry->ipDefaultRouterAddress);
        CONFD_SET_INT32(v_p++, entry->ipDefaultRouterIfIndex);
        confd_data_reply_next_key(tctx, v, v_p - v, next + 1);
    }
    return CONFD_OK;
}


int ip6_scope_zone_idx_get_elem(struct confd_trans_ctx *tctx,
                                confd_hkeypath_t * kp)
{
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);
    int ix;
    ipv6ScopeZoneIndexEntry_t *entry;
    confd_value_t v;

    FOR_ENTRIES(ip6_scope_zone_idx_stats_list, entry, ix) {
        if (entry->ipv6ScopeZoneIndexIfIndex == CONFD_GET_INT32(&kp->v[1][0])) {
            break;
        }
    }
    switch (tag) {
    case IP_ELEM(ipv6ScopeZoneIndexIfIndex):
        CONFD_SET_INT32(&v, entry->ipv6ScopeZoneIndexIfIndex);
        break;
    case IP_ELEM(ipv6ScopeZoneIndexLinkLocal):
        CONFD_SET_UINT32(&v, entry->ipv6ScopeZoneIndexLinkLocal);
        break;
    case IP_ELEM(ipv6ScopeZoneIndex3):
        CONFD_SET_UINT32(&v, entry->ipv6ScopeZoneIndex3);
        break;
    case IP_ELEM(ipv6ScopeZoneIndexAdminLocal):
        CONFD_SET_UINT32(&v, entry->ipv6ScopeZoneIndexAdminLocal);
        break;
    case IP_ELEM(ipv6ScopeZoneIndexSiteLocal):
        CONFD_SET_UINT32(&v, entry->ipv6ScopeZoneIndexSiteLocal);
        break;
    case IP_ELEM(ipv6ScopeZoneIndex6):
        CONFD_SET_UINT32(&v, entry->ipv6ScopeZoneIndex6);
        break;
    case IP_ELEM(ipv6ScopeZoneIndex7):
        CONFD_SET_UINT32(&v, entry->ipv6ScopeZoneIndex7);
        break;
    case IP_ELEM(ipv6ScopeZoneIndexOrganizationLocal):
        CONFD_SET_UINT32(&v, entry->ipv6ScopeZoneIndexOrganizationLocal);
        break;
    case IP_ELEM(ipv6ScopeZoneIndex9):
        CONFD_SET_UINT32(&v, entry->ipv6ScopeZoneIndex9);
        break;
    case IP_ELEM(ipv6ScopeZoneIndexA):
        CONFD_SET_UINT32(&v, entry->ipv6ScopeZoneIndexA);
        break;
    case IP_ELEM(ipv6ScopeZoneIndexB):
        CONFD_SET_UINT32(&v, entry->ipv6ScopeZoneIndexB);
        break;
    case IP_ELEM(ipv6ScopeZoneIndexC):
        CONFD_SET_UINT32(&v, entry->ipv6ScopeZoneIndexC);
        break;
    case IP_ELEM(ipv6ScopeZoneIndexD):
        CONFD_SET_UINT32(&v, entry->ipv6ScopeZoneIndexD);
        break;
    }
    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}

int ip6_scope_zone_idx_get_next(struct confd_trans_ctx *tctx,
                                confd_hkeypath_t * kp, long next)
{
    confd_value_t v[8], *v_p = v;

    if (next == -1) {
        ip6_scope_zone_idx_stats_list = GET_TABLE(&ip6_scope_zone_idx_stats);
        next = 0;
    }
    if (next >= LIST_SIZE(ip6_scope_zone_idx_stats_list)) {
        confd_data_reply_next_key(tctx, NULL, 0, 0);
    } else {
        ipv6ScopeZoneIndexEntry_t *entry =
            GET_ENTRY(ip6_scope_zone_idx_stats_list, next);
        CONFD_SET_INT32(v_p++, entry->ipv6ScopeZoneIndexIfIndex);
        confd_data_reply_next_key(tctx, v, v_p - v, next + 1);
    }
    return CONFD_OK;
}

int ip_net_phys_get_elem(struct confd_trans_ctx *tctx, confd_hkeypath_t * kp)
{
    int ix;
    ip_net_phys_stats_t *np_stats;
    confd_value_t v;

    FOR_ENTRIES(ip_net_phys_list, np_stats, ix)
        if (np_stats->if_index == CONFD_GET_INT32(&kp->v[1][0]) &&
            np_stats->addr_type == CONFD_GET_ENUM_HASH(&kp->v[1][1]) &&
            BIN_ADDR_EQ(np_stats->net_addr, &kp->v[1][2]))
        break;
    switch (CONFD_GET_XMLTAG(&kp->v[0][0])) {
    case IP_ELEM(ipNetToPhysicalPhysAddress):
        CONFD_SET_BINARY(&v, np_stats->hw_addr, np_stats->hw_addr_len);
        break;
    case IP_ELEM(ipNetToPhysicalLastUpdated):
        CONFD_SET_UINT32(&v, 0);
        break;
    case IP_ELEM(ipNetToPhysicalType):
        CONFD_SET_ENUM_HASH(&v, np_stats->type);
        break;
    case IP_ELEM(ipNetToPhysicalState):
        CONFD_SET_ENUM_HASH(&v, np_stats->state);
        break;
    case IP_ELEM(ipNetToPhysicalRowStatus):
        // SNMP agent returns this
        CONFD_SET_ENUM_HASH(&v, 1);
        break;
    }
    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}

int ip_address_prefix_get_elem(struct confd_trans_ctx *tctx,
                               confd_hkeypath_t * kp)
{
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);
    int ix;
    ipAddressPrefixEntry_t *entry;
    confd_value_t v;

    FOR_ENTRIES(ip_addr_prefix_stats_list, entry, ix) {
        confd_value_t *v_p = kp->v[1], *v_pp;

        if ((v_pp = v_p++,
             entry->ipAddressPrefixIfIndex == CONFD_GET_INT32(v_pp))
            && (v_pp = v_p++,
                entry->ipAddressPrefixType == CONFD_GET_ENUM_HASH(v_pp))
            && (v_pp = v_p++, BIN_ADDR_EQ(entry->ipAddressPrefixPrefix, v_pp))
            && (v_pp = v_p++,
                entry->ipAddressPrefixLength == CONFD_GET_UINT32(v_pp))) {
            break;
        }
    }
    switch (tag) {
    case IP_ELEM(ipAddressPrefixIfIndex):
        CONFD_SET_INT32(&v,
                        entry->ipAddressPrefixIfIndex);
        break;
    case IP_ELEM(ipAddressPrefixType):
        CONFD_SET_ENUM_HASH(&v, entry->ipAddressPrefixType);
        break;
    case IP_ELEM(ipAddressPrefixPrefix):
        CONFD_SET_ADDR(&v, entry->ipAddressPrefixPrefix);
        break;
    case IP_ELEM(ipAddressPrefixLength):
        CONFD_SET_UINT32(&v, entry->ipAddressPrefixLength);
        break;
    case IP_ELEM(ipAddressPrefixOrigin):
        CONFD_SET_ENUM_HASH(&v, entry->ipAddressPrefixOrigin);
        break;
    case IP_ELEM(ipAddressPrefixOnLinkFlag):
        CONFD_SET_ENUM_HASH(&v, entry->ipAddressPrefixOnLinkFlag);
        break;
    case IP_ELEM(ipAddressPrefixAutonomousFlag):
        CONFD_SET_ENUM_HASH(&v, entry->ipAddressPrefixAutonomousFlag);
        break;
    case IP_ELEM(ipAddressPrefixAdvPreferredLifetime):
        CONFD_SET_UINT32(&v,
                         entry->ipAddressPrefixAdvPreferredLifetime / 1000000);
        break;
    case IP_ELEM(ipAddressPrefixAdvValidLifetime):
        CONFD_SET_UINT32(&v, entry->ipAddressPrefixAdvValidLifetime / 1000000);
        break;
    }
    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}

int ip_address_get_elem(struct confd_trans_ctx *tctx, confd_hkeypath_t * kp)
{
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);
    int ix;
    ipAddressEntry_t *entry;
    confd_value_t v;

    FOR_ENTRIES(ip_addr_stats_list, entry, ix) {
        confd_value_t *v_p = kp->v[1], *v_pp;

        if ((v_pp = v_p++,
             entry->ipAddressAddrType == CONFD_GET_ENUM_HASH(v_pp))
            && (v_pp = v_p++, BIN_ADDR_EQ(entry->ipAddressAddr, v_pp))) {
            break;
        }
    }
    switch (tag) {
    case IP_ELEM(ipAddressAddrType):
        CONFD_SET_ENUM_HASH(&v, entry->ipAddressAddrType);
        break;
    case IP_ELEM(ipAddressAddr):
        CONFD_SET_ADDR(&v, entry->ipAddressAddr);
        break;
    case IP_ELEM(ipAddressIfIndex):
        CONFD_SET_INT32(&v, entry->ipAddressIfIndex);
        break;
    case IP_ELEM(ipAddressType):
        CONFD_SET_ENUM_HASH(&v, entry->ipAddressType);
        break;
    case IP_ELEM(ipAddressPrefix):
        CONFD_SET_OID(&v, &entry->ipAddressPrefix);
        break;
    case IP_ELEM(ipAddressOrigin):
        CONFD_SET_ENUM_HASH(&v, entry->ipAddressOrigin);
        break;
    case IP_ELEM(ipAddressStatus):
        CONFD_SET_ENUM_HASH(&v, entry->ipAddressStatus);
        break;
    case IP_ELEM(ipAddressCreated):
    case IP_ELEM(ipAddressLastChanged):
        CONFD_SET_UINT32(&v, 0);
        break;
    case IP_ELEM(ipAddressStorageType):
        CONFD_SET_ENUM_HASH(&v, entry->ipAddressStorageType);
        break;
    }
    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}

/* Turn ifName into ifIndex */
int ifNameToIfIndex(char *ifname) {
    if_entry_t *ife;
    int ifIndex;

    ife = get_if_entry_n(ifname);

    if (ife == NULL) {
        error("Couldn't find interface %s.", ifname);
        return -1;
    }

    ifIndex = ife->ifIndex;
    LOG("ifName: %s => %d", ifname, ifIndex);

    return ifIndex;
}


ip_net_phys_stats_t *getNeighEntry(char *ifname,
                                  void *ip,
                                  int ipv6) {
    int ix = 0;
    int ifIndex;

    /* Make sure we have a recently updated table */
    ip_net_phys_list = GET_TABLE(&ip_net_phys_entries);

    ifIndex = ifNameToIfIndex(ifname);

    if (ifIndex == -1)
        return NULL;

    for(ix = 0;ix < LIST_SIZE(ip_net_phys_list); ix++) {
        ip_net_phys_stats_t *stats = GET_ENTRY(ip_net_phys_list, ix);

        if (stats->if_index == ifIndex) {
            if ((!ipv6) && (stats->addr_type == INET(ipv4))) {
                if (stats->net_addr.addr_len != 4) {
                    error("Unexpected addr_len: %d.",
                          stats->net_addr.addr_len);
                    continue;
                }

                if (memcmp(stats->net_addr.address, ip, 4) == 0)
                    return stats;
            }

            if ((ipv6) && (stats->addr_type == INET(ipv6))) {
                if (stats->net_addr.addr_len != 16) {
                    error("Unexpected addr_len: %d.",
                          stats->net_addr.addr_len);
                    continue;
                }

                if (memcmp(stats->net_addr.address, ip, 6) == 0)
                    return stats;
            }
        }
    }

    return NULL;
}


int ip_net_phys_get_next_name(struct confd_trans_ctx *tctx,
                              confd_hkeypath_t * kp,
                              long next, char *ifname, int ipv6)
{
    int ifIndex;
    confd_value_t v[3], *v_p = v;
    struct in_addr ip;
    struct in6_addr ip6;

    ifIndex = ifNameToIfIndex(ifname);

    if (ifIndex == -1) {
        error("Couldn't find interface %s.", ifname);
        confd_data_reply_next_key(tctx, NULL, 0, 0);
        return CONFD_OK;
    }

    /* Make sure we have a recently updated table */
    if (next == -1) {
        ip_net_phys_list = GET_TABLE_NOCACHE(&ip_net_phys_entries);
        next = 0;
    }

    while (next <  LIST_SIZE(ip_net_phys_list)) {
        ip_net_phys_stats_t *stats = GET_ENTRY(ip_net_phys_list, next);

        if (stats->if_index == ifIndex) {
            if ((!ipv6) && (stats->addr_type == INET(ipv4))) {
                if (stats->net_addr.addr_len != 4) {
                    error("Unexpected addr_len: %d.",
                          stats->net_addr.addr_len);
                    next++;
                    continue;
                }

                memcpy(&ip.s_addr, stats->net_addr.address,
                       stats->net_addr.addr_len);

                CONFD_SET_IPV4(v_p++, ip);

                confd_data_reply_next_key(tctx, v, v_p - v, next + 1);
                return CONFD_OK;
            }

            if ((ipv6) && (stats->addr_type == INET(ipv6))) {
                if (stats->net_addr.addr_len != 16) {
                    error("Unexpected addr_len: %d.",
                          stats->net_addr.addr_len);
                    next++;
                    continue;
                }

                memcpy(ip6.s6_addr, stats->net_addr.address,
                       stats->net_addr.addr_len);

                CONFD_SET_IPV6(v_p++, ip6);
                confd_data_reply_next_key(tctx, v, v_p - v, next + 1);
                return CONFD_OK;
            }
        }

        next = next + 1;
    }

    confd_data_reply_next_key(tctx, NULL, 0, 0);
    return CONFD_OK;
}

int ip_net_phys_get_next(struct confd_trans_ctx *tctx, confd_hkeypath_t * kp,
                         long next)
{
    confd_value_t v[3], *v_p = v;

    if (next == -1) {
        ip_net_phys_list = GET_TABLE(&ip_net_phys_entries);
        next = 0;
    }
    if (next >= LIST_SIZE(ip_net_phys_list)) {
        confd_data_reply_next_key(tctx, NULL, 0, 0);
    } else {
        ip_net_phys_stats_t *stats = GET_ENTRY(ip_net_phys_list, next);

        CONFD_SET_INT32(v_p++, stats->if_index);
        CONFD_SET_ENUM_HASH(v_p++, stats->addr_type);
        CONFD_SET_ADDR(v_p++, stats->net_addr);
        confd_data_reply_next_key(tctx, v, v_p - v, next + 1);
    }
    return CONFD_OK;
}


int ip_address_prefix_get_next(struct confd_trans_ctx *tctx,
                               confd_hkeypath_t * kp, long next)
{
    confd_value_t v[8], *v_p = v;

    if (next == -1) {
        ip_addr_prefix_stats_list = GET_TABLE(&ip_addr_prefix_stats);
        next = 0;
    }
    if (next >= LIST_SIZE(ip_addr_prefix_stats_list)) {
        confd_data_reply_next_key(tctx, NULL, 0, 0);
    } else {
        ipAddressPrefixEntry_t *entry =
            GET_ENTRY(ip_addr_prefix_stats_list, next);
        CONFD_SET_INT32(v_p++, entry->ipAddressPrefixIfIndex);
        CONFD_SET_ENUM_HASH(v_p++, entry->ipAddressPrefixType);
        CONFD_SET_ADDR(v_p++, entry->ipAddressPrefixPrefix);
        CONFD_SET_UINT32(v_p++, entry->ipAddressPrefixLength);
        confd_data_reply_next_key(tctx, v, v_p - v, next + 1);
    }
    return CONFD_OK;
}

ipAddressEntry_t *getIpEntry(char *ifName, void *ip, int ipv6) {
    int ix = 0;
    int ifIndex;

    /* Make sure we have a recently updated table */
    ip_addr_stats_list = GET_TABLE(&ip_addr_stats);

    ifIndex = ifNameToIfIndex(ifName);

    if (ifIndex == -1) {
        error("Couldn't find interface %s.", ifName);
        return NULL;
    }

    for(ix = 0;ix < LIST_SIZE(ip_addr_stats_list); ix++) {
        ipAddressEntry_t *entry = GET_ENTRY(ip_addr_stats_list, ix);

        if ((entry->ipAddressIfIndex == ifIndex)
            && (entry->ipAddressType == IPADDRESSTYPE_UNICAST)) {
            if ((!ipv6) && (entry->ipAddressAddrType == INET(ipv4))) {
                if (entry->ipAddressAddr.addr_len != 4) {
                    error("Unexpected addr_len: %d.",
                          entry->ipAddressAddr.addr_len);
                }

                if (memcmp(entry->ipAddressAddr.address, ip, 4) == 0)
                    return entry;
            }
            if ((ipv6) && (entry->ipAddressAddrType == INET(ipv6))) {
                if (entry->ipAddressAddr.addr_len != 16) {
                    error("Unexpected addr_len: %d.",
                          entry->ipAddressAddr.addr_len);
                }

            if (memcmp(entry->ipAddressAddr.address, ip, 8) == 0)
                return entry;
            }
        }

    }

    return NULL;
}

int ip_address_get_next_name(struct confd_trans_ctx *tctx,
                             confd_hkeypath_t * kp, long next,
                             char *name, int ipv6) {
    confd_value_t v[8], *v_p = v;
    int ifIndex;

    ifIndex = ifNameToIfIndex(name);

    if (ifIndex == -1) {
        error("Couldn't find interface %s.", name);
        confd_data_reply_next_key(tctx, NULL, 0, 0);
        return CONFD_OK;
    }

    /* Make sure we have a recently updated table */
    if (next == -1) {
        ip_addr_stats_list = GET_TABLE(&ip_addr_stats);
        next = 0;

    }
    while (next < LIST_SIZE(ip_addr_stats_list)) {
        ipAddressEntry_t *entry = GET_ENTRY(ip_addr_stats_list, next);

        if ((entry->ipAddressIfIndex == ifIndex)
            && (entry->ipAddressType == IPADDRESSTYPE_UNICAST))
        {
            if ((!ipv6) && (entry->ipAddressAddrType == INET(ipv4))) {
                struct in_addr ip;

                if (entry->ipAddressAddr.addr_len != 4) {
                    error("Unexpected addr_len: %d.",
                          entry->ipAddressAddr.addr_len);
                }

                memcpy(&ip.s_addr, entry->ipAddressAddr.address,
                       entry->ipAddressAddr.addr_len);

                CONFD_SET_IPV4(v_p++, ip);
                confd_data_reply_next_key(tctx, v, v_p - v, next + 1);
                return CONFD_OK;
            }

            if ((ipv6) && (entry->ipAddressAddrType == INET(ipv6))) {
                struct in6_addr ip6;
                if (entry->ipAddressAddr.addr_len != 6) {
                    error("Unexpected addr_len: %d.",
                          entry->ipAddressAddr.addr_len);
                }

                memcpy(ip6.s6_addr, entry->ipAddressAddr.address,
                       entry->ipAddressAddr.addr_len);

                CONFD_SET_IPV6(v_p++, ip6);
                confd_data_reply_next_key(tctx, v, v_p - v, next + 1);
                return CONFD_OK;
            }

        }
        next = next + 1;
    }
    confd_data_reply_next_key(tctx, NULL, 0, 0);
    return CONFD_OK;
}

int ip_address_get_next(struct confd_trans_ctx *tctx, confd_hkeypath_t * kp,
                        long next)
{
    confd_value_t v[8], *v_p = v;

    if (next == -1) {
        ip_addr_stats_list = GET_TABLE(&ip_addr_stats);
        next = 0;
    }
    if (next >= LIST_SIZE(ip_addr_stats_list)) {
        confd_data_reply_next_key(tctx, NULL, 0, 0);
    } else {
        ipAddressEntry_t *entry = GET_ENTRY(ip_addr_stats_list, next);

        CONFD_SET_ENUM_HASH(v_p++, entry->ipAddressAddrType);
        CONFD_SET_ADDR(v_p++, entry->ipAddressAddr);
        confd_data_reply_next_key(tctx, v, v_p - v, next + 1);
    }
    return CONFD_OK;
}

int ip_icmp_stat_get_elem(struct confd_trans_ctx *tctx, confd_hkeypath_t * kp)
{
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);
    int ix;
    icmpStatsEntry_t *entry;
    confd_value_t v;

    FOR_ENTRIES(ip_icmp_stats_list, entry, ix) {
        confd_value_t *v_p = kp->v[1], *v_pp;

        if (v_pp = v_p++,
            entry->icmpStatsIPVersion == CONFD_GET_ENUM_HASH(v_pp)) {
            break;
        }
    }
    switch (tag) {
    case IP_ELEM(icmpStatsIPVersion):
        CONFD_SET_ENUM_HASH(&v, entry->icmpStatsIPVersion);
        break;
    case IP_ELEM(icmpStatsInMsgs):
        CONFD_SET_UINT32(&v, (uint32_t) entry->icmpStatsInMsgs);
        break;
    case IP_ELEM(icmpStatsInErrors):
        CONFD_SET_UINT32(&v, (uint32_t) entry->icmpStatsInErrors);
        break;
    case IP_ELEM(icmpStatsOutMsgs):
        CONFD_SET_UINT32(&v, (uint32_t) entry->icmpStatsOutMsgs);
        break;
    case IP_ELEM(icmpStatsOutErrors):
        CONFD_SET_UINT32(&v, (uint32_t) entry->icmpStatsOutErrors);
        break;
    }
    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}

int ip_icmp_stat_get_next(struct confd_trans_ctx *tctx, confd_hkeypath_t * kp,
                          long next)
{
    confd_value_t v[8], *v_p = v;

    if (next == -1) {
        ip_icmp_stats_list = GET_TABLE(&ip_icmp_stats);
        next = 0;
    }
    if (next >= LIST_SIZE(ip_icmp_stats_list)) {
        confd_data_reply_next_key(tctx, NULL, 0, 0);
    } else {
        icmpStatsEntry_t *entry = GET_ENTRY(ip_icmp_stats_list, next);

        CONFD_SET_ENUM_HASH(v_p++, entry->icmpStatsIPVersion);
        confd_data_reply_next_key(tctx, v, v_p - v, next + 1);
    }
    return CONFD_OK;
}

int ip_icmp_msg_stat_get_elem(struct confd_trans_ctx *tctx,
                              confd_hkeypath_t * kp)
{
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);
    int ix;
    icmpMsgStatsEntry_t *entry;
    confd_value_t v;

    FOR_ENTRIES(ip_icmp_msg_stats_list, entry, ix) {
        confd_value_t *v_p = kp->v[1], *v_pp;

        if ((v_pp = v_p++,
             entry->icmpMsgStatsIPVersion == CONFD_GET_ENUM_HASH(v_pp))
            && (v_pp = v_p++,
                entry->icmpMsgStatsType == CONFD_GET_INT32(v_pp))) {
            break;
        }
    }
    switch (tag) {
    case IP_ELEM(icmpMsgStatsIPVersion):
        CONFD_SET_ENUM_HASH(&v, entry->icmpMsgStatsIPVersion);
        break;
    case IP_ELEM(icmpMsgStatsType):
        CONFD_SET_INT32(&v, entry->icmpMsgStatsType);
        break;
    case IP_ELEM(icmpMsgStatsInPkts):
        CONFD_SET_UINT32(&v, (uint32_t) entry->icmpMsgStatsInPkts);
        break;
    case IP_ELEM(icmpMsgStatsOutPkts):
        CONFD_SET_UINT32(&v, (uint32_t) entry->icmpMsgStatsOutPkts);
        break;
    }
    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}

int ip_icmp_msg_stat_get_next(struct confd_trans_ctx *tctx,
                              confd_hkeypath_t * kp, long next)
{
    confd_value_t v[8], *v_p = v;

    if (next == -1) {
        ip_icmp_msg_stats_list = GET_TABLE(&ip_icmp_msg_stats);
        next = 0;
    }
    if (next >= LIST_SIZE(ip_icmp_msg_stats_list)) {
        confd_data_reply_next_key(tctx, NULL, 0, 0);
    } else {
        icmpMsgStatsEntry_t *entry = GET_ENTRY(ip_icmp_msg_stats_list, next);

        CONFD_SET_ENUM_HASH(v_p++, entry->icmpMsgStatsIPVersion);
        CONFD_SET_INT32(v_p++, entry->icmpMsgStatsType);
        confd_data_reply_next_key(tctx, v, v_p - v, next + 1);
    }
    return CONFD_OK;
}

int ip6_router_advert_get_next(struct confd_trans_ctx *tctx,
                               confd_hkeypath_t * kp, long next)
{
    confd_value_t v[8], *v_p = v;

    if (next == -1) {
        ip6_router_advert_list = GET_TABLE(&ip6_router_advert_stats);
        next = 0;
    }
    if (next >= LIST_SIZE(ip6_router_advert_list)) {
        confd_data_reply_next_key(tctx, NULL, 0, 0);
    } else {
        ipv6RouterAdvertEntry_t *entry =
            GET_ENTRY(ip6_router_advert_list, next);
        CONFD_SET_INT32(v_p++, entry->ipv6RouterAdvertIfIndex);
        confd_data_reply_next_key(tctx, v, v_p - v, next + 1);
    }
    return CONFD_OK;
}

int ip6_router_advert_get_elem(struct confd_trans_ctx *tctx,
                               confd_hkeypath_t * kp)
{
    u_int32_t tag = CONFD_GET_XMLTAG(&kp->v[0][0]);
    int ix;
    ipv6RouterAdvertEntry_t *entry;
    confd_value_t v;

    FOR_ENTRIES(ip6_router_advert_list, entry, ix) {
        confd_value_t *v_p = kp->v[1], *v_pp;

        if (v_pp = v_p++,
            entry->ipv6RouterAdvertIfIndex == CONFD_GET_INT32(v_pp)) {
            break;
        }
    }
    switch (tag) {
    case IP_ELEM(ipv6RouterAdvertIfIndex):
        CONFD_SET_INT32(&v, entry->ipv6RouterAdvertIfIndex);
        break;
    case IP_ELEM(ipv6RouterAdvertSendAdverts):
        CONFD_SET_ENUM_HASH(&v, entry->ipv6RouterAdvertSendAdverts);
        break;
    case IP_ELEM(ipv6RouterAdvertMaxInterval):
        CONFD_SET_UINT32(&v, entry->ipv6RouterAdvertMaxInterval);
        break;
    case IP_ELEM(ipv6RouterAdvertMinInterval):
        CONFD_SET_UINT32(&v, entry->ipv6RouterAdvertMinInterval);
        break;
    case IP_ELEM(ipv6RouterAdvertManagedFlag):
        CONFD_SET_ENUM_HASH(&v, entry->ipv6RouterAdvertManagedFlag);
        break;
    case IP_ELEM(ipv6RouterAdvertOtherConfigFlag):
        CONFD_SET_ENUM_HASH(&v, entry->ipv6RouterAdvertOtherConfigFlag);
        break;
    case IP_ELEM(ipv6RouterAdvertLinkMTU):
        CONFD_SET_UINT32(&v, entry->ipv6RouterAdvertLinkMTU);
        break;
    case IP_ELEM(ipv6RouterAdvertReachableTime):
        CONFD_SET_UINT32(&v, entry->ipv6RouterAdvertReachableTime);
        break;
    case IP_ELEM(ipv6RouterAdvertRetransmitTime):
        CONFD_SET_UINT32(&v, entry->ipv6RouterAdvertRetransmitTime);
        break;
    case IP_ELEM(ipv6RouterAdvertCurHopLimit):
        CONFD_SET_UINT32(&v, entry->ipv6RouterAdvertCurHopLimit);
        break;
    case IP_ELEM(ipv6RouterAdvertDefaultLifetime):
        CONFD_SET_UINT32(&v, entry->ipv6RouterAdvertDefaultLifetime);
        break;
    case IP_ELEM(ipv6RouterAdvertRowStatus):
        CONFD_SET_ENUM_HASH(&v, entry->ipv6RouterAdvertRowStatus);
        break;
    }
    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}

int dummy_get_elem(struct confd_trans_ctx *tctx, confd_hkeypath_t * kp)
{
    return CONFD_ERR;            // should not be called (yet)
}

int dummy_get_next(struct confd_trans_ctx *tctx, confd_hkeypath_t * kp,
                   long next)
{
    warn("list ipv6RouterAdvertEntry not supported");
    confd_data_reply_next_key(tctx, NULL, 0, 0);
    return CONFD_OK;
}
