#ifndef _UDP_H_
#define _UDP_H_

#include <stdint.h>

#include "utils.h"
#include "caches.h"

typedef struct {
    uint32_t udpInDatagrams, udpNoPorts, udpInErrors, udpOutDatagrams;
    uint64_t udpHCInDatagrams, udpHCOutDatagrams;
} udp_stats_t;

typedef struct {
    uint32_t udpEndpointLocalAddressType, udpEndpointRemoteAddressType;
    inet_address_t udpEndpointLocalAddress, udpEndpointRemoteAddress;
    int local_len, remote_len;
    uint32_t udpEndpointLocalPort, udpEndpointRemotePort;
    uint32_t udpEndpointInstance;
    uint32_t udpEndpointProcess;
} udp_endpointstats_t;

typedef ROW_LIST(udp_endpointstats_t) udp_endpoint_list_t;

void update_udp_endpoints(udp_endpoint_list_t *udp_list);
ENTRY_COMPARATOR(udp_endpointstats_t, compare_udp_endpoints);
void update_udp_stats(udp_stats_t *udp_stats);

#endif
