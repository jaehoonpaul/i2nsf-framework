#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <malloc.h>

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <confd.h>

#include "linuxcfg_api.h"
#include "utils.h"

#include "udp.h"
#include "UDP-MIB.h"

void update_udp_stats(udp_stats_t * udp_stats)
{
    char buf[BUFSIZ], *pos;
    uint32_t in, out;

    TRACE_ENTRY();
    memset(udp_stats, 0, sizeof(udp_stats_t));
    pos = get_net_snmp_line(buf, BUFSIZ, "Udp");
    if (pos == NULL) {
        error("could not find Udp entry in /proc/net/snmp");
        return;
    }
    if (4 != sscanf(pos, "%u %u %u %u",
                    &in, &udp_stats->udpNoPorts, &udp_stats->udpInErrors,
                    &out)) {
        error("failed to scan the Udp entry");
    }
    UPDATE_COUNTER_PAIR(in, udp_stats->udpInDatagrams,
                        udp_stats->udpHCInDatagrams);
    UPDATE_COUNTER_PAIR(out, udp_stats->udpOutDatagrams,
                        udp_stats->udpHCOutDatagrams);
    TRACE_EXIT();
}

void update_udp_endpoints(udp_endpoint_list_t * udp_list)
{
    FILE *in;
    char line[BUFSIZ];
    int i, cnt = 0;
    inode_pid_pair_t *pairs;
    struct {
        const char *file;
        u_int32_t ipv;
    } ipversions[] = {{"udp", INET(ipv4)},
                      {"udp6", INET(ipv6)}};
    char file[256];

    TRACE_ENTRY();
    RESET_LIST(udp_list);
    for (i = 0; i < sizeof(ipversions) / sizeof(*ipversions); i++) {
        sprintf(file, "/proc/net/%s", ipversions[i].file);
        if (!(in = fopen(file, "r"))) {
            error("%s: Failed to load UDP table %s", __FUNCTION__, file);
            continue;
        }

        if (line != fgets(line, sizeof(line), in))
            warn("Couldn't read whole line.");

        while (line == fgets(line, sizeof(line), in)) {
            char locaddr[ADDRLEN], remaddr[ADDRLEN];
            udp_endpointstats_t *stats = GET_ENTRY(udp_list, cnt++);

            if (3 !=
                sscanf(line,
                       "%*d: %s %s %*x %*x:%*x %*x:%*x %*x %*u %*u %u",
                       locaddr, remaddr, &stats->udpEndpointInstance))
                continue;
            stats->udpEndpointLocalAddressType =
                stats->udpEndpointRemoteAddressType = ipversions[i].ipv;
            /* store in network byte order */
            if (scan_addr_port
                (&stats->udpEndpointLocalAddress,
                 &stats->udpEndpointLocalPort, locaddr, ipversions[i].ipv) < 0)
                error("Failed to scan address:port pair from %s", locaddr);
            if (scan_addr_port
                (&stats->udpEndpointRemoteAddress,
                 &stats->udpEndpointRemotePort, remaddr, ipversions[i].ipv) < 0)
                error("Failed to scan address:port pair from %s", remaddr);
        }
        fclose(in);
    }
    pairs = malloc(sizeof(inode_pid_pair_t) * LIST_SIZE(udp_list));
    for (i = 0; i < LIST_SIZE(udp_list); i++) {
        pairs[i].inode = GET_ENTRY(udp_list, i)->udpEndpointInstance;
        pairs[i].process_id = 0;
    }
    find_process_ids(pairs, LIST_SIZE(udp_list));
    for (i = 0; i < LIST_SIZE(udp_list); i++)
        GET_ENTRY(udp_list, i)->udpEndpointProcess = pairs[i].process_id;
    free(pairs);
    SORT_LIST(udp_list);
    FILTER_LIST(udp_list);
    TRACE_EXIT();
}

int compare_udp_endpoints(const udp_endpointstats_t * s1,
                          const udp_endpointstats_t * s2)
{
    return
        UINT_CMP(s1->udpEndpointLocalAddressType,
                 s2->udpEndpointLocalAddressType) ? :
        (INET_ADDR_CMP(s1->udpEndpointLocalAddress,
                       s2->udpEndpointLocalAddress) ? :
         (UINT_CMP(s1->udpEndpointLocalPort, s2->udpEndpointLocalPort) ? :
          (UINT_CMP(s1->udpEndpointRemoteAddressType,
                    s2->udpEndpointRemoteAddressType) ? :
           (INET_ADDR_CMP(s1->udpEndpointRemoteAddress,
                          s2->udpEndpointRemoteAddress) ? :
            (UINT_CMP(s1->udpEndpointRemotePort, s2->udpEndpointRemotePort) ? :
             UINT_CMP(s1->udpEndpointInstance, s2->udpEndpointInstance))))));
}
