#ifndef __IP__H__
#define __IP__H__

#include <stdint.h>

#include "utils.h"
#include "caches.h"

#undef TODO
#define TODO_t uint32_t

#define IP_ELEM(el) _NSCONCAT(ip_mib, el)

/**************** container ip, types ****************/

typedef struct {
    int32_t  ipv4InterfaceIfIndex;
    int32_t  ipv4InterfaceReasmMaxSize;
    int32_t  ipv4InterfaceEnableStatus;
    uint32_t ipv4InterfaceRetransmitTime;
} ipv4InterfaceEntry_t;
typedef ROW_LIST(ipv4InterfaceEntry_t) ipv4InterfaceEntry_list_t;

typedef struct {
    int32_t  ipv6InterfaceIfIndex;
    int32_t  ipv6InterfaceReasmMaxSize;
    char     ipv6InterfaceIdentifier[259];
    int32_t  ipv6InterfaceEnableStatus;
    uint32_t ipv6InterfaceReachableTime;
    uint32_t ipv6InterfaceRetransmitTime;
    int32_t  ipv6InterfaceForwarding;
} ipv6InterfaceEntry_t;
typedef ROW_LIST(ipv6InterfaceEntry_t) ipv6InterfaceEntry_list_t;


typedef struct {
    int32_t  ipAddressPrefixIfIndex;
    int32_t  ipAddressPrefixType;
    inet_address_t ipAddressPrefixPrefix;
    int32_t  ipAddressPrefixLength;
    int32_t  ipAddressPrefixOrigin;
    int32_t  ipAddressPrefixOnLinkFlag;
    int32_t  ipAddressPrefixAutonomousFlag;
    uint32_t ipAddressPrefixAdvPreferredLifetime;
    uint32_t ipAddressPrefixAdvValidLifetime;
} ipAddressPrefixEntry_t;
typedef ROW_LIST(ipAddressPrefixEntry_t) ipAddressPrefixEntry_list_t;

// IP-MIB enumeration types
#define IPADDRESSPREFIXORIGINTC_OTHER      1
#define IPADDRESSPREFIXORIGINTC_MANUAL     2
#define IPADDRESSPREFIXORIGINTC_WELLKNOWN  3
#define IPADDRESSPREFIXORIGINTC_DHCP       4
#define IPADDRESSPREFIXORIGINTC_ROUTERADV  5

#define STORAGETYPE_OTHER        1
#define STORAGETYPE_VOLATILE     2
#define STORAGETYPE_NONVOLATILE  3
#define STORAGETYPE_PERMANENT    4
#define STORAGETYPE_READONLY     5

#define IPADDRESSTYPE_UNICAST   1
#define IPADDRESSTYPE_ANYCAST   2
#define IPADDRESSTYPE_BROADCAST 3

#define IPADDRESSORIGINTC_OTHER     1
#define IPADDRESSORIGINTC_MANUAL    2
#define IPADDRESSORIGINTC_DHCP      4
#define IPADDRESSORIGINTC_LINKLAYER 5
#define IPADDRESSORIGINTC_RANDOM    6

#define IPADDRESSSTATUSTC_PREFERRED    1
#define IPADDRESSSTATUSTC_DEPRECATED   2
#define IPADDRESSSTATUSTC_INVALID      3
#define IPADDRESSSTATUSTC_INACCESSIBLE 4
#define IPADDRESSSTATUSTC_UNKNOWN      5
#define IPADDRESSSTATUSTC_TENTATIVE    6
#define IPADDRESSSTATUSTC_DUPLICATE    7

typedef struct {
    int32_t  ipAddressAddrType;
    inet_address_t ipAddressAddr;
    int32_t  ipAddressIfIndex;
    int32_t  ipAddressType;
    struct confd_snmp_oid ipAddressPrefix;
    int32_t  ipAddressOrigin;
    int32_t  ipAddressStatus;
    uint32_t ipAddressCreated;
    uint32_t ipAddressLastChanged;
    TODO_t   ipAddressRowStatus;
    int32_t  ipAddressStorageType;

    uint32_t ipFlags;           // not IP-MIB
    uint32_t ipPrefixLen;       // not IP-MIB
} ipAddressEntry_t;
typedef ROW_LIST(ipAddressEntry_t) ipAddressEntry_list_t;

typedef struct {
    int32_t ipv6ScopeZoneIndexIfIndex;
    uint32_t ipv6ScopeZoneIndexLinkLocal;
    uint32_t ipv6ScopeZoneIndex3;
    uint32_t ipv6ScopeZoneIndexAdminLocal;
    uint32_t ipv6ScopeZoneIndexSiteLocal;
    uint32_t ipv6ScopeZoneIndex6;
    uint32_t ipv6ScopeZoneIndex7;
    uint32_t ipv6ScopeZoneIndexOrganizationLocal;
    uint32_t ipv6ScopeZoneIndex9;
    uint32_t ipv6ScopeZoneIndexA;
    uint32_t ipv6ScopeZoneIndexB;
    uint32_t ipv6ScopeZoneIndexC;
    uint32_t ipv6ScopeZoneIndexD;

} ipv6ScopeZoneIndexEntry_t;
typedef ROW_LIST(ipv6ScopeZoneIndexEntry_t) ipv6ScopeZoneIndexEntry_list_t;

typedef struct {
    int32_t  ipDefaultRouterAddressType;
    inet_address_t     ipDefaultRouterAddress;
    int32_t  ipDefaultRouterIfIndex;
    uint32_t ipDefaultRouterLifetime;
    int32_t  ipDefaultRouterPreference;
} ipDefaultRouterEntry_t;
typedef ROW_LIST(ipDefaultRouterEntry_t) ipDefaultRouterEntry_list_t;

typedef struct {
    int32_t  ipv6RouterAdvertIfIndex;
    int32_t  ipv6RouterAdvertSendAdverts;
    uint32_t ipv6RouterAdvertMaxInterval;
    uint32_t ipv6RouterAdvertMinInterval;
    int32_t  ipv6RouterAdvertManagedFlag;
    int32_t  ipv6RouterAdvertOtherConfigFlag;
    uint32_t ipv6RouterAdvertLinkMTU;
    uint32_t ipv6RouterAdvertReachableTime;
    uint32_t ipv6RouterAdvertRetransmitTime;
    uint32_t ipv6RouterAdvertCurHopLimit;
    uint32_t ipv6RouterAdvertDefaultLifetime;
    int32_t  ipv6RouterAdvertRowStatus;

} ipv6RouterAdvertEntry_t;
typedef ROW_LIST(ipv6RouterAdvertEntry_t) ipv6RouterAdvertEntry_list_t;


typedef struct {
    int32_t ipForwarding;
    int32_t ipDefaultTTL;
    int32_t ipReasmTimeout;
    int32_t ipv6IpForwarding;
    int32_t ipv6IpDefaultHopLimit;
} ip_basic_stats_t;

/******** container ip ********/
typedef struct {
    ipv4InterfaceEntry_list_t      ipv4InterfaceEntry_list;
    uint32_t                       ipv6InterfaceTableLastChange;
    ipv6InterfaceEntry_list_t      ipv6InterfaceEntry_list;
    ipAddressPrefixEntry_list_t    ipAddressPrefixEntry_list;
    TODO_t                         ipAddressSpinLock;
    ipAddressEntry_list_t          ipAddressEntry_list;
    ipv6ScopeZoneIndexEntry_list_t ipv6ScopeZoneIndexEntry_list;
    ipDefaultRouterEntry_list_t    ipDefaultRouterEntry_list;
    TODO_t                         ipv6RouterAdvertSpinLock;
    ipv6RouterAdvertEntry_list_t   ipv6RouterAdvertEntry_list;

} ip_container_t;



/**************** container ipTrafficStats, types ****************/
typedef struct {
    int32_t  ipSystemStatsIPVersion;
    uint32_t ipSystemStatsInReceives;
    uint64_t ipSystemStatsHCInReceives;
    uint32_t ipSystemStatsInOctets;
    uint64_t ipSystemStatsHCInOctets;
    uint64_t ipSystemStatsInHdrErrors;
    uint64_t ipSystemStatsInNoRoutes;
    uint64_t ipSystemStatsInAddrErrors;
    uint64_t ipSystemStatsInUnknownProtos;
    uint64_t ipSystemStatsInTruncatedPkts;
    uint64_t ipSystemStatsInForwDatagrams;
    uint64_t ipSystemStatsReasmReqds;
    uint64_t ipSystemStatsReasmOKs;
    uint64_t ipSystemStatsReasmFails;
    uint64_t ipSystemStatsInDiscards;
    uint32_t ipSystemStatsInDelivers;
    uint64_t ipSystemStatsHCInDelivers;
    uint32_t ipSystemStatsOutRequests;
    uint64_t ipSystemStatsHCOutRequests;
    uint64_t ipSystemStatsOutNoRoutes;
    uint32_t ipSystemStatsOutForwDatagrams;
    uint64_t ipSystemStatsHCOutForwDatagrams;
    uint64_t ipSystemStatsOutDiscards;
    uint64_t ipSystemStatsOutFragReqds;
    uint64_t ipSystemStatsOutFragOKs;
    uint64_t ipSystemStatsOutFragFails;
    uint64_t ipSystemStatsOutFragCreates;
    uint64_t ipSystemStatsOutTransmits;
    uint32_t ipSystemStatsOutOctets;
    uint64_t ipSystemStatsHCOutOctets;
    uint32_t ipSystemStatsInMcastPkts;
    uint64_t ipSystemStatsHCInMcastPkts;
    uint32_t ipSystemStatsInMcastOctets;
    uint64_t ipSystemStatsHCInMcastOctets;
    uint32_t ipSystemStatsOutMcastPkts;
    uint64_t ipSystemStatsHCOutMcastPkts;
    uint32_t ipSystemStatsOutMcastOctets;
    uint64_t ipSystemStatsHCOutMcastOctets;
    uint32_t ipSystemStatsInBcastPkts;
    uint64_t ipSystemStatsHCInBcastPkts;
    uint32_t ipSystemStatsOutBcastPkts;
    uint64_t ipSystemStatsHCOutBcastPkts;
    uint32_t ipSystemStatsDiscontinuityTime;
    uint32_t ipSystemStatsRefreshRate;
} ipSystemStatsEntry_t;
typedef struct {
    ipSystemStatsEntry_t entries[2];
} ipSystemStatsEntry_table_t;

typedef struct {
    int32_t  ipIfStatsIPVersion;
    int32_t  ipIfStatsIfIndex;
    uint32_t ipIfStatsInReceives;
    uint64_t ipIfStatsHCInReceives;
    uint32_t ipIfStatsInOctets;
    uint64_t ipIfStatsHCInOctets;
    uint64_t ipIfStatsInHdrErrors;
    uint64_t ipIfStatsInNoRoutes;
    uint64_t ipIfStatsInAddrErrors;
    uint64_t ipIfStatsInUnknownProtos;
    uint64_t ipIfStatsInTruncatedPkts;
    uint32_t ipIfStatsInForwDatagrams;
    uint64_t ipIfStatsReasmReqds;
    uint64_t ipIfStatsReasmOKs;
    uint64_t ipIfStatsReasmFails;
    uint64_t ipIfStatsInDiscards;
    uint32_t ipIfStatsInDelivers;
    uint64_t ipIfStatsHCInDelivers;
    uint32_t ipIfStatsOutRequests;
    uint64_t ipIfStatsHCOutRequests;
    uint32_t ipIfStatsOutForwDatagrams;
    uint64_t ipIfStatsHCOutForwDatagrams;
    uint64_t ipIfStatsOutDiscards;
    uint64_t ipIfStatsOutNoRoutes;
    uint64_t ipIfStatsOutFragReqds;
    uint64_t ipIfStatsOutFragOKs;
    uint64_t ipIfStatsOutFragFails;
    uint64_t ipIfStatsOutFragCreates;
    uint64_t ipIfStatsOutTransmits;
    uint32_t ipIfStatsOutOctets;
    uint64_t ipIfStatsHCOutOctets;
    uint32_t ipIfStatsInMcastPkts;
    uint64_t ipIfStatsHCInMcastPkts;
    uint32_t ipIfStatsInMcastOctets;
    uint64_t ipIfStatsHCInMcastOctets;
    uint32_t ipIfStatsOutMcastPkts;
    uint64_t ipIfStatsHCOutMcastPkts;
    uint32_t ipIfStatsOutMcastOctets;
    uint64_t ipIfStatsHCOutMcastOctets;
    uint32_t ipIfStatsInBcastPkts;
    uint64_t ipIfStatsHCInBcastPkts;
    uint32_t ipIfStatsOutBcastPkts;
    uint64_t ipIfStatsHCOutBcastPkts;

} ipIfStatsEntry_t;
typedef ROW_LIST(ipIfStatsEntry_t) ipIfStatsEntry_list_t;

/* net to physical mapping */

typedef struct {
    int32_t if_index;
    u_int32_t addr_type;
    inet_address_t net_addr;
    u_int8_t hw_addr[ADDRLEN];
    unsigned hw_addr_len;
    u_int32_t type;
    u_int32_t state;
    u_int32_t ipv6router;
} ip_net_phys_stats_t;

typedef ROW_LIST(ip_net_phys_stats_t) ip_net_phys_list_t;

/******** container icmp ********/

typedef struct {
    int32_t  icmpStatsIPVersion;
    uint64_t icmpStatsInMsgs;
    uint64_t icmpStatsInErrors;
    uint64_t icmpStatsOutMsgs;
    uint64_t icmpStatsOutErrors;

} icmpStatsEntry_t;
typedef ROW_LIST(icmpStatsEntry_t) icmpStatsEntry_list_t;

typedef struct {
    int32_t  icmpMsgStatsIPVersion;
    int32_t  icmpMsgStatsType;
    uint64_t icmpMsgStatsInPkts;
    uint64_t icmpMsgStatsOutPkts;

} icmpMsgStatsEntry_t;
typedef ROW_LIST(icmpMsgStatsEntry_t) icmpMsgStatsEntry_list_t;

/******** interface ********/

void update_ip_system_stats(ipSystemStatsEntry_table_t *stats);

void update_ip_if_stats(ipIfStatsEntry_list_t *stats);
ENTRY_COMPARATOR(ipIfStatsEntry_t, compare_ip_if_stats);

void update_ip_def_router(ipDefaultRouterEntry_list_t *stats);
ENTRY_COMPARATOR(ipDefaultRouterEntry_t, compare_ip_def_router);

void update_ip6_scope_zone_index(ipv6ScopeZoneIndexEntry_list_t *stats);
ENTRY_COMPARATOR(ipv6ScopeZoneIndexEntry_t, compare_ip6_scope_zone_index);

void update_ip_address_prefix(ipAddressPrefixEntry_list_t *stats);
ENTRY_COMPARATOR(ipAddressPrefixEntry_t, compare_ip_address_prefix);

void update_ip_address(ipAddressEntry_list_t *stats);
ENTRY_COMPARATOR(ipAddressEntry_t, compare_ip_address);


void update_ip_basic_stats(ip_basic_stats_t *stats);

void update_net_phys_list(ip_net_phys_list_t *entry);
ENTRY_COMPARATOR(ip_net_phys_stats_t, compare_net_phys_entries);


void update_ip_icmp_stats(icmpStatsEntry_list_t *stats);
ENTRY_COMPARATOR(icmpStatsEntry_t, compare_ip_icmp_stats);

void update_ip_icmp_msg_stats(icmpMsgStatsEntry_list_t *stats);
ENTRY_COMPARATOR(icmpMsgStatsEntry_t, compare_ip_icmp_msg_stats);

void update_ipv6_router_advert_stats(ipv6RouterAdvertEntry_list_t *stats);
ENTRY_COMPARATOR(ipv6RouterAdvertEntry_t, compare_ipv6_router_advert_stats);


#endif
