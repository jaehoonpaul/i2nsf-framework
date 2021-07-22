#ifndef __IF__H__
#define __IF__H__

#include <stdint.h>

#include "utils.h"
#include "caches.h"

typedef struct {
    int32_t   ifIndex;
    char      ifDescr[256];
    int32_t   ifType;
#ifdef LINUXCFG_INTERFACES
    int32_t   ifIanaType;
#endif
    int32_t   ifMtu;
    uint32_t  ifSpeed;
    u_int8_t  ifPhysAddress[ADDRLEN];
    unsigned  ifPhysAddress_len; // number of octets really acquired
    int32_t   ifAdminStatus;
    int32_t   ifOperStatus;
    uint32_t  ifLastChange;
    uint64_t  ifInOctets;
    uint64_t  ifInUcastPkts;
    uint64_t  ifInDiscards;
    uint64_t  ifInErrors;
    uint64_t  ifInUnknownProtos;
    uint64_t  ifOutOctets;
    uint64_t  ifOutUcastPkts;
    uint64_t  ifOutDiscards;
    uint64_t  ifOutErrors;
    char      ifName[256];
    uint64_t  ifInMulticastPkts;
    uint64_t  ifInBroadcastPkts;
    uint64_t  ifOutMulticastPkts;
    uint64_t  ifOutBroadcastPkts;
    uint64_t  ifHCInOctets;
    uint64_t  ifHCInUcastPkts;
    uint64_t  ifHCInMulticastPkts;
    uint64_t  ifHCInBroadcastPkts;
    uint64_t  ifHCOutOctets;
    uint64_t  ifHCOutUcastPkts;
    uint64_t  ifHCOutMulticastPkts;
    uint64_t  ifHCOutBroadcastPkts;
    int32_t   ifLinkUpDownTrapEnable;
    uint32_t  ifHighSpeed;
    int32_t   ifPromiscuousMode;
    int32_t   ifConnectorPresent;
    char      ifAlias[256];
    uint32_t  ifCounterDiscontinuityTime;
    uint32_t  ifFlags;
    uint32_t  if4RetransmitTime;
    uint32_t  if6RetransmitTime;
    uint32_t  if6ReachableTime;
    int       if6Forwarding;
} if_entry_t;

typedef ROW_LIST(if_entry_t) if_entry_list_t;

if_entry_t *get_if_entry_n(char *ifName);
if_entry_list_t *get_if_entry_list();


//****    container ifMIBObjects Begin

//    list ifStackEntry, key : ifStackHigherLayer, ifStackLowerLayer
typedef struct {
    int32_t ifStackHigherLayer;
    int32_t ifStackLowerLayer;
//    int32_t ifStackStatus;
} ifStackEntry_t;

typedef ROW_LIST(ifStackEntry_t) ifStackEntry_list_t;


//    list ifRcvAddressEntry, key : ifIndex, ifRcvAddressAddress
typedef struct {
    int32_t  ifIndex;
    uint8_t ifRcvAddressAddress[256]; // phys address
    size_t  ifRcvAddressAddress_len; // number of octets really acquired
//    int32_t ifRcvAddressStatus; // enum
    int32_t  ifRcvAddressType;  // enum
    int32_t  ifTableLastChange;
    int32_t  ifStackLastChange;
} ifRcvAddressEntry_t;

typedef ROW_LIST(ifRcvAddressEntry_t) ifRcvAddressEntry_list_t;

void update_if_stack_entry(ifStackEntry_list_t *list);
ENTRY_COMPARATOR(ifStackEntry_t, compare_if_stack_entry);


void update_if_rcv_address_entry(ifRcvAddressEntry_list_t *list);
ENTRY_COMPARATOR(ifRcvAddressEntry_t, compare_if_rcv_address_entry);


//****    container ifMIBObjects End


void update_if_entries(if_entry_list_t *if_list);
ENTRY_COMPARATOR(if_entry_t, compare_if_entries);

const char* get_manufacturer(uint8_t mac0, uint8_t mac1, uint8_t mac2);

#endif
