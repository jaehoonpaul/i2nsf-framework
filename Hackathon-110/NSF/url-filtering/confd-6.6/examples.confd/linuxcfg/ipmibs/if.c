#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <linux/if.h>
#include <linux/ethtool.h>
#include <linux/sockios.h>
#include <linux/if_arp.h>

#include <confd.h>

#include "linuxcfg_api.h"
#include "utils.h"

#include "udp.h"
#include "IF-MIB.h"
#include "IANAifType-MIB.h"
#define IF_TYPE(type) _NSCONCAT(ianaiftype_mib, type)

#ifdef LINUXCFG_INTERFACES
#include "iana-if-type.h"
#define IANAIF_TYPE(type) _NSCONCAT(ianaift, type);
#endif
#include "if.h"

/******** support for loading a line list from file ********/
typedef char line_t[256];

static ENTRY_COMPARATOR(line_t, compare_lines);
typedef ROW_LIST(line_t) line_list_t;
line_list_t g_lines;
static int g_lines_need_init = 1;

static int load_line_list(const char *file, line_list_t * list)
{
    FILE *fp;
    char line[256];

    if ((fp = fopen(file, "r")) == NULL) {
        error("\n%s: Failed fopen(\"%s\", \"r\")", __FUNCTION__, file);
        return -1;
    }
    while (fgets(line, sizeof(line), fp)) {
        strcpy((char *) NEW_ENTRY(list), line);
    }
    fclose(fp);
    return 0;
}

#define GET_IF_COUNTER(pos, entry, field) do {            \
        uint64_t tmp;                                    \
        get_uint64_dec(&(pos), &tmp);                    \
        UPDATE_COUNTER_PAIR(tmp, (entry)->if ## field,    \
                            (entry)->ifHC ## field);    \
    } while(0)

/******** supplement ifc list with statistics from /proc/net/dev ********/
static int load_if_stats(if_entry_list_t * if_list)
{
    int j;
    char *pos;

    if_entry_t *if_entry;
    line_t *line;
    uint64_t dummy;

    RESET_LIST(if_list);
    if (g_lines_need_init) {
        INIT_LIST(&g_lines, compare_lines);
        g_lines_need_init = 0;
    }
    RESET_LIST(&g_lines);
    load_line_list("/proc/net/dev", &g_lines);
    FOR_ENTRIES(&g_lines, line, j) {
        if ((pos = strchr(*line, ':')) != NULL) {
            *(pos++) = 0;
            if_entry = NEW_ENTRY(if_list);
            sscanf(*line, " %15s", if_entry->ifName);
            GET_IF_COUNTER(pos, if_entry, InOctets);
            GET_IF_COUNTER(pos, if_entry, InUcastPkts);
            get_uint64_dec(&pos, &if_entry->ifInErrors);
            get_uint64_dec(&pos, &if_entry->ifInDiscards);
            get_uint64_dec(&pos, &dummy);
            get_uint64_dec(&pos, &dummy);
            get_uint64_dec(&pos, &dummy);
            GET_IF_COUNTER(pos, if_entry, InMulticastPkts);
            GET_IF_COUNTER(pos, if_entry, OutOctets);
            GET_IF_COUNTER(pos, if_entry, OutUcastPkts);
            get_uint64_dec(&pos, &if_entry->ifOutErrors);
            get_uint64_dec(&pos, &if_entry->ifOutDiscards);
        }
    }
    return 0;
}
static int compare_lines(const line_t * l1, const line_t * l2)
{
    // This is not used (line_t entries are not sorted)
    return 0;
}

static uint64_t get_if_speed_mii(const char *name)
{
    int fd;
    unsigned long long retspeed = 10000000;
    struct ifreq ifr;


    /* the code is based on mii-diag utility by Donald Becker
     * see ftp://ftp.scyld.com/pub/diag/mii-diag.c
     */
    ushort *data = (ushort *) (&ifr.ifr_data);
    unsigned phy_id;
    int mii_reg, i;

    ushort mii_val[32];
    ushort bmcr, bmsr, nway_advert, lkpar;
    const unsigned long long media_speeds[] =
        { 10000000, 10000000, 100000000, 100000000, 10000000, 0 };

    /* It corresponds to "10baseT", "10baseT-FD", "100baseTx",
       "100baseTx-FD", "100baseT4", "Flow-control", 0, */
    strncpy(ifr.ifr_name, name, sizeof(ifr.ifr_name));
    ifr.ifr_name[sizeof(ifr.ifr_name) - 1] = 0;
    data[0] = 0;
    if ((fd = get_ioctl_socket()) < 0) {
        error("\n%s: failed socket(AF_INET, SOCK_DGRAM, 0)", __FUNCTION__);
        return 0;
    }

    /*
     * SIOCGMIIPHY has been defined since at least kernel 2.4.10 (Sept 2001).
     * It's probably safe to drop the interim SIOCDEVPRIVATE handling now!
     */
    if (ioctl(fd, SIOCGMIIPHY, &ifr) < 0) {
        warn("\n%s: failed ioctl(%d, SIOCGMIIPHY, ,,,)", __FUNCTION__, fd);
        return retspeed;
    }

    /* Begin getting mii register values */
    phy_id = data[0];
    for (mii_reg = 0; mii_reg < 8; mii_reg++) {
        data[0] = phy_id;
        data[1] = mii_reg;
        if (ioctl(fd, SIOCGMIIREG, &ifr) < 0) {
            warn("SIOCGMIIREG on %s failed\n", name);
        }
        mii_val[mii_reg] = data[3];
    }

    /*Parsing of mii values */
    /*Invalid basic mode control register */
    if (mii_val[0] == 0xffff || mii_val[1] == 0x0000) {
        warn("\n%s: No MII transceiver present", __FUNCTION__);
        return retspeed;
    }

    /* Descriptive rename. */
    bmcr = mii_val[0];            /*basic mode control register */
    bmsr = mii_val[1];            /* basic mode status register */
    nway_advert = mii_val[4];    /* autonegotiation advertisement */
    lkpar = mii_val[5];            /*link partner ability */

    /*Check for link existence, returns 0 if link is absent */
    if ((bmsr & 0x0016) != 0x0004) {
        retspeed = 0;
        return retspeed;
    }
    if (!(bmcr & 0x1000)) {
        retspeed = bmcr & 0x2000 ? 100000000 : 10000000;
        return retspeed;
    }

    /* Link partner got our advertised abilities */
    if (lkpar & 0x4000) {
        int negotiated = nway_advert & lkpar & 0x3e0;
        int max_capability = 0;


        /* Scan for the highest negotiated capability, highest priority
           (100baseTx-FDX) to lowest (10baseT-HDX). */
        int media_priority[] = { 8, 9, 7, 6, 5 };    /* media_names[i-5] */
        for (i = 0; media_priority[i]; i++) {
            if (negotiated & (1 << media_priority[i])) {
                max_capability = media_priority[i];
                break;
            }
        }
        if (max_capability)
            retspeed = media_speeds[max_capability - 5];

        else
            error("\n%s: No common media type was autonegotiated",
                  __FUNCTION__);
    } else if (lkpar & 0x00A0) {
        retspeed = (lkpar & 0x0080) ? 100000000 : 10000000;
    }
    return retspeed;
}

static uint64_t get_if_speed(const char *name)
{
    int fd;
    struct ifreq ifr;
    struct ethtool_cmd edata;

    memset(&ifr, 0, sizeof(ifr));
    edata.cmd = ETHTOOL_GSET;
    edata.speed = 0;
    strncpy(ifr.ifr_name, name, sizeof(ifr.ifr_name) - 1);
    ifr.ifr_data = &edata;
    if ((fd = get_ioctl_socket()) < 0) {
        error("\n%s: failed socket(AF_INET, SOCK_DGRAM, 0)", __FUNCTION__);
        return 0;
    }
    if (ioctl(fd, SIOCETHTOOL, &ifr) == -1) {
        warn("\n%s: failed ioctl(%d, SIOCETHTOOL, ...) for %s", __FUNCTION__,
             fd, name);
        return get_if_speed_mii(name);
    }
    if (edata.speed != SPEED_10 && edata.speed != SPEED_100
#ifdef SPEED_10000
        && edata.speed != SPEED_10000
#endif
#ifdef SPEED_2500
        && edata.speed != SPEED_2500
#endif
        && edata.speed != SPEED_1000 ) {
        return get_if_speed_mii(name);
    }
    return edata.speed * 1000LL * 1000LL;
}

static uint64_t guess_if_speed(int32_t if_type)
{
    if (if_type == IF_TYPE(ethernetCsmacd))
        return 10000000;

    else if (if_type == IF_TYPE(softwareLoopback))
        return 10000000;

    else if (if_type == IF_TYPE(iso88025TokenRing))
        return 4000000;

    else
        return 0;
}


/******** get ifc list and info provided by ioctl ********/
void update_if_entries(if_entry_list_t * if_list)
{
    int sock;
    size_t i;
    struct ifreq ifreq;
    uint64_t speed;
    if_entry_t *entry;

    TRACE_ENTRY();
    load_if_stats(if_list);
    memset(&ifreq, 0, sizeof(ifreq));
    sock = get_ioctl_socket();
    if (sock < 0) {
        error("\n%s: Failed to obtain socket(AF_INET, SOCK_STREAM, 0)",
              __FUNCTION__);
        return;
    }
    FOR_ENTRIES(if_list, entry, i) {
        strncpy(ifreq.ifr_name, entry->ifName, 16);
        if (ioctl(sock, SIOCGIFFLAGS, (char *) &ifreq) >= 0) {
            entry->ifFlags = ifreq.ifr_flags;
        } else {
            error("\n%s: Failed ioctl(SIOCGIFFLAGS)", __FUNCTION__);
        }
        if (ioctl(sock, SIOCGIFINDEX, (char *) &ifreq) >= 0) {
            entry->ifIndex = ifreq.ifr_ifindex;
        } else {
            error("\n%s: Failed ioctl(SIOCGIFINDEX)", __FUNCTION__);
        }
        if (ioctl(sock, SIOCGIFMTU, (char *) &ifreq) >= 0) {
            entry->ifMtu = ifreq.ifr_mtu;
        } else {
            error("\n%s: Failed ioctl(SIOCGIFMTU)", __FUNCTION__);
        }
        entry->ifDescr[0] = 0;
        entry->ifType = IF_TYPE(other);
#ifdef LINUXCFG_INTERFACES
        entry->ifIanaType = IANAIF_TYPE(other);
#endif
        entry->ifPhysAddress_len = 0;
        memset(entry->ifPhysAddress, 0, sizeof(entry->ifPhysAddress));
        if (ioctl(sock, SIOCGIFHWADDR, (char *) &ifreq) >= 0) {
            if (ifreq.ifr_hwaddr.sa_data[0] || ifreq.ifr_hwaddr.sa_data[1]
                || ifreq.ifr_hwaddr.sa_data[2] || ifreq.ifr_hwaddr.sa_data[3]
                || ifreq.ifr_hwaddr.sa_data[4] || ifreq.ifr_hwaddr.sa_data[5]) {
                const char *manufact =
                    get_manufacturer((uint8_t) ifreq.ifr_hwaddr.sa_data[0],
                                     (uint8_t) ifreq.ifr_hwaddr.sa_data[1],
                                     (uint8_t) ifreq.ifr_hwaddr.sa_data[2]);

                strcpy(entry->ifDescr, manufact);
                memcpy(entry->ifPhysAddress, ifreq.ifr_hwaddr.sa_data,
                       IFHWADDRLEN);
                entry->ifPhysAddress_len = IFHWADDRLEN;
            }
            switch (ifreq.ifr_hwaddr.sa_family) {
            case ARPHRD_ETHER:
                entry->ifType = IF_TYPE(ethernetCsmacd);
#ifdef LINUXCFG_INTERFACES
                entry->ifIanaType = IANAIF_TYPE(ethernetCsmacd);
#endif
                break;
            case ARPHRD_TUNNEL:
            case ARPHRD_TUNNEL6:
            case ARPHRD_SIT:
                entry->ifType = IF_TYPE(tunnel);
#ifdef LINUXCFG_INTERFACES
                entry->ifIanaType = IANAIF_TYPE(tunnel);
#endif
                break;
            case ARPHRD_SLIP:
            case ARPHRD_CSLIP:
            case ARPHRD_SLIP6:
            case ARPHRD_CSLIP6:
                entry->ifType = IF_TYPE(slip);
#ifdef LINUXCFG_INTERFACES
                entry->ifIanaType = IANAIF_TYPE(slip);
#endif
                break;
            case ARPHRD_PPP:
                entry->ifType = IF_TYPE(ppp);
#ifdef LINUXCFG_INTERFACES
                entry->ifIanaType = IANAIF_TYPE(ppp);
#endif
                break;
            case ARPHRD_LOOPBACK:
                entry->ifType = IF_TYPE(softwareLoopback);
#ifdef LINUXCFG_INTERFACES
                entry->ifIanaType = IANAIF_TYPE(softwareLoopback);
#endif
                break;
            case ARPHRD_FDDI:
                entry->ifType = IF_TYPE(fddi);
#ifdef LINUXCFG_INTERFACES
                entry->ifIanaType = IANAIF_TYPE(fddi);
#endif
                break;
            case ARPHRD_ARCNET:
                entry->ifType = IF_TYPE(arcnet);
#ifdef LINUXCFG_INTERFACES
                entry->ifIanaType = IANAIF_TYPE(arcnet);
#endif
                break;
            case ARPHRD_LOCALTLK:
                entry->ifType = IF_TYPE(localTalk);
#ifdef LINUXCFG_INTERFACES
                entry->ifIanaType = IANAIF_TYPE(localTalk);
#endif
                break;
            case ARPHRD_HIPPI:
                entry->ifType = IF_TYPE(hippi);
#ifdef LINUXCFG_INTERFACES
                entry->ifIanaType = IANAIF_TYPE(hippi);
#endif
                break;
            case ARPHRD_ATM:
                entry->ifType = IF_TYPE(atm);
#ifdef LINUXCFG_INTERFACES
                entry->ifIanaType = IANAIF_TYPE(atm);
#endif
                break;
            default:
                entry->ifType = 1;
#ifdef LINUXCFG_INTERFACES
                entry->ifIanaType = IANAIF_TYPE(other);
#endif

            }
        } else {
            error("\n%s: Failed ioctl(SIOCGIFHWADDR)", __FUNCTION__);
        }
        if (1) {

            /*
               struct ethtool_stats {
               __u32   cmd;            // ETHTOOL_GSTATS
               __u32   n_stats;        // number of u64's being returned
               __u64   data[0];
               };
             */
            struct ethtool_drvinfo info;

            memset(&info, 0, sizeof(info));
            memset(&info, 0, sizeof(info));
            info.cmd = ETHTOOL_GDRVINFO;
            ifreq.ifr_data = (char *) &info;
            if (ioctl(sock, SIOCETHTOOL, (char *) &ifreq) >= 0) {
                if (strlen(info.driver)) {
                    strcpy(entry->ifDescr + strlen(entry->ifDescr), "  ");
                    strcpy(entry->ifDescr + strlen(entry->ifDescr),
                           info.driver);
                }
                if (strlen(info.version)) {
                    strcpy(entry->ifDescr + strlen(entry->ifDescr), "  ");
                    strcpy(entry->ifDescr + strlen(entry->ifDescr),
                           info.version);
                }
                if (strlen(info.fw_version)) {
                    strcpy(entry->ifDescr + strlen(entry->ifDescr), "  ");
                    strcpy(entry->ifDescr + strlen(entry->ifDescr),
                           info.fw_version);
                }
            } else {
                error
                    ("\n%s: Failed ioctl(SIOCETHTOOL, ETHTOOL_GDRVINFO),"
                     " for iface %s",
                     __FUNCTION__, entry->ifName);
            }
            speed =
                entry->ifType ==
                IF_TYPE(ethernetCsmacd) ? get_if_speed(entry->
                                                       ifName) :
                guess_if_speed(entry->ifType);
            if (speed > 0xFFFFFFFFLL) {
                entry->ifSpeed = 0xFFFFFFFF;
            } else {
                entry->ifSpeed = speed;
            }
            entry->ifHighSpeed = speed / 1000000LL;
        }
        entry->ifPromiscuousMode = entry->ifFlags & IFF_PROMISC ? 1 : 2;

        // up = 1, down = 2, testing = 3
        entry->ifAdminStatus = ((entry->ifFlags & IFF_UP) == 0) ? 2 :
            ((entry->ifFlags & IFF_DEBUG) ? 3 : 1);
        entry->ifOperStatus = ((entry->ifFlags & IFF_RUNNING) == 0) ? 2 :
            ((entry->ifFlags & IFF_DEBUG) ? 3 : 1);
        // TODO
        /******** NOT YET collected items must be initialized to a valid
                  value if 0 not valid (enumerations) ********/
        entry->ifLinkUpDownTrapEnable = 2;
        entry->ifConnectorPresent = entry->ifFlags & IFF_LOOPBACK ? 2 : 1;
        char fentry[256];

        GET_PAR_FILEENTRY(entry->if4RetransmitTime, fentry,
                          "/proc/sys/net/ipv4/neigh/%s/retrans_time_ms",
                          entry->ifName);
        GET_PAR_FILEENTRY(entry->if6RetransmitTime, fentry,
                          "/proc/sys/net/ipv6/neigh/%s/retrans_time_ms",
                          entry->ifName);
        GET_PAR_FILEENTRY(entry->if6ReachableTime, fentry,
                          "/proc/sys/net/ipv6/neigh/%s/base_reachable_time_ms",
                          entry->ifName);
        GET_PAR_FILEENTRY(entry->if6Forwarding, fentry,
                          "/proc/sys/net/ipv6/conf/%s/forwarding",
                          entry->ifName);
    }
    SORT_LIST(if_list);
    TRACE_EXIT();
}

int compare_if_entries(const if_entry_t * ie1, const if_entry_t * ie2)
{
    return UINT_CMP(ie1->ifIndex, ie2->ifIndex);
}

void update_if_stack_entry(ifStackEntry_list_t * list)
{
    // not implemented
}

int compare_if_stack_entry(const ifStackEntry_t * e1, const ifStackEntry_t * e2)
{
    return 1;                    // TODO
}

void update_if_rcv_address_entry(ifRcvAddressEntry_list_t * list)
{
    // not implemented
}

int compare_if_rcv_address_entry(const ifRcvAddressEntry_t * e1,
                                 const ifRcvAddressEntry_t * e2)
{
    return 1;                    // TODO
}
