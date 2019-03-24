#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <ctype.h>

#include <dirent.h>
#include <unistd.h>
#include <net/if.h>
#include <sys/ioctl.h>

#include <netinet/in.h>
#include <netinet/if_ether.h>
#include <linux/if_packet.h>

#include <stdint.h>
#include <asm/types.h>
#include <asm/socket.h>            /* arch-dependent defines       */
#include <linux/sockios.h>         /* the SIOCxxx I/O controls     */
#include <linux/types.h>           /* pid_t                        */
#include <linux/socket.h>
#include <errno.h>

#include <sys/ioctl.h>
#include <net/if.h>

#include "confd.h"
#include "linuxcfg_api.h"
#include "linuxcfg_util.h"

#include "utils.h"
#include "caches.h"

#include "IP-MIB.h"


#include "ip.h"
#include "if.h"

#define GET_IP_FILEENTRY(fld, ip_file) \
    GET_FILEENTRY(fld, "/proc/sys/net/ipv4/" ip_file)
#define GET_IPv6_FILEENTRY(fld, ipv6_file) \
    GET_FILEENTRY(fld, "/proc/sys/net/ipv6/" ipv6_file)

/**************** Tool section Begin ****************/
typedef char line_t[1024];

typedef struct
{
    char m_name[256];
    uint64_t m_value;
} counter_t;

typedef struct
{
    char m_addr[128];
    uint32_t m_idx;
    uint32_t m_prefix_len;
    uint32_t m_scope;
    uint32_t m_flags;
    char m_name[64];
} if_inet6_line_t;

typedef struct
{
    int bcastflg;
    int anycastflg;
    struct in_addr inp;
} address_flag_info_t;


/******** universal list container ********/

typedef struct
{
    char *m_items;
    size_t m_item_cnt;
    size_t m_item_size;
    size_t m_size;
} list_t;

static void init_list(list_t * list, size_t item_size, size_t item_cnt)
{
    memset(list, 0, sizeof(*list));

    list->m_item_size = item_size;
    list->m_size = item_size * item_cnt;

    list->m_items = (char *) malloc(list->m_size);
    memset(list->m_items, 0, list->m_size);
}

static void reset_list(list_t * list)
{
    if (list->m_items && list->m_size) {
        memset(list->m_items, 0, list->m_size);
    }
    list->m_item_cnt = 0;
}

static void free_list(list_t * list)
{
    if (list->m_items)
        free(list->m_items);
    memset(list, 0, sizeof(*list));
}

static void *get_list_item(list_t * list, size_t idx)
{
    if (idx < list->m_item_cnt) {
        return list->m_items + (idx * list->m_item_size);
    } else
        return NULL;
}

static void *new_list_item(list_t * list)
{
    list->m_item_cnt++;
    if ((list->m_item_cnt * list->m_item_size) > list->m_size) {
        char *items = list->m_items;
        size_t size = list->m_size;

        list->m_size <<= 1;
        list->m_items = (char *) malloc(list->m_size);
        memset(list->m_items, 0, list->m_size);
        memcpy(list->m_items, items, size);
        free(items);
    }
    return list->m_items + (list->m_item_cnt - 1) * list->m_item_size;
}
static void *add_list_item(list_t * list, void *value)
{
    void *item;

    list->m_item_cnt++;
    if ((list->m_item_cnt * list->m_item_size) > list->m_size) {
        char *items = list->m_items;
        size_t size = list->m_size;

        list->m_size <<= 1;
        list->m_items = (char *) malloc(list->m_size);
        memset(list->m_items, 0, list->m_size);
        memcpy(list->m_items, items, size);
        free(items);
    }
    item = list->m_items + (list->m_item_cnt - 1) * list->m_item_size;
    memcpy(item, value, list->m_item_size);
    return item;
}

static size_t list_size(list_t * list)
{
    return list->m_item_cnt;
}

/******** line parsing support ********/

static int load_uint64_list(list_t * list, char *line)
{
    uint64_t value;
    char *pos = line;

    init_list(list, sizeof(value), 64);
    while (get_uint64_dec(&pos, &value) >= 0) {
        add_list_item(list, &value);
    }
    return list_size(list);
}
static int load_name_list(list_t * list, char *line)
{
    char name[64];
    char *pos = line;

    init_list(list, sizeof(name), 64);
    while (get_name(&pos, name) >= 0) {
        add_list_item(list, name);
    }
    return list_size(list);
}

/******** file parsing support ********/

static int load_file_lines(const char *file, list_t * list)
{
    FILE *fp;

    if (!file || !list) {
        perror("file = NULL || list = NULL");
        return -1;
    }
    line_t line;

    init_list(list, sizeof(line_t), 256);
    if ((fp = fopen(file, "r")) == NULL) {
        error("%s - Failed fopen(\"%s\") : \"%s\"", __FUNCTION__, file,
              strerror(errno));
        return -1;
    }
    for (; fgets(line, sizeof(line), fp) == line;) {
        if (!empty_line(line)) {
            add_list_item(list, line);
        }
    }
    fclose(fp);
    return 0;
}
static int load_cnt_list_by_lines(const char *file, list_t * list)
{
    int cc = 0, i;
    list_t lines;

    cc = load_file_lines(file, &lines);
    if ((cc < 0) || (list_size(&lines) < 1)) {
        return -1;
    }
    for (i = 0; i < list_size(&lines); i++) {
        char *pos = get_list_item(&lines, i);
        counter_t counter;

        memset(&counter, 0, sizeof(counter));
        if ((get_name(&pos, (char *) counter.m_name) >= 0)
            && (get_uint64_dec(&pos, &counter.m_value) >= 0)) {
            add_list_item(list, &counter);
        }
    }
    free_list(&lines);

    return 0;
}
static int load_cnt_list_by_columns(const char *file, list_t * list,
                             const char *line_name)
{
    int i, cnt_names, cnt_values;
    list_t lines, name_list, value_list;
    char *names = NULL;
    char *values = NULL;

    if ((load_file_lines(file, &lines) < 0) || (list_size(&lines) < 2)) {
        error("%s - Failed load_file_lines(\"%s\") or LineCnt = %d\n",
              __FUNCTION__, file, list_size(&lines));
        return -1;
    }
    if (line_name == NULL) {
        if (list_size(&lines) != 2) {
            error("%s - Loaded %d lines from \"%s\", expected 2\n",
                  __FUNCTION__, list_size(&lines), file);
            return -1;
        }
        names = (char *) get_list_item(&lines, 0);
        values = (char *) get_list_item(&lines, 1);
    } else {
        for (i = 0; i < list_size(&lines); i++) {
            char *pos = get_list_item(&lines, i);

            if ((pos = strstr(pos, line_name)) != NULL) {
                if (names == NULL) {
                    names = pos + strlen(line_name);
                } else {
                    values = pos + strlen(line_name);
                    break;
                }
            }
        }
        if (!names || !values) {
            error
                ("%s - failed to find \"%s\" prefixed name or value "
                 "line in \"%s\"\n",
                 __FUNCTION__, line_name, file);
            return -1;
        }
    }
    cnt_names = load_name_list(&name_list, names);
    cnt_values = load_uint64_list(&value_list, values);
    if ((cnt_names < 1) || (cnt_values != cnt_names)) {
        if (line_name == NULL) {
            error("\n%s - mame cnt %d != value cnt %d in \"%s\"", __FUNCTION__,
                  file);
        } else {
            error
                ("\n%s - mame cnt %d != value cnt %d in \"%s\" lines "
                 "of  \"%s\"",
                 __FUNCTION__, line_name, file);
        }
        return -1;
    }
    for (i = 0; i < cnt_names; i++) {
        counter_t counter;

        memset(&counter, 0, sizeof(counter));
        strcpy(counter.m_name, get_list_item(&name_list, i));
        counter.m_value = *((uint64_t *) get_list_item(&value_list, i));
        add_list_item(list, &counter);
    }
    free_list(&lines);
    free_list(&name_list);
    free_list(&value_list);

    return 0;
}


static int load_if_inet6_line_list(list_t * list)
{
    FILE *file;
    const char *filename = "/proc/net/if_inet6";
    char line[1024];
    if_inet6_line_t if_inet6_line;

    init_list(list, sizeof(if_inet6_line_t), 16);
    if (!(file = fopen(filename, "r"))) {
        error("%s - Failed fopen(\"%s\") : \"%s\"", __FUNCTION__, filename,
              strerror(errno));
        return -1;
    }
    while (fgets(line, sizeof(line), file)) {
        char *pos = line;

        memset(&if_inet6_line, 0, sizeof(if_inet6_line));
        if ((get_name(&pos, if_inet6_line.m_addr) < 0)
            || (get_uint_hex(&pos, &if_inet6_line.m_idx) < 0)
            || (get_uint_hex(&pos, &if_inet6_line.m_prefix_len) < 0)
            || (get_uint_hex(&pos, &if_inet6_line.m_scope) < 0)
            || (get_uint_hex(&pos, &if_inet6_line.m_flags) < 0)
            || (get_name(&pos, if_inet6_line.m_name) < 0)) {
            error("%s - Unexpected line format \"%s\" in \"%s\"", __FUNCTION__,
                  line, filename);
            continue;
        }
        add_list_item(list, &if_inet6_line);
    }
    fclose(file);
    return 0;
}


/******** search in counter list ********/

static int get_counter_by_name(list_t * list, const char *name,
                               uint64_t * value)
{
    int i;

    for (i = 0; i < list_size(list); i++) {
        counter_t *counter = (counter_t *) get_list_item(list, i);

        if (strcasecmp(counter->m_name, name) == 0) {
            *value = counter->m_value;
            return 0;
        }
    }
    *value = 0;
    return -1;
}

#define CHECK_GET_COUNTER(list, name, value, file) do {                 \
        if (get_counter_by_name(list, name, value) < 0) {               \
            error("\n%s - Failed to get counter \"%s\" from \"%s\"",    \
                  __FUNCTION__, name, file);                            \
        }                                                               \
    } while(0)

static int ipaddress_ipv4_prefix_len(in_addr_t mask)
{
    int len = 0;

    while ((0xff000000 & mask) == 0xff000000) {
        len += 8;
        mask = mask << 8;
    }
    while (0x80000000 & mask) {
        ++len;
        mask = mask << 1;
    }
    return len;
}

typedef union
{
    struct in_addr m_addr4;
    struct in6_addr m_addr6;
} u_addr_t;

enum
{
    eIFA_UNSPEC = (1),
    eIFA_ADDRESS = (1 << 1),
    eIFA_LOCAL = (1 << 2),
    eIFA_LABEL = (1 << 3),
    eIFA_BROADCAST = (1 << 4),
    eIFA_ANYCAST = (1 << 5),
    eIFA_CACHEINFO = (1 << 6),
} eFLAGS;

typedef struct
{
    struct ifaddrmsg m_iface;
    struct ifa_cacheinfo m_cacheinfo;
    char m_name[254];
    u_addr_t m_addr;
    u_addr_t m_addr_local;
    u_addr_t m_addr_broadcast;
    u_addr_t m_addr_anycast;
    uint32_t m_flags;

} netlink_addr_info_t;

/**
 * @param list initialized list of netlink_addr_info_t
 */
static int get_netlink_addr_info(list_t * list)
{
    int i;
    struct rtattr *rta;
    int status;
    char buf[16384];
    struct nlmsghdr *nlmp;
    struct ifaddrmsg *rtmp;
    struct rtattr *rtatp;
    int rtattrlen;
    struct
    {
        struct nlmsghdr n;
        struct ifaddrmsg r;
        char buf[1024];
    } req;

    for (i = 0; i < 2; i++) {
        // better use request with implicit INET type
        int ifa_family = i == 0 ? AF_INET : AF_INET6;

        memset(&req, 0, sizeof(req));
        req.n.nlmsg_len = NLMSG_LENGTH(sizeof(struct ifaddrmsg));
        req.n.nlmsg_flags = NLM_F_REQUEST | NLM_F_ROOT;
        req.n.nlmsg_type = RTM_GETADDR;    // RTM_GETPREFIX not supported

        req.r.ifa_family = ifa_family;

        rta =
            (struct rtattr *) (((char *) &req) + NLMSG_ALIGN(req.n.nlmsg_len));
        rta->rta_len = RTA_LENGTH(16);

        // DO NOT reuse socket
        int fd = socket(PF_NETLINK, SOCK_DGRAM, NETLINK_ROUTE);
        if (fd < 0) {
            error
                ("\n%s - Failed socket(AF_NETLINK, SOCK_DGRAM, "
                 "NETLINK_ROUTE) : err \"%s\"",
                 __FUNCTION__, strerror(errno));
            continue;
        }
        status = send(fd, &req, req.n.nlmsg_len, 0);
        if (status < 0) {
            error("\n%s - Failed send() : err \"%s\"", __FUNCTION__,
                  strerror(errno));
            close(fd);
            continue;
        }
        status = recv(fd, buf, sizeof(buf), 0);
        if (status < 0) {
            error("\n%s - Failed recv() : err \"%s\"", __FUNCTION__,
                  strerror(errno));
            close(fd);
            continue;
        }
        if (status == 0) {
            close(fd);
            continue;
        }
        for (nlmp = (struct nlmsghdr *) buf; status > sizeof(*nlmp);) {
            netlink_addr_info_t info_item;

            int len = nlmp->nlmsg_len;
            int req_len = len - sizeof(*nlmp);

            if (req_len < 0 || len > status) {
                error("\n%s - netlink message parse error", __FUNCTION__);
                break;
            }
            if (!NLMSG_OK(nlmp, status)) {
                error("\n%s - netlink message not OK", __FUNCTION__);
                break;
            }
            rtmp = (struct ifaddrmsg *) NLMSG_DATA(nlmp);
            rtatp = (struct rtattr *) IFA_RTA(rtmp);
            memset(&info_item, 0, sizeof(info_item));
            memcpy(&info_item.m_iface, rtmp, sizeof(info_item.m_iface));

            rtattrlen = IFA_PAYLOAD(nlmp);
            for (; RTA_OK(rtatp, rtattrlen);
                 rtatp = RTA_NEXT(rtatp, rtattrlen)) {
                if (rtatp->rta_type == IFA_CACHEINFO) {
                    memcpy(&info_item.m_cacheinfo, RTA_DATA(rtatp),
                           sizeof(info_item.m_cacheinfo));
                    info_item.m_flags |= eIFA_CACHEINFO;

                } else if (rtatp->rta_type == IFA_ADDRESS) {
                    if (rtmp->ifa_family == AF_INET) {
                        memcpy(&info_item.m_addr.m_addr4, RTA_DATA(rtatp),
                               sizeof(struct in_addr));
                    } else if (rtmp->ifa_family == AF_INET6) {
                        memcpy(&info_item.m_addr.m_addr6, RTA_DATA(rtatp),
                               sizeof(struct in6_addr));
                    }
                    info_item.m_flags |= eIFA_ADDRESS;

                } else if (rtatp->rta_type == IFA_LOCAL) {
                    if (rtmp->ifa_family == AF_INET) {
                        memcpy(&info_item.m_addr_local.m_addr4, RTA_DATA(rtatp),
                               sizeof(struct in_addr));
                    } else if (rtmp->ifa_family == AF_INET6) {
                        memcpy(&info_item.m_addr_local.m_addr6, RTA_DATA(rtatp),
                               sizeof(struct in6_addr));
                    }
                    info_item.m_flags |= eIFA_LOCAL;

                } else if (rtatp->rta_type == IFA_BROADCAST) {
                    if (rtmp->ifa_family == AF_INET) {
                        memcpy(&info_item.m_addr_broadcast.m_addr4,
                               RTA_DATA(rtatp), sizeof(struct in_addr));
                    } else if (rtmp->ifa_family == AF_INET6) {
                        memcpy(&info_item.m_addr_broadcast.m_addr6,
                               RTA_DATA(rtatp), sizeof(struct in6_addr));
                    }
                    info_item.m_flags |= eIFA_BROADCAST;

                } else if (rtatp->rta_type == IFA_ANYCAST) {
                    if (rtmp->ifa_family == AF_INET) {
                        memcpy(&info_item.m_addr_anycast.m_addr4,
                               RTA_DATA(rtatp), sizeof(struct in_addr));
                    } else if (rtmp->ifa_family == AF_INET6) {
                        memcpy(&info_item.m_addr_anycast.m_addr6,
                               RTA_DATA(rtatp), sizeof(struct in6_addr));
                    }
                    info_item.m_flags |= eIFA_ANYCAST;

                } else if (rtatp->rta_type == IFA_LABEL) {
                    strncpy(info_item.m_name, (const char *) RTA_DATA(rtatp),
                            sizeof(info_item.m_name));
                    info_item.m_flags |= eIFA_LABEL;

                } else {
                    // TODO find what
                    continue;
                }
            }
            memcpy(new_list_item(list), &info_item, sizeof(info_item));

            status -= NLMSG_ALIGN(len);
            nlmp = (struct nlmsghdr *) ((char *) nlmp + NLMSG_ALIGN(len));
        }
        close(fd);
    }

    return 0;
}

static int mask_prefix(inet_address_t * addr, size_t prefix_len)
{
    int i;
    int pref = prefix_len;
    uint8_t *byte = addr->address;

    if (!addr)
        return -1;
    for (i = 0; i < addr->addr_len; i++) {
        if (pref < 1) {
            byte[i] = 0;
        } else if (pref < 8) {
            byte[i] &= ~((1 << (8 - pref)) - 1);
        }
        pref -= 8;
    }
    return 0;
}


static int decide_address_prefix_origin(void *addr, size_t addr_len,
                                        uint32_t flags,
                                        int *addr_origin, int *prefix_origin)
{
    struct in_addr *addr4;
    struct in6_addr *addr6;
    int addr_org, pref_org;

    if (!addr)
        return -1;
    if (addr_len == sizeof(struct in_addr)) {
        addr4 = (struct in_addr *) addr;
        if ((addr4->s_addr << 16) == 0xFEA90000) {    // 169.254.x.x
            addr_org = IPADDRESSORIGINTC_RANDOM;
            pref_org = IPADDRESSPREFIXORIGINTC_WELLKNOWN;
        } else {
            addr_org = IPADDRESSORIGINTC_MANUAL;
            pref_org = IPADDRESSPREFIXORIGINTC_MANUAL;
        }
    } else if (addr_len == sizeof(struct in6_addr)) {
        addr6 = (struct in6_addr *) addr;
        if (!flags) {
            addr_org = IPADDRESSORIGINTC_LINKLAYER;
            pref_org = IPADDRESSPREFIXORIGINTC_ROUTERADV;
        } else if (flags & IFA_F_TEMPORARY) {
            addr_org = IPADDRESSORIGINTC_RANDOM;
            pref_org = IPADDRESSPREFIXORIGINTC_ROUTERADV;
        } else if (IN6_IS_ADDR_LINKLOCAL(addr6)) {
            addr_org = IPADDRESSORIGINTC_LINKLAYER;
            pref_org = IPADDRESSPREFIXORIGINTC_WELLKNOWN;
        } else {
            addr_org = IPADDRESSORIGINTC_MANUAL;
            pref_org = IPADDRESSPREFIXORIGINTC_MANUAL;
        }
    } else {
        return -1;
    }
    if (addr_origin)
        *addr_origin = addr_org;
    if (prefix_origin)
        *prefix_origin = pref_org;

    return 0;
}


/**************** Tool section End ****************/


static int complete_systemstats_entry(ipSystemStatsEntry_t * entry)
{
    entry->ipSystemStatsInForwDatagrams = entry->ipSystemStatsInNoRoutes +
        entry->ipSystemStatsHCOutForwDatagrams;
    entry->ipSystemStatsOutFragReqds = entry->ipSystemStatsOutFragOKs +
        entry->ipSystemStatsOutFragFails;
    entry->ipSystemStatsOutTransmits = entry->ipSystemStatsHCOutRequests +
        entry->ipSystemStatsHCOutForwDatagrams +
        entry->ipSystemStatsOutFragCreates - entry->ipSystemStatsOutFragReqds -
        entry->ipSystemStatsOutNoRoutes - entry->ipSystemStatsOutDiscards;
    return 0;
}

#define GET_SYSSTATS_COUNTER(list, name, entry, field, file) do {    \
        uint64_t new;                                                \
        CHECK_GET_COUNTER(list, name, &new, file);                    \
        UPDATE_COUNTER_PAIR(new,                                    \
                            entry->ipSystemStats ## field,            \
                            entry->ipSystemStatsHC ## field);        \
    } while(0)

static int update_ipv4_system_stats(ipSystemStatsEntry_t * entry)
{
    const char *file = "/proc/net/snmp";
    const char *line_name = "Ip:";
    list_t list;

    init_list(&list, sizeof(counter_t), 256);
    entry->ipSystemStatsIPVersion = INET(ipv4);

    if (load_cnt_list_by_columns(file, &list, line_name) < 0) {
        error
            ("%s - Failed to parse \"%s\" for \"%s\" lines (IPv4 System Stats)",
             __FUNCTION__, file, line_name);
    } else {
        GET_SYSSTATS_COUNTER(&list, "InReceives", entry, InReceives, file);
        CHECK_GET_COUNTER(&list, "InHdrErrors",
                          &entry->ipSystemStatsInHdrErrors, file);
        CHECK_GET_COUNTER(&list, "InAddrErrors",
                          &entry->ipSystemStatsInAddrErrors, file);
        GET_SYSSTATS_COUNTER(&list, "ForwDatagrams", entry, OutForwDatagrams,
                             file);
        CHECK_GET_COUNTER(&list, "InUnknownProtos",
                          &entry->ipSystemStatsInUnknownProtos, file);
        CHECK_GET_COUNTER(&list, "InDiscards", &entry->ipSystemStatsInDiscards,
                          file);
        GET_SYSSTATS_COUNTER(&list, "InDelivers", entry, InDelivers, file);
        GET_SYSSTATS_COUNTER(&list, "OutRequests", entry, OutRequests, file);
        CHECK_GET_COUNTER(&list, "OutDiscards",
                          &entry->ipSystemStatsOutDiscards, file);
        CHECK_GET_COUNTER(&list, "OutNoRoutes",
                          &entry->ipSystemStatsOutNoRoutes, file);
        CHECK_GET_COUNTER(&list, "ReasmReqds", &entry->ipSystemStatsReasmReqds,
                          file);
        CHECK_GET_COUNTER(&list, "ReasmOKs", &entry->ipSystemStatsReasmOKs,
                          file);
        CHECK_GET_COUNTER(&list, "ReasmFails", &entry->ipSystemStatsReasmFails,
                          file);
        CHECK_GET_COUNTER(&list, "FragOKs", &entry->ipSystemStatsOutFragOKs,
                          file);
        CHECK_GET_COUNTER(&list, "FragFails", &entry->ipSystemStatsOutFragFails,
                          file);
        CHECK_GET_COUNTER(&list, "FragCreates",
                          &entry->ipSystemStatsOutFragCreates, file);
    }

    file = "/proc/net/netstat";
    line_name = "IpExt:";
    reset_list(&list);
    if (load_cnt_list_by_columns(file, &list, line_name) < 0) {
        error
            ("\n%s - Failed to parse \"%s\" for \"%s\" "
             "lines (IPv4 System Statistics)",
             __FUNCTION__, file, line_name);
        free_list(&list);
        return -1;
    }
    CHECK_GET_COUNTER(&list, "InNoRoutes", &entry->ipSystemStatsInNoRoutes,
                      file);
    CHECK_GET_COUNTER(&list, "InTruncatedPkts",
                      &entry->ipSystemStatsInTruncatedPkts, file);
    GET_SYSSTATS_COUNTER(&list, "InMcastPkts", entry, InMcastPkts, file);
    GET_SYSSTATS_COUNTER(&list, "OutMcastPkts", entry, OutMcastPkts, file);
    GET_SYSSTATS_COUNTER(&list, "InBcastPkts", entry, InBcastPkts, file);
    GET_SYSSTATS_COUNTER(&list, "OutBcastPkts", entry, OutBcastPkts, file);

    complete_systemstats_entry(entry);
    free_list(&list);
    return 0;
}


static int update_ipv6_system_stats(ipSystemStatsEntry_t * entry)
{
    const char *file = "/proc/net/snmp6";
    list_t list;

    init_list(&list, sizeof(counter_t), 256);
    entry->ipSystemStatsIPVersion = INET(ipv6);

    if (load_cnt_list_by_lines(file, &list) < 0) {
        error("\n%s - Failed to parse \"%s\" (IPv6 System Statistics)",
              __FUNCTION__, file);
        free_list(&list);
        return -1;
    }
    GET_SYSSTATS_COUNTER(&list, "Ip6InReceives", entry, InReceives, file);
    CHECK_GET_COUNTER(&list, "Ip6InHdrErrors", &entry->ipSystemStatsInHdrErrors,
                      file);
    CHECK_GET_COUNTER(&list, "Ip6InNoRoutes", &entry->ipSystemStatsInNoRoutes,
                      file);
    CHECK_GET_COUNTER(&list, "Ip6InAddrErrors",
                      &entry->ipSystemStatsInAddrErrors, file);
    CHECK_GET_COUNTER(&list, "Ip6InUnknownProtos",
                      &entry->ipSystemStatsInUnknownProtos, file);
    CHECK_GET_COUNTER(&list, "Ip6InTruncatedPkts",
                      &entry->ipSystemStatsInTruncatedPkts, file);
    CHECK_GET_COUNTER(&list, "Ip6InDiscards", &entry->ipSystemStatsInDiscards,
                      file);
    GET_SYSSTATS_COUNTER(&list, "Ip6InDelivers", entry, InDelivers, file);
    GET_SYSSTATS_COUNTER(&list, "Ip6OutForwDatagrams", entry, OutForwDatagrams,
                         file);
    GET_SYSSTATS_COUNTER(&list, "Ip6OutRequests", entry, OutRequests, file);
    CHECK_GET_COUNTER(&list, "Ip6OutDiscards", &entry->ipSystemStatsOutDiscards,
                      file);
    CHECK_GET_COUNTER(&list, "Ip6OutNoRoutes", &entry->ipSystemStatsOutNoRoutes,
                      file);
    CHECK_GET_COUNTER(&list, "Ip6ReasmReqds", &entry->ipSystemStatsReasmReqds,
                      file);
    CHECK_GET_COUNTER(&list, "Ip6ReasmOKs", &entry->ipSystemStatsReasmOKs,
                      file);
    CHECK_GET_COUNTER(&list, "Ip6ReasmFails", &entry->ipSystemStatsReasmFails,
                      file);
    CHECK_GET_COUNTER(&list, "Ip6FragOKs", &entry->ipSystemStatsOutFragOKs,
                      file);
    CHECK_GET_COUNTER(&list, "Ip6FragFails", &entry->ipSystemStatsOutFragFails,
                      file);
    CHECK_GET_COUNTER(&list, "Ip6FragCreates",
                      &entry->ipSystemStatsOutFragCreates, file);
    GET_SYSSTATS_COUNTER(&list, "Ip6InMcastPkts", entry, InMcastPkts, file);
    GET_SYSSTATS_COUNTER(&list, "Ip6OutMcastPkts", entry, OutMcastPkts, file);
    // the rest is not available in /proc/net/snmp6 :
    // ipSystemStatsInOctets, ipSystemStatsOutOctets,
    // ipSystemStatsInMcastOctets,
    // ipSystemStatsOutMcastOctets, ipSystemStatsInBcastPkts,
    // ipSystemStatsOutBcastPkts

    complete_systemstats_entry(entry);
    free_list(&list);
    return 0;
}

#define GET_IFSTATS_COUNTER(list, name, entry, field, file) do {        \
        uint64_t new;                                                   \
        CHECK_GET_COUNTER(list, name, &new, file);                      \
        UPDATE_COUNTER_PAIR(new, entry->ipIfStats ## field,             \
                            entry->ipIfStatsHC ## field);               \
    } while(0)

int update_ipv6_if_stats(ipIfStatsEntry_list_t * stats)
{
    DIR *dir;
    const char *dirname = "/proc/net/dev_snmp6";
    struct dirent *dir_entry;
    char file[256];
    int cc = 0;
    uint64_t val;

    list_t list;

    init_list(&list, sizeof(counter_t), 256);
    if ((dir = opendir(dirname)) == NULL) {
        error("\n%s - Failed opendir(\"%s\") for ipv6_if_stats(ipIfStats",
              __FUNCTION__, dirname);
        return -1;
    }
    while ((dir_entry = readdir(dir)) != NULL) {
        if (dir_entry->d_name[0] == '.') {
            continue;
        }
        snprintf(file, 256, "%s/%s", dirname, dir_entry->d_name);

        reset_list(&list);
        if (load_cnt_list_by_lines(file, &list) < 0) {
            error("\n%s - Failed to parse \"%s\" for Ipv6IfStats(ipIfStats",
                  __FUNCTION__, file);
            continue;
        }
        ipIfStatsEntry_t *entry = NEW_ENTRY(stats);

        entry->ipIfStatsIPVersion = INET(ipv6);
        val = 0;
        CHECK_GET_COUNTER(&list, "ifIndex", &val, file);
        entry->ipIfStatsIfIndex = val;
        GET_IFSTATS_COUNTER(&list, "Ip6InReceives", entry, InReceives, file);
        CHECK_GET_COUNTER(&list, "Ip6InHdrErrors", &entry->ipIfStatsInHdrErrors,
                          file);
        CHECK_GET_COUNTER(&list, "Ip6InNoRoutes", &entry->ipIfStatsInNoRoutes,
                          file);
        CHECK_GET_COUNTER(&list, "Ip6InAddrErrors",
                          &entry->ipIfStatsInAddrErrors, file);
        CHECK_GET_COUNTER(&list, "Ip6InUnknownProtos",
                          &entry->ipIfStatsInUnknownProtos, file);
        CHECK_GET_COUNTER(&list, "Ip6InTruncatedPkts",
                          &entry->ipIfStatsInTruncatedPkts, file);
        CHECK_GET_COUNTER(&list, "Ip6InDiscards", &entry->ipIfStatsInDiscards,
                          file);
        GET_IFSTATS_COUNTER(&list, "Ip6InDelivers", entry, InDelivers, file);
        GET_IFSTATS_COUNTER(&list, "Ip6OutForwDatagrams", entry,
                            OutForwDatagrams, file);
        GET_IFSTATS_COUNTER(&list, "Ip6OutRequests", entry, OutRequests, file);
        CHECK_GET_COUNTER(&list, "Ip6OutDiscards", &entry->ipIfStatsOutDiscards,
                          file);
        CHECK_GET_COUNTER(&list, "Ip6OutNoRoutes", &entry->ipIfStatsOutNoRoutes,
                          file);
        CHECK_GET_COUNTER(&list, "Ip6ReasmReqds", &entry->ipIfStatsReasmReqds,
                          file);
        CHECK_GET_COUNTER(&list, "Ip6ReasmOKs", &entry->ipIfStatsReasmOKs,
                          file);
        CHECK_GET_COUNTER(&list, "Ip6ReasmFails", &entry->ipIfStatsReasmFails,
                          file);
        CHECK_GET_COUNTER(&list, "Ip6FragOKs", &entry->ipIfStatsOutFragOKs,
                          file);
        CHECK_GET_COUNTER(&list, "Ip6FragFails", &entry->ipIfStatsOutFragFails,
                          file);
        CHECK_GET_COUNTER(&list, "Ip6FragCreates",
                          &entry->ipIfStatsOutFragCreates, file);
        GET_IFSTATS_COUNTER(&list, "Ip6InMcastPkts", entry, InMcastPkts, file);
        GET_IFSTATS_COUNTER(&list, "Ip6OutMcastPkts", entry, OutMcastPkts,
                            file);

        entry->ipIfStatsInForwDatagrams = entry->ipIfStatsInNoRoutes +
            entry->ipIfStatsHCOutForwDatagrams;
        entry->ipIfStatsOutFragReqds = entry->ipIfStatsOutFragOKs +
            entry->ipIfStatsOutFragFails;
        entry->ipIfStatsOutTransmits = entry->ipIfStatsHCOutRequests +
            entry->ipIfStatsHCOutForwDatagrams +
            entry->ipIfStatsOutFragCreates - entry->ipIfStatsOutFragReqds -
            entry->ipIfStatsOutNoRoutes - entry->ipIfStatsOutDiscards;

        // the rest is not available on Linux:
        // ipIfStatsInOctets, ipIfStatsOutOctets, ipIfStatsInMcastOctets,
        // ipIfStatsOutMcastOctets, ipIfStatsInBcastPkts, ipIfStatsOutBcastPkts

        cc++;
    }
    closedir(dir);
    free_list(&list);
    return cc;
}

/**************** Interface ****************/
void update_ip_system_stats(ipSystemStatsEntry_table_t * stats)
{
    update_ipv4_system_stats(stats->entries);
    update_ipv6_system_stats(stats->entries + 1);
}

void update_ip_if_stats(ipIfStatsEntry_list_t * stats)
{
    TRACE_ENTRY();
    RESET_LIST(stats);

    update_ipv6_if_stats(stats);

    SORT_LIST(stats);
    TRACE_EXIT();
}

void update_ip_basic_stats(ip_basic_stats_t * stats)
{
    TRACE_ENTRY();
    GET_IP_FILEENTRY(stats->ipForwarding, "ip_forward");
    GET_IP_FILEENTRY(stats->ipDefaultTTL, "ip_default_ttl");
    GET_IP_FILEENTRY(stats->ipReasmTimeout, "ipfrag_time");
    GET_IPv6_FILEENTRY(stats->ipv6IpForwarding, "conf/all/forwarding");
    GET_IPv6_FILEENTRY(stats->ipv6IpDefaultHopLimit, "conf/default/hop_limit");
    TRACE_EXIT();
}

int compare_ip_if_stats(const ipIfStatsEntry_t * s1,
                        const ipIfStatsEntry_t * s2)
{
    return s1->ipIfStatsIPVersion - s2->ipIfStatsIPVersion ? :
        UINT_CMP(s1->ipIfStatsIfIndex, s2->ipIfStatsIfIndex);
}
int compare_ip_def_router(const ipDefaultRouterEntry_t * s1,
                          const ipDefaultRouterEntry_t * s2)
{
    return
        s1->ipDefaultRouterAddressType - s2->ipDefaultRouterAddressType ? :
        (INET_ADDR_CMP(s1->ipDefaultRouterAddress,
                       s2->ipDefaultRouterAddress) ? :
         (UINT_CMP(s1->ipDefaultRouterIfIndex, s2->ipDefaultRouterIfIndex)));
}

int compare_ip6_scope_zone_index(const ipv6ScopeZoneIndexEntry_t * s1,
                                 const ipv6ScopeZoneIndexEntry_t * s2)
{
    return UINT_CMP(s1->ipv6ScopeZoneIndexIfIndex,
                    s2->ipv6ScopeZoneIndexIfIndex);
}

int compare_ip_address(const ipAddressEntry_t * s1, const ipAddressEntry_t * s2)
{
    return
        s1->ipAddressAddrType - s2->ipAddressAddrType ? :
        INET_ADDR_CMP(s1->ipAddressAddr, s2->ipAddressAddr);
}

static int process_route(struct nlmsghdr *nlmsghdr_p, void *arg)
{
    ipDefaultRouterEntry_list_t *stats = arg;
    static long hz = -1;

    if (hz < 0)
        hz = sysconf(_SC_CLK_TCK);

    struct rtattr *rtattr_p;
    struct rta_cacheinfo *rta_cacheinfo_p;
    struct rtmsg *rtmsg_p = NLMSG_DATA(nlmsghdr_p);
    int rt_cnt;
    u_char addresstype;
    char address[64];            // [NETSNMP_ACCESS_DEFAULTROUTER_BUF_SIZE + 1]
    size_t address_len = 0;
    int if_index = -1;
    u_long lifetime = 0;
    int preference = -3;

    if (rtmsg_p->rtm_family == AF_INET) {
        addresstype = INET(ipv4);
        lifetime = 65535;        // infinity
    } else if (rtmsg_p->rtm_family == AF_INET6) {
        addresstype = INET(ipv6);
        lifetime = 0;            // use RTA_CACHEINFO
    } else {
        return 0;
    }

    preference = 0;

    rtattr_p = RTM_RTA(rtmsg_p);
    rt_cnt = RTM_PAYLOAD(nlmsghdr_p);
    while (RTA_OK(rtattr_p, rt_cnt)) {
        switch (rtattr_p->rta_type) {
        case RTA_OIF:
            if_index = *(int *) (RTA_DATA(rtattr_p));
            break;
        case RTA_GATEWAY:
            address_len = RTA_PAYLOAD(rtattr_p);
            memset(address, 0, sizeof(address));
            memcpy(address, RTA_DATA(rtattr_p), address_len);
            break;
        case RTA_CACHEINFO:
            rta_cacheinfo_p = RTA_DATA(rtattr_p);
            if ((rtmsg_p->rtm_flags & RTM_F_CLONED) ||
                (rta_cacheinfo_p && rta_cacheinfo_p->rta_expires)) {
                lifetime = rta_cacheinfo_p->rta_expires / hz;
            }
            break;
        default:
            break;
        }
        rtattr_p = RTA_NEXT(rtattr_p, rt_cnt);
    }
    if (address_len && (if_index >= 0) && lifetime && (preference != -3)) {
        ipDefaultRouterEntry_t *entry = NEW_ENTRY(stats);

        entry->ipDefaultRouterAddress.addr_len =
            (rtmsg_p->rtm_family == AF_INET) ? 4 : 16;
        memcpy(entry->ipDefaultRouterAddress.address,
               address, entry->ipDefaultRouterAddress.addr_len);
        entry->ipDefaultRouterAddressType = addresstype;
        entry->ipDefaultRouterIfIndex = if_index;
        entry->ipDefaultRouterLifetime = lifetime;
        entry->ipDefaultRouterPreference = preference;
    }
    return 0;
}

void update_ip_def_router(ipDefaultRouterEntry_list_t * stats)
{
    TRACE_ENTRY();
    RESET_LIST(stats);
    netlink_process(RTM_GETROUTE, process_route, stats);
    SORT_LIST(stats);
    TRACE_EXIT();
}

#define IPV6_ADDR_LOOPBACK      0x0010U
#define IPV6_ADDR_LINKLOCAL     0x0020U
#define IPV6_ADDR_SITELOCAL     0x0040U

void update_ip6_scope_zone_index(ipv6ScopeZoneIndexEntry_list_t * stats)
{
    FILE *file;
    const char *filename = "/proc/net/if_inet6";
    char line[256], addr[256];
    int if_index, pfx_len, scope, flags;

    TRACE_ENTRY();
    RESET_LIST(stats);

    if (!(file = fopen(filename, "r"))) {
        error("%s - Failed fopen(\"%s\") : \"%s\"", __FUNCTION__, filename,
              strerror(errno));
        return;
    }
    while (fgets(line, sizeof(line), file)) {
        int ix = 0;
        ipv6ScopeZoneIndexEntry_t *entry;
        int skip_line = 0;

        if (sscanf
            (line, "%39s %04x %02x %02x %02x\n", addr, &if_index, &pfx_len,
             &scope, &flags) != 5) {
            error("%s - Unexpected line format \"%s\" in \"%s\"", __FUNCTION__,
                  line, filename);
            continue;
        }
        if (!(scope & IPV6_ADDR_LINKLOCAL)) {
            continue;
        }
        FOR_ENTRIES(stats, entry, ix) {
            if (entry->ipv6ScopeZoneIndexIfIndex == if_index) {
                skip_line = 1;
                break;
            }
        }
        if (skip_line)
            continue;

        entry = NEW_ENTRY(stats);
        memset(entry, 0, sizeof(*entry));
        entry->ipv6ScopeZoneIndexIfIndex = if_index;
        entry->ipv6ScopeZoneIndexLinkLocal = if_index;
    }
    fclose(file);

    SORT_LIST(stats);
    TRACE_EXIT();
}

/**
 * Copy in_addr or in6_addr to inet_address_t.
 */
static void copy_inet_address(int af, inet_address_t * addr, void *src)
{
    if (af == AF_INET) {
        memcpy(addr->address, &((struct in_addr *) src)->s_addr,
               addr->addr_len = sizeof(in_addr_t));
    } else {
        memcpy(addr->address, ((struct in6_addr *) src)->s6_addr,
               addr->addr_len = 16);
    }
}

static int process_neigh(struct nlmsghdr *n, void *arg)
{
    ip_net_phys_list_t *stats = arg;
    struct ndmsg *r = NLMSG_DATA(n);
    ip_net_phys_stats_t *entry;
    struct rtattr *rtattr_p = RTM_RTA(r);
    int rt_cnt = n->nlmsg_len - NLMSG_LENGTH(sizeof(*r));
    int af;

    if (r->ndm_family == AF_INET)
        af = INET(ipv4);
    else if (r->ndm_family == AF_INET6)
        af = INET(ipv6);
    else
        return -1;
    entry = NEW_ENTRY(stats);
    entry->if_index = r->ndm_ifindex;
    entry->addr_type = af;
    while (RTA_OK(rtattr_p, rt_cnt)) {
        switch (rtattr_p->rta_type) {
        case NDA_DST:
            copy_inet_address(r->ndm_family, &entry->net_addr,
                              RTA_DATA(rtattr_p));
            break;
        case NDA_LLADDR:
            memcpy(entry->hw_addr, RTA_DATA(rtattr_p), RTA_PAYLOAD(rtattr_p));
            entry->hw_addr_len = RTA_PAYLOAD(rtattr_p);
            break;
        default:
            break;
        }
        rtattr_p = RTA_NEXT(rtattr_p, rt_cnt);
    }

    if (r->ndm_flags & NTF_ROUTER)
        entry->ipv6router = 1;
    else
        entry->ipv6router = 0;

    entry->state = IP_ELEM(reachable);    // reasonable default
    struct state_mask
    {
        int mask, value;
    } *sptr;
    static struct state_mask states[] = {
        {NUD_INCOMPLETE, IP_ELEM(incomplete)},
        {NUD_STALE, IP_ELEM(stale)},
        {NUD_DELAY, IP_ELEM(delay)},
        {NUD_PROBE, IP_ELEM(probe)},
        {NUD_FAILED, IP_ELEM(ps_invalid)},
        {NUD_NONE, IP_ELEM(ps_unknown)},
        // REACHABLE, NOARP and PERMANENT translate to the default
        {0}
    };
    for (sptr = states; sptr->mask != 0; sptr++)
        if ((sptr->mask & r->ndm_state) != 0) {
            entry->state = sptr->value;
            break;
        }
    if (entry->state == IP_ELEM(ps_invalid))
        entry->hw_addr_len = 0;
    entry->type = (r->ndm_state & NUD_NOARP)
        || (r->ndm_state & NUD_PERMANENT) ? IP_ELEM(static) : IP_ELEM(dynamic);

    return 0;
}

void update_net_phys_list(ip_net_phys_list_t * stats)
{
    TRACE_ENTRY();
    RESET_LIST(stats);
    if (netlink_process(RTM_GETNEIGH, process_neigh, stats) < 0) {
        error("NETLINK process terminated\n");
        return;
    }

    SORT_LIST(stats);
    TRACE_EXIT();
}

int compare_net_phys_entries(const ip_net_phys_stats_t * s1,
                             const ip_net_phys_stats_t * s2)
{
    return
        UINT_CMP(s1->if_index, s2->if_index) ? :
        (s1->addr_type - s2->addr_type ? :
         (INET_ADDR_CMP(s1->net_addr, s2->net_addr)));
}

int compare_ipv6_router_advert_stats(const ipv6RouterAdvertEntry_t * s1,
                                     const ipv6RouterAdvertEntry_t * s2)
{
    return UINT_CMP(s1->ipv6RouterAdvertIfIndex, s2->ipv6RouterAdvertIfIndex);
}

static int get_if_index_by_name(const char *name, uint32_t * index)
{
    int sock;
    int cc;
    struct ifreq if_req;

    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
        error("\n%s: Failed to obtain socket(AF_INET, SOCK_STREAM, 0) : \"%s\"",
              __FUNCTION__, strerror(errno));
        return -1;
    }
    memset(&if_req, 0, sizeof(if_req));
    strcpy(if_req.ifr_name, name);
    if (ioctl(sock, SIOCGIFINDEX, (char *) &if_req) >= 0) {
        cc = 0;
        *index = if_req.ifr_ifindex;
    } else {
        error("\n%s: Failed ioctl(SIOCGIFINDEX) : \"%s\"", __FUNCTION__,
              strerror(errno));
        cc = -1;
    }
    close(sock);
    return cc;
}


static int get_address_flag_info(address_flag_info_t *addr,
                                 int index, int family)
{
    struct
    {
        struct nlmsghdr n;
        struct ifaddrmsg r;
        char buf[1024];
    } req;
    struct rtattr *rta;
    int status;
    char buf[16384];
    struct nlmsghdr *nlmp;
    struct ifaddrmsg *rtmp;
    struct rtattr *rtatp;
    int rtattrlen;
    int sd;

    memset(addr, 0, sizeof(*addr));
    sd = socket(PF_NETLINK, SOCK_DGRAM, NETLINK_ROUTE);
    if (sd < 0) {
        error
            ("\n%s - Failed socket(AF_NETLINK, SOCK_DGRAM, "
             "NETLINK_ROUTE) : err \"%s\"",
             __FUNCTION__, strerror(errno));
        return CONFD_ERR;
    }
    memset(&req, 0, sizeof(req));
    req.n.nlmsg_len = NLMSG_LENGTH(sizeof(struct ifaddrmsg));
    req.n.nlmsg_flags = NLM_F_REQUEST | NLM_F_ROOT;
    req.n.nlmsg_type = RTM_GETADDR;
    req.r.ifa_family = family;
    rta = (struct rtattr *) (((char *) &req) + NLMSG_ALIGN(req.n.nlmsg_len));
    if (family == AF_INET) {
        rta->rta_len = RTA_LENGTH(4);
    } else {
        rta->rta_len = RTA_LENGTH(16);
    }
    status = send(sd, &req, req.n.nlmsg_len, 0);
    if (status < 0) {
        error("\n%s - Failed send(netlink request) : err \"%s\"", __FUNCTION__,
              strerror(errno));
        close(sd);
        return CONFD_ERR;
    }
    status = recv(sd, buf, sizeof(buf), 0);
    if (status < 0) {
        error("\n%s - Failed recv(netlink request) : err \"%s\"", __FUNCTION__,
              strerror(errno));
        close(sd);
        return CONFD_ERR;
    }
    if (status == 0) {
        error("\n%s - recv(netlink request) returned zero", __FUNCTION__);
        close(sd);
        return CONFD_ERR;
    }
    for (nlmp = (struct nlmsghdr *) buf; status > sizeof(*nlmp);) {
        int len = nlmp->nlmsg_len;
        int req_len = len - sizeof(*nlmp);

        if (req_len < 0 || len > status) {
            error("\n%s - Invalid netlink message", __FUNCTION__);
            close(sd);
            return CONFD_ERR;
        }
        if (!NLMSG_OK(nlmp, status)) {
            error("\n%s - Invalid NLMSG message", __FUNCTION__);
            close(sd);
            return CONFD_ERR;
        }
        rtmp = (struct ifaddrmsg *) NLMSG_DATA(nlmp);
        rtatp = (struct rtattr *) IFA_RTA(rtmp);
        rtattrlen = IFA_PAYLOAD(nlmp);
        if (index == rtmp->ifa_index) {
            for (; RTA_OK(rtatp, rtattrlen);
                 rtatp = RTA_NEXT(rtatp, rtattrlen)) {
                if (rtatp->rta_type == IFA_BROADCAST) {
                    addr->inp = *(struct in_addr *) RTA_DATA(rtatp);
                    addr->bcastflg = 1;
                }
                if (rtatp->rta_type == IFA_ANYCAST) {
                    addr->inp = *(struct in_addr *) RTA_DATA(rtatp);
                    addr->anycastflg = 1;
                }
            }
        }
        status -= NLMSG_ALIGN(len);
        nlmp = (struct nlmsghdr *) ((char *) nlmp + NLMSG_ALIGN(len));
    }
    close(sd);
    return CONFD_OK;
}

static int get_ioctl_interfaces(int sd, struct ifconf *ifc)
{
    int lastlen = 0, i;
    struct ifconf ifc_tmp;

    if (ifc == NULL) {
        memset(&ifc_tmp, 0x0, sizeof(ifc_tmp));
        ifc = &ifc_tmp;
    }
    for (i = 8;; i *= 2) {
        ifc->ifc_buf = malloc(i * sizeof(struct ifreq));
        if (ifc->ifc_buf == NULL) {
            error("\n%s - Failed malloc(%d)", __FUNCTION__,
                  i * sizeof(struct ifreq));
            return -1;
        }
        ifc->ifc_len = i * sizeof(struct ifreq);

        if (ioctl(sd, SIOCGIFCONF, (char *) ifc) < 0) {
            if (errno != EINVAL || lastlen != 0) {
                error("\n%s - Failed ioctl(,,,, SIOCGIFCONF, ...) : %s",
                      __FUNCTION__, strerror(errno));
                free(ifc->ifc_buf);
                ifc->ifc_buf = NULL;
                break;
            }
        } else {
            if (ifc->ifc_len == lastlen) {
                break;
            }
            lastlen = ifc->ifc_len;
        }
        free(ifc->ifc_buf);
    }

    if (ifc == &ifc_tmp) {
        free(ifc_tmp.ifc_buf);
    }
    return ifc->ifc_len / sizeof(struct ifreq);
}

static void fill_prefix_oid(ipAddressEntry_t * entry)
{
    static uint32_t *dst, oid_base[] = { 1, 3, 6, 1, 2, 1, 4, 32, 1, 5 };
    inet_address_t addr = entry->ipAddressAddr;
    int i;

    dst =
        memcpy(entry->ipAddressPrefix.oid, oid_base,
               sizeof(oid_base)) + sizeof(oid_base);
    *(dst++) = entry->ipAddressIfIndex;
    *(dst++) = entry->ipAddressAddrType;
    *(dst++) = addr.addr_len;
    mask_prefix(&addr, entry->ipPrefixLen);
    for (i = 0; i < addr.addr_len; i++) {
        // converting uchar to uint32
        *(dst++) = addr.address[i];
    }
    *(dst++) = entry->ipPrefixLen;
    entry->ipAddressPrefix.len = dst - entry->ipAddressPrefix.oid;
}

static void update_ipv6_address(ipAddressEntry_list_t * stats)
{
    int i, cnt;
    ipAddressEntry_t *entry;
    list_t addr_list;
    if_inet6_line_t *line;
    uint32_t index;
    address_flag_info_t addr_info;
    int is_zero;
    char *pos;

    if ((load_if_inet6_line_list(&addr_list) < 0)
        || (list_size(&addr_list) < 1)) {
        free_list(&addr_list);
        return;
    }

    for (i = 0; i < list_size(&addr_list); i++) {
        line = get_list_item(&addr_list, i);
        entry = (ipAddressEntry_t *) NEW_ENTRY(stats);
        memset(entry, 0, sizeof(*entry));

        entry->ipAddressAddrType = INET(ipv6);
        pos = line->m_addr;
        cnt = get_byte_field_hex(&pos, entry->ipAddressAddr.address, ADDRLEN);

        if (cnt != 32) {
            error("\n%s - Unexpected Address digit count %d (expected 32)",
                  __FUNCTION__, cnt);
        }

        if (get_if_index_by_name(line->m_name, &index) < 0) {
            index = line->m_idx;
        }
        entry->ipAddressIfIndex = index;
        entry->ipPrefixLen = line->m_prefix_len;
        fill_prefix_oid(entry);
        memset(&addr_info, 0, sizeof(addr_info));
        get_address_flag_info(&addr_info, index, AF_INET6);

        is_zero = 0;
        pos = line->m_addr;
        get_ip_addr_hex_h(&pos, &entry->ipAddressAddr,
                          &(entry->ipAddressAddrType), &is_zero);
        if (entry->ipAddressAddrType != INET(ipv6)) {
            error("\n%s - Unexpected AddressType %d (expected %d)",
                  __FUNCTION__, entry->ipAddressAddrType, INET(ipv6));
            entry->ipAddressAddrType = INET(ipv6);
        }

        if ((line->m_flags & IFA_F_PERMANENT) || (!line->m_flags)) {
            entry->ipAddressStatus = IPADDRESSSTATUSTC_PREFERRED;
        } else if (line->m_flags & IFA_F_TEMPORARY) {
            entry->ipAddressStatus = IPADDRESSSTATUSTC_PREFERRED;
        } else if (line->m_flags & IFA_F_DEPRECATED) {
            entry->ipAddressStatus = IPADDRESSSTATUSTC_DEPRECATED;
        } else if (line->m_flags & IFA_F_TENTATIVE) {
            entry->ipAddressStatus = IPADDRESSSTATUSTC_TENTATIVE;
        } else {
            entry->ipAddressStatus = IPADDRESSSTATUSTC_UNKNOWN;
        }
        entry->ipAddressType =
            addr_info.
            anycastflg ? IPADDRESSTYPE_ANYCAST : IPADDRESSTYPE_UNICAST;
        if (!line->m_flags) {
            entry->ipAddressOrigin = IPADDRESSORIGINTC_LINKLAYER;
        } else if (line->m_flags & IFA_F_TEMPORARY) {
            entry->ipAddressOrigin = IPADDRESSORIGINTC_RANDOM;
        } else if (IN6_IS_ADDR_LINKLOCAL(entry->ipAddressAddr.address)) {
            entry->ipAddressOrigin = IPADDRESSORIGINTC_LINKLAYER;
        } else {
            entry->ipAddressOrigin = IPADDRESSORIGINTC_MANUAL;
        }

        if (entry->ipAddressOrigin == IPADDRESSORIGINTC_LINKLAYER) {
            entry->ipAddressStorageType = STORAGETYPE_PERMANENT;
        } else {
            entry->ipAddressStorageType = STORAGETYPE_VOLATILE;
        }
    }

    free_list(&addr_list);
}

#define IS_APIPA(a)  (((in_addr_t)(a << 16)) == 0xFEA90000)

static void update_ipv4_address(ipAddressEntry_list_t * stats)
{
    int sd, i;
    uint32_t index;
    int interfaces = 0;
    struct ifconf ifc;
    struct ifreq *ifrp;
    ipAddressEntry_t *entry = NULL;
    ipAddressEntry_t *bcast_entry = NULL;
    char if_name[256];
    struct sockaddr save_addr;
    struct sockaddr_in *si;
    in_addr_t ipval;
    address_flag_info_t addr_info;


    if ((sd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        error("\n%s - Failed socket(AF_INET, SOCK_DGRAM, 0) : err \"%s\"",
              __FUNCTION__, strerror(errno));
        return;
    }
    interfaces = get_ioctl_interfaces(sd, &ifc);
    if (interfaces < 0) {
        close(sd);
        return;
    }

    ifrp = ifc.ifc_req;

    for (i = 0; i < interfaces; i++, ifrp++) {
        RESERVE_ENTRIES(stats, 2);
          // hack to prevent table reallocation in the middle
        entry = (ipAddressEntry_t *) NEW_ENTRY(stats);
        memset(entry, 0, sizeof(*entry));

        entry->ipAddressAddrType = INET(ipv4);
        strcpy(if_name, ifrp->ifr_name);
        save_addr = ifrp->ifr_addr;
        si = (struct sockaddr_in *) &ifrp->ifr_addr;

        ipval = si->sin_addr.s_addr;
        memcpy(entry->ipAddressAddr.address, &si->sin_addr.s_addr,
               entry->ipAddressAddr.addr_len = sizeof(in_addr_t));

        if (get_if_index_by_name(if_name, &index) < 0) {
            // TODO
        } else {
            entry->ipAddressIfIndex = index;
        }
        memset(&addr_info, 0, sizeof(addr_info));
        get_address_flag_info(&addr_info, index, AF_INET);
        if (addr_info.bcastflg) {
            bcast_entry = NEW_ENTRY(stats);
            memset(bcast_entry, 0, sizeof(*entry));

            bcast_entry->ipAddressAddrType = INET(ipv4);

            bcast_entry->ipAddressIfIndex = entry->ipAddressIfIndex;
            memcpy(bcast_entry->ipAddressPrefix.oid, (uint32_t[]) {
                   0, 0}
                   , 2);
            bcast_entry->ipAddressPrefix.len = 2;
            memcpy(bcast_entry->ipAddressAddr.address, &addr_info.inp.s_addr,
                   bcast_entry->ipAddressAddr.addr_len = sizeof(in_addr_t));
        }
//      Prefix Len
        ifrp->ifr_addr = save_addr;
        if (ioctl(sd, SIOCGIFNETMASK, ifrp) < 0) {
//          error("\n%s - Failed ioctl(..., SIOCGIFNETMASK, ...) :
//                   err \"%s\"", __FUNCTION__, strerror(errno));
//            continue; // ???
        } else {

            si = (struct sockaddr_in *) &ifrp->ifr_addr;
            entry->ipPrefixLen =
                ipaddress_ipv4_prefix_len(ntohl(si->sin_addr.s_addr));
            if (bcast_entry) {
                bcast_entry->ipPrefixLen = entry->ipPrefixLen;
            }
        }
        fill_prefix_oid(entry);
        // get flags
        ifrp->ifr_addr = save_addr;
        if (ioctl(sd, SIOCGIFFLAGS, ifrp) < 0) {
//            error ...
//            continue;
        } else {
            entry->ipFlags = ifrp->ifr_flags;
            // ?? bcast_entry
        }
        entry->ipAddressStorageType = STORAGETYPE_VOLATILE;
        if (bcast_entry) {
            bcast_entry->ipAddressType = IPADDRESSTYPE_BROADCAST;
            bcast_entry->ipAddressStorageType = STORAGETYPE_VOLATILE;
        }
        if (addr_info.anycastflg) {
            entry->ipAddressType = IPADDRESSTYPE_ANYCAST;
        } else {
            entry->ipAddressType = IPADDRESSTYPE_UNICAST;
        }

        entry->ipAddressStatus = IPADDRESSSTATUSTC_PREFERRED;
        if (bcast_entry) {
            bcast_entry->ipAddressStatus = IPADDRESSSTATUSTC_PREFERRED;
        }

        if (IS_APIPA(ipval)) {
            entry->ipAddressOrigin = IPADDRESSORIGINTC_RANDOM;
            if (bcast_entry) {
                bcast_entry->ipAddressOrigin = IPADDRESSORIGINTC_RANDOM;
            }
        } else {
            entry->ipAddressOrigin = IPADDRESSORIGINTC_MANUAL;
            if (bcast_entry) {
                bcast_entry->ipAddressOrigin = IPADDRESSORIGINTC_MANUAL;
            }
        }
    }
    free(ifc.ifc_buf);
    close(sd);
}

void update_ipv4_icmp_stats(icmpStatsEntry_list_t * stats)
{
    const char *file = "/proc/net/snmp";
    const char *line_name = "Icmp:";
    list_t list;
    icmpStatsEntry_t *entry = NULL;

    init_list(&list, sizeof(counter_t), 256);

    if (load_cnt_list_by_columns(file, &list, line_name) < 0) {
        error("\n%s - Failed to parse \"%s\" (IPv4 icmpStatsEntry)",
              __FUNCTION__, file);
        free_list(&list);
        return;
    }
    entry = NEW_ENTRY(stats);
    memset(entry, 0, sizeof(*entry));
    entry->icmpStatsIPVersion = INET(ipv4);

    CHECK_GET_COUNTER(&list, "InMsgs", &entry->icmpStatsInMsgs, file);
    CHECK_GET_COUNTER(&list, "InErrors", &entry->icmpStatsInErrors, file);
    CHECK_GET_COUNTER(&list, "OutMsgs", &entry->icmpStatsOutMsgs, file);
    CHECK_GET_COUNTER(&list, "OutErrors", &entry->icmpStatsOutErrors, file);
    free_list(&list);
}

void update_ipv6_icmp_stats(icmpStatsEntry_list_t * stats)
{
    const char *file = "/proc/net/snmp6";
    list_t list;
    icmpStatsEntry_t *entry = NULL;    // = NEW_ENTRY(stats);

    init_list(&list, sizeof(counter_t), 256);

    if (load_cnt_list_by_lines(file, &list) < 0) {
        error("\n%s - Failed to parse \"%s\" (IPv6 icmpStatsEntry)",
              __FUNCTION__, file);
        free_list(&list);
        return;
    }
    entry = NEW_ENTRY(stats);
    memset(entry, 0, sizeof(*entry));
    entry->icmpStatsIPVersion = INET(ipv6);

    CHECK_GET_COUNTER(&list, "Icmp6InMsgs", &entry->icmpStatsInMsgs, file);
    CHECK_GET_COUNTER(&list, "Icmp6InErrors", &entry->icmpStatsInErrors, file);
    CHECK_GET_COUNTER(&list, "Icmp6OutMsgs", &entry->icmpStatsOutMsgs, file);
    if (get_counter_by_name(&list, "Icmp6OutErrors",
                            &entry->icmpStatsOutErrors) < 0) {
        // not available on all hosts
        //error("\n%s - Failed to get counter \"Icmp6OutErrors\"
        // " from \"%s\"", __FUNCTION__, file);
    }
    free_list(&list);
}


void update_ipv6_icmp_msg_stats(icmpMsgStatsEntry_list_t * stats)
{
    int i, j;
    const char *file = "/proc/net/snmp6";
    list_t list;

    init_list(&list, sizeof(counter_t), 256);
    if (load_cnt_list_by_lines(file, &list) < 0) {
        error("\n%s - Failed to parse \"%s\" (IPv6 icmpMsgStatsEntry)",
              __FUNCTION__, file);
        free_list(&list);
        return;
    }
    for (i = 0; i < list_size(&list); i++) {
        int out, type;
        counter_t *counter = get_list_item(&list, i);

        if ((out = sscanf(counter->m_name, "Icmp6OutType%d", &type))
            || sscanf(counter->m_name, "Icmp6InType%d", &type)) {
            icmpMsgStatsEntry_t *entry = NULL;

            FOR_ENTRIES(stats, entry, j) {
                if ((entry->icmpMsgStatsIPVersion == INET(ipv6))
                    && (entry->icmpMsgStatsType == type)) {
                    break;
                } else {
                    entry = NULL;
                }
            }
            if (!entry) {
                entry = NEW_ENTRY(stats);
                memset(entry, 0, sizeof(*entry));
                entry->icmpMsgStatsIPVersion = INET(ipv6);
                entry->icmpMsgStatsType = type;
            }
            if (out) {
                entry->icmpMsgStatsOutPkts = counter->m_value;
            } else {
                entry->icmpMsgStatsInPkts = counter->m_value;
            }
        }
    }
    free_list(&list);
}

void update_ipv4_icmp_msg_stats(icmpMsgStatsEntry_list_t * stats)
{
    int i, j;
    const char *file = "/proc/net/snmp";
    const char *line_name = "IcmpMsg:";
    list_t list;

    init_list(&list, sizeof(counter_t), 256);
    if (load_cnt_list_by_columns(file, &list, line_name) < 0) {
        error("\n%s - Failed to parse \"%s\" (IPv4 icmpMsgStatsEntry)",
              __FUNCTION__, file);
        free_list(&list);
        return;
    }
    for (i = 0; i < list_size(&list); i++) {
        int out, type;
        counter_t *counter = get_list_item(&list, i);

        if ((out = sscanf(counter->m_name, "OutType%d", &type))
            || sscanf(counter->m_name, "InType%d", &type)) {
            icmpMsgStatsEntry_t *entry = NULL;

            FOR_ENTRIES(stats, entry, j) {
                if ((entry->icmpMsgStatsIPVersion == INET(ipv4))
                    && (entry->icmpMsgStatsType == type)) {
                    break;
                } else {
                    entry = NULL;
                }
            }
            if (!entry) {
                entry = NEW_ENTRY(stats);
                memset(entry, 0, sizeof(*entry));
                entry->icmpMsgStatsIPVersion = INET(ipv4);
                entry->icmpMsgStatsType = type;
            }
            if (out) {
                entry->icmpMsgStatsOutPkts = counter->m_value;
            } else {
                entry->icmpMsgStatsInPkts = counter->m_value;
            }
        }
    }
    free_list(&list);
}

int compare_ip_icmp_stats(const icmpStatsEntry_t * s1,
                          const icmpStatsEntry_t * s2)
{
    return s1->icmpStatsIPVersion - s2->icmpStatsIPVersion;
}
int compare_ip_icmp_msg_stats(const icmpMsgStatsEntry_t * s1,
                              const icmpMsgStatsEntry_t * s2)
{
    return s1->icmpMsgStatsIPVersion - s2->icmpMsgStatsIPVersion ? :
        s1->icmpMsgStatsType - s2->icmpMsgStatsType;
}

int compare_ip_address_prefix(const ipAddressPrefixEntry_t * s1,
                              const ipAddressPrefixEntry_t * s2)
{
    return
        UINT_CMP(s1->ipAddressPrefixIfIndex, s2->ipAddressPrefixIfIndex) ? :
        (s1->ipAddressPrefixType - s2->ipAddressPrefixType ? :
         (INET_ADDR_CMP(s1->ipAddressPrefixPrefix,
                        s2->ipAddressPrefixPrefix) ? :
          (s1->ipAddressPrefixLength - s2->ipAddressPrefixLength)));
}

void update_ip_address(ipAddressEntry_list_t * stats)
{
    TRACE_ENTRY();
    RESET_LIST(stats);
    update_ipv4_address(stats);
    update_ipv6_address(stats);
    SORT_LIST(stats);
    TRACE_EXIT();
}

void update_ip_icmp_stats(icmpStatsEntry_list_t * stats)
{
    TRACE_ENTRY();
    RESET_LIST(stats);
    update_ipv4_icmp_stats(stats);
    update_ipv6_icmp_stats(stats);
    SORT_LIST(stats);
    TRACE_EXIT();
}


void update_ip_icmp_msg_stats(icmpMsgStatsEntry_list_t * stats)
{
    TRACE_ENTRY();
    RESET_LIST(stats);
    update_ipv4_icmp_msg_stats(stats);
    update_ipv6_icmp_msg_stats(stats);
    SORT_LIST(stats);
    TRACE_EXIT();
}

void update_ip_address_prefix(ipAddressPrefixEntry_list_t * stats)
{
    int i;
    list_t list;

    TRACE_ENTRY();
    RESET_LIST(stats);
    init_list(&list, sizeof(netlink_addr_info_t), 64);

    if (get_netlink_addr_info(&list) < 0) {
        error
            ("\n%s - Failed to get ipAddressPrefixEntry list by "
             "means of NETLINK",
             __FUNCTION__);
        return;
    }
    for (i = 0; i < list_size(&list); i++) {
        netlink_addr_info_t *info = get_list_item(&list, i);
        ipAddressPrefixEntry_t *entry = NEW_ENTRY(stats);

        entry->ipAddressPrefixIfIndex = info->m_iface.ifa_index;
        entry->ipAddressPrefixLength = info->m_iface.ifa_prefixlen;
        if (info->m_iface.ifa_family == AF_INET) {
            entry->ipAddressPrefixType = INET(ipv4);
            if (info->m_flags & eIFA_ADDRESS) {
                memcpy(entry->ipAddressPrefixPrefix.address,
                       &info->m_addr.m_addr4.s_addr,
                       entry->ipAddressPrefixPrefix.addr_len =
                       sizeof(in_addr_t));
                if (decide_address_prefix_origin
                    (&info->m_addr.m_addr4, sizeof(info->m_addr.m_addr4),
                     info->m_iface.ifa_flags, NULL,
                     &entry->ipAddressPrefixOrigin) < 0) {
                    error("\n%s - Failed to decide ipv4 prefix origin",
                          __FUNCTION__);
                }
            }
        } else if (info->m_iface.ifa_family == AF_INET6) {
            entry->ipAddressPrefixType = INET(ipv6);
            if (info->m_flags & eIFA_ADDRESS) {
                memcpy(entry->ipAddressPrefixPrefix.address,
                       &info->m_addr.m_addr6.s6_addr,
                       entry->ipAddressPrefixPrefix.addr_len =
                       sizeof(info->m_addr.m_addr6.s6_addr));
                if (decide_address_prefix_origin
                    (&info->m_addr.m_addr6, sizeof(info->m_addr.m_addr6),
                     info->m_iface.ifa_flags, NULL,
                     &entry->ipAddressPrefixOrigin) < 0) {
                    error("\n%s - Failed to decide ipv6 prefix origin",
                          __FUNCTION__);
                }
            }
        }
        mask_prefix(&entry->ipAddressPrefixPrefix,
                    entry->ipAddressPrefixLength);

        if (info->m_flags & eIFA_CACHEINFO) {
            entry->ipAddressPrefixAdvPreferredLifetime =
                info->m_cacheinfo.ifa_prefered;    //usec
            entry->ipAddressPrefixAdvValidLifetime =
                info->m_cacheinfo.ifa_valid;    //usec
        }
        if (!entry->ipAddressPrefixOrigin) {
            entry->ipAddressPrefixOrigin = 1;    //Other
        }
        entry->ipAddressPrefixOnLinkFlag = 1;    //True TODO
        entry->ipAddressPrefixAutonomousFlag = 2;    //False TODO

    }
    free_list(&list);
    SORT_LIST(stats);
    FILTER_LIST(stats);
    TRACE_EXIT();
}


void update_ipv6_router_advert_stats(ipv6RouterAdvertEntry_list_t * stats)
{
    int sd, i;
    u_int32_t index;
    int32_t value;
    int if_cnt = 0;
    struct ifconf ifc;
    struct ifreq *ifrp;
    char path_name[1024];
    ipv6RouterAdvertEntry_t *entry = NULL;
    char if_name[256];

    int forwarding_all = 1;

    TRACE_ENTRY();
    if ((sd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        error("\n%s - Failed socket(AF_INET, SOCK_DGRAM, 0) : err \"%s\"",
              __FUNCTION__, strerror(errno));
        return;
    }
    if_cnt = get_ioctl_interfaces(sd, &ifc);
    if (if_cnt < 0) {
        close(sd);
        return;
    }
    close(sd);

    RESET_LIST(stats);

    if (get_int32_file_cfg("/proc/sys/net/ipv6/conf/all/forwarding", &value) <
        0) {
        error("\n%s - Failed to get forwarding all status", __FUNCTION__);
        forwarding_all = 1;
    } else {
        forwarding_all = value;
    }

    ifrp = ifc.ifc_req;
    for (i = 0; i < if_cnt; i++, ifrp++) {
        entry = (ipv6RouterAdvertEntry_t *) NEW_ENTRY(stats);
        strcpy(if_name, ifrp->ifr_name);

        if (get_if_index_by_name(if_name, &index) < 0) {
            error("\n%s - Failed to get ifIndex for \"%s\"", __FUNCTION__,
                  if_name);
        } else {
            entry->ipv6RouterAdvertIfIndex = index;
        }

        snprintf(path_name, sizeof(path_name),
                 "/proc/sys/net/ipv6/conf/%s/forwarding", if_name);
        if (get_int32_file_cfg(path_name, &value) < 0) {
            error("\n%s - Failed to get forvarding status for \"%s\"",
                  __FUNCTION__, if_name);
            value = 1;
        }
        entry->ipv6RouterAdvertSendAdverts = forwarding_all && value ? 1 : 2;

        snprintf(path_name, sizeof(path_name), "/proc/sys/net/ipv6/conf/%s/mtu",
                 if_name);
        if (get_int32_file_cfg(path_name, &value) < 0) {
            error("\n%s - Failed to get ipv6RouterAdvertLinkMTU for \"%s\"",
                  __FUNCTION__, if_name);
        } else {
            entry->ipv6RouterAdvertLinkMTU = value;
        }

        snprintf(path_name, sizeof(path_name),
                 "/proc/sys/net/ipv6/conf/%s/hop_limit", if_name);
        if (get_int32_file_cfg(path_name, &value) < 0) {
            error("\n%s - Failed to get ipv6RouterAdvertCurHopLimit for \"%s\"",
                  __FUNCTION__, if_name);
        } else {
            entry->ipv6RouterAdvertCurHopLimit = value;
        }

        snprintf(path_name, sizeof(path_name),
                 "/proc/sys/net/ipv6/neigh/%s/retrans_time_ms", if_name);
        if (get_int32_file_cfg(path_name, &value) < 0) {
            snprintf(path_name, sizeof(path_name),
                     "/proc/sys/net/ipv6/neigh/%s/retrans_time", if_name);
            if (get_int32_file_cfg(path_name, &value) < 0) {
                error
                    ("\n%s - Failed to get ipv6RouterAdvertRetransmitTime "
                     "for \"%s\"",
                     __FUNCTION__, if_name);
            } else {
                entry->ipv6RouterAdvertRetransmitTime = value * 1000;
            }
        } else {
            entry->ipv6RouterAdvertRetransmitTime = value;
        }

        snprintf(path_name, sizeof(path_name),
                 "/proc/sys/net/ipv6/neigh/%s/base_reachable_time_ms", if_name);
        if (get_int32_file_cfg(path_name, &value) < 0) {
            snprintf(path_name, sizeof(path_name),
                     "/proc/sys/net/ipv6/neigh/%s/base_reachable_time",
                     if_name);
            if (get_int32_file_cfg(path_name, &value) < 0) {
                error
                    ("\n%s - Failed to get ipv6RouterAdvertReachableTime "
                     "for \"%s\"",
                     __FUNCTION__, if_name);
            } else {
                entry->ipv6RouterAdvertReachableTime = value * 1000;
            }
        } else {
            entry->ipv6RouterAdvertReachableTime = value;
        }

        // TODO: using default values here
        entry->ipv6RouterAdvertMaxInterval = 600;
        entry->ipv6RouterAdvertMinInterval = 200;
        entry->ipv6RouterAdvertManagedFlag = 2;
        entry->ipv6RouterAdvertOtherConfigFlag = 2;
        entry->ipv6RouterAdvertDefaultLifetime = 0;
        entry->ipv6RouterAdvertRowStatus = 1;
    }

    free(ifc.ifc_buf);

    SORT_LIST(stats);
    TRACE_EXIT();
}
