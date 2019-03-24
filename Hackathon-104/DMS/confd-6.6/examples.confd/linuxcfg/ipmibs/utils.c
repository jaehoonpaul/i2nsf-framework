#include <ctype.h>
#include <dirent.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include <confd.h>
#include "linuxcfg_api.h"

#include "utils.h"

void find_process_ids(inode_pid_pair_t * pairs, int size)
{
    char name[1024];
    DIR *dir;
    struct dirent *d;
    int pid = 0;
    int cnt = 0;
    unsigned inode;
    int i;

    if ((dir = opendir("/proc")) == NULL)
        return;

    while ((d = readdir(dir)) != NULL && cnt < size) {
        DIR *dir1;
        struct dirent *d1;
        int pos;
        char crap;

        if (sscanf(d->d_name, "%d%c", &pid, &crap) != 1)
            continue;

        pos = sprintf(name, "/proc/%d/fd/", pid);
        if ((dir1 = opendir(name)) == NULL)
            continue;

        while ((d1 = readdir(dir1)) != NULL && cnt < size) {
            int fd, n;
            char lnk[64];

            if (sscanf(d1->d_name, "%d", &fd) != 1)
                continue;

            sprintf(name + pos, "%d", fd);
            n = readlink(name, lnk, sizeof(lnk) - 1);
            if (n + 1 < sizeof(lnk)) {
                lnk[n] = 0;
                if (sscanf(lnk, "socket:[%u]", &inode) != 1)
                    continue;
                for (i = 0; i < size; i++) {
                    if (pairs[i].inode == inode) {
                        cnt++;
                        pairs[i].process_id = pid;
                        break;
                    }
                }
            }
        }
        closedir(dir1);
    }
    closedir(dir);
}

/**
 * Some address strings contain addresses in network byte order, some
 * do not. If conv_hton is true, the address is assumed in host byte
 * order and converted to network byte order.
 */
ssize_t _scanaddr(inet_address_t * addr, const char *src, uint32_t type,
                  int conv_hton)
{
    struct in_addr addr4;
    struct in6_addr addr6;
    void *ptr;
    int len;

    if (type == INET(ipv4)) {
        sscanf(src, "%x", &addr4.s_addr);
        if (conv_hton)
            addr4.s_addr = ntohl(addr4.s_addr);
        ptr = &addr4.s_addr;
        len = sizeof(addr4.s_addr);
    } else {
        sscanf(src, "%8x%8x%8x%8x", addr6.s6_addr32,
               addr6.s6_addr32 + 1, addr6.s6_addr32 + 2,
               addr6.s6_addr32 + 3);
        if (conv_hton) {
            int i;

            for (i = 0; i < 4; i++)
                addr6.s6_addr32[i] = ntohl(addr6.s6_addr32[i]);
        }
        ptr = &addr6.s6_addr32;
        len = sizeof(addr6.s6_addr32);
    }
    if (len <= ADDRLEN) {
        memcpy(addr->address, ptr, len);
        addr->addr_len = len;
        return len;
    } else
        return -1;
}

ssize_t scanaddr_nbo(inet_address_t * addr, const char *src, uint32_t type)
{
    return _scanaddr(addr, src, type, 1);
}

ssize_t scanaddr(inet_address_t * addr, const char *src, uint32_t type)
{
    return _scanaddr(addr, src, type, 0);
}

ssize_t scan_addr_port(inet_address_t * addr, uint32_t * port, const char *src,
                       uint32_t type)
{
    ssize_t len = 0;
    char *p = strchr(src, ':');

    if (p != NULL && (len = scanaddr(addr, src, type)) >= 0)
        sscanf(p + 1, "%x", port);
    return len;
}

char *get_net_snmp_line(char *buf, size_t len, const char *proto)
{
    FILE *fp;
    char pproto[256], *pptr = NULL;
    int plen;

    plen = sprintf(pproto, "%s:", proto);
    if ((fp = fopen("/proc/net/snmp", "r")) == NULL) {
        error("could not open /proc/net/snmp: %s", strerror(errno));
        return NULL;
    }
    while (fgets(buf, len, fp) != NULL) {
        if (strncmp(buf, pproto, plen) == 0) {
            if (fgets(buf, len, fp) != NULL &&
                strncmp(buf, pproto, plen) == 0) {
                pptr = buf + plen + 1;
                break;
            }
        }
    }
    fclose(fp);
    return pptr;
}

int get_ioctl_socket()
{
    static int fd = -1;

    if (fd < 0)
        fd = socket(AF_INET, SOCK_STREAM, 0);
    return fd;
}

static int netlink_sd = -1;
static uint8_t rcvbuf[32768];
static uint8_t sndbuf[512];

int get_netlink_sd()
{
    if (netlink_sd >= 0)
        return netlink_sd;
    int sd = socket(AF_NETLINK, SOCK_DGRAM, NETLINK_ROUTE);

    if (sd < 0) {
        error("\n%s - Failed socket(AF_NETLINK, SOCK_DGRAM, NETLINK_ROUTE): "
              "err \"%s\"\n", __FUNCTION__, strerror(errno));
        return -1;
    }
    int rcvbuf_size = sizeof(rcvbuf);

    if (setsockopt(sd, SOL_SOCKET, SO_RCVBUF, &rcvbuf_size, sizeof(rcvbuf_size))
        < 0) {
        error("\n%s - Failed setsockopt(... , SOL_SOCKET, SO_RCVBUF, ...) : "
              "err \"%s\"\n", __FUNCTION__, strerror(errno));
        return -1;
    }
    return (netlink_sd = sd);
}

int netlink_process(int nlmsg_type,
                    int (*process) (struct nlmsghdr * nlmsghdr_p, void *arg),
                    void *arg)
{
    struct sockaddr_nl addr;
    struct nlmsghdr *nl_hdr;
    struct rtmsg *rt_hdr;
    int nlmsg_done = 0;
    int sd = get_netlink_sd();

    memset(&addr, 0, sizeof(addr));
    addr.nl_family = AF_NETLINK;

    memset(sndbuf, 0, sizeof(sndbuf));
    nl_hdr = (struct nlmsghdr *) sndbuf;
    nl_hdr->nlmsg_type = nlmsg_type;
    nl_hdr->nlmsg_pid = getpid();
    nl_hdr->nlmsg_seq = 0;
    nl_hdr->nlmsg_flags = NLM_F_ROOT | NLM_F_MATCH | NLM_F_REQUEST;
    nl_hdr->nlmsg_len = NLMSG_LENGTH(sizeof(struct rtmsg));
    rt_hdr = (struct rtmsg *) NLMSG_DATA(nl_hdr);
    rt_hdr->rtm_table = RT_TABLE_MAIN;

    if (sendto
        (sd, sndbuf, nl_hdr->nlmsg_len, 0, (struct sockaddr *) &addr,
         sizeof(struct sockaddr_nl)) < 0) {
        error("\n%s - Failed sendto() : err \"%s\"\n", __FUNCTION__,
              strerror(errno));
        return -1;
    }

    do {
        int rcv_cnt;
        struct nlmsghdr *nlmsghdr_p;
        struct rtmsg *rtmsg_p;
        socklen_t sock_len;

        memset(rcvbuf, 0, sizeof(rcvbuf));
        sock_len = sizeof(struct sockaddr_nl);

        rcv_cnt =
            recvfrom(sd, rcvbuf, sizeof(rcvbuf), 0, (struct sockaddr *) &addr,
                     &sock_len);
        if (rcv_cnt < 0) {
            error("\n%s - Failed recvfrom() : err \"%s\"\n", __FUNCTION__,
                  strerror(errno));
            break;
        }

        nlmsghdr_p = (struct nlmsghdr *) rcvbuf;
        while (NLMSG_OK(nlmsghdr_p, rcv_cnt)) {
            if (nlmsghdr_p->nlmsg_type == NLMSG_ERROR) {
                error("\n%s - netling message error\n", __FUNCTION__);
                return -1;
            }
            if (nlmsghdr_p->nlmsg_type & NLMSG_DONE) {
                nlmsg_done = 1;
                break;
            }
            rtmsg_p = NLMSG_DATA(nlmsghdr_p);
            if (rtmsg_p->rtm_dst_len != 0) {
                nlmsghdr_p = NLMSG_NEXT(nlmsghdr_p, rcv_cnt);
                continue;
            }
            process(nlmsghdr_p, arg);
            nlmsghdr_p = NLMSG_NEXT(nlmsghdr_p, rcv_cnt);
        }                        // while NLMSG_OK
    } while (!nlmsg_done);

    return 0;
}

// Parsing utilities

int get_uint_dec(char **pos, unsigned int *value)
{
    char chr = **pos;
    uint64_t val;

    if (!chr)
        return -1;
    while (!isdigit((unsigned char) chr)) {
        (*pos)++;
        chr = **pos;
        if (!chr)
            return -1;
    }
    val = (uint64_t) chr - '0';
    (*pos)++;
    chr = **pos;
    while (isdigit((unsigned char) chr)) {
        val *= 10;
        val += (uint64_t) chr - '0';
        (*pos)++;
        chr = **pos;
    }
    *value = val;
    return 0;
}
int get_uint64_dec(char **pos, uint64_t * value)
{
    char chr = **pos;
    uint64_t val;

    *value = 0;
    if (!chr)
        return -1;
    while (!isdigit((unsigned char) chr)) {
        (*pos)++;
        chr = **pos;
        if (!chr)
            return -1;
    }
    val = (uint64_t) chr - '0';
    (*pos)++;
    chr = **pos;
    while (isdigit((unsigned char) chr)) {
        val *= 10;
        val += (uint64_t) chr - '0';
        (*pos)++;
        chr = **pos;
    }
    *value = val;
    return 0;
}
int get_name(char **pos, char *name)
{
    int i = 0;
    char chr = **pos;

    name[0] = '\0';
    if (!chr)
        return -1;
    while (chr && !isalnum((unsigned char) chr) && (chr != '_')) {
        (*pos)++;
        chr = **pos;
    }
    if (!chr)
        return -1;
    name[i++] = chr;
    (*pos)++;
    chr = **pos;
    while (isalnum((unsigned char) chr) || (chr == '_')) {
        name[i++] = chr;
        (*pos)++;
        chr = **pos;
    }
    name[i] = '\0';
    return 0;
}

int get_uint_hex(char **pos, unsigned int *value)
{
    char chr = **pos;

    if (!chr)
        return -1;
    while (!isxdigit((unsigned char) chr)) {
        if (!chr)
            return -1;
        (*pos)++;
        chr = **pos;
        if (!chr)
            return -1;
    }
    *value = hex_val(chr);
    (*pos)++;
    chr = **pos;
    while (isxdigit((unsigned char) chr)) {
        *value *= 16;
        *value += hex_val(chr);
        (*pos)++;
        chr = **pos;
    }
    return 0;
}
int get_byte_field_hex(char **pos, uint8_t * field, int max_size)
{
    char chr = **pos;
    uint8_t *field_pos = field;
    int i = 0;

    if (!chr)
        return -1;
    while (!isxdigit((unsigned char) chr)) {
        if (!chr)
            return -1;
        (*pos)++;
        chr = **pos;
        if (!chr)
            return 0;
    }
    while (isxdigit((unsigned char) chr)) {
        if (i & 1) {
            *field_pos |= hex_val(chr);
            field_pos += 1;
        } else {
            if (field_pos >= (field + max_size)) {
                return -1;
            }
            *field_pos = hex_val(chr) << 4;
        }
        i++;
        (*pos)++;
        chr = **pos;
    }
    if (i & 1) {
        while (field_pos >= field) {
            *field_pos >>= 4;
            if (field_pos == field) {
                break;
            }
            *field_pos |= *(field_pos - 1) << 4;
            field_pos -= 1;
        }
    }
    return i;
}

int get_int_dec(char **pos, int *value)
{
    char chr = **pos;
    char chr_last = 0;

    if (!chr)
        return -1;
    while (!isdigit((unsigned char) chr)) {
        (*pos)++;
        chr_last = chr;
        chr = **pos;
        if (!chr)
            return -1;
    }
    *value = chr - '0';
    (*pos)++;
    chr = **pos;
    while (isdigit((unsigned char) chr)) {
        *value *= 10;
        *value += chr - '0';
        (*pos)++;
        chr = **pos;
    }
    if (chr_last == '-') {
        *value *= -1;
    }
    return 0;
}

int _get_ip_addr_hex(char **pos, inet_address_t * addr, int *addr_type,
                     int *is_zero, int reorder)
{
    char str[128];
    char chr;
    int i = 0;

    if (!pos || !addr)
        return -1;
    if (!(chr = **pos))
        return -1;
    memset(str, 0, sizeof(str));
    addr->addr_len = 0;
    *is_zero = 1;
    char *cc = NULL;

    while (!isxdigit((unsigned char) chr)) {
        (*pos)++;
        if (!(chr = **pos))
            return -1;
    }
    while (isxdigit((unsigned char) chr)) {
        if (i < 127)
            str[i++] = chr;
        if (chr != '0')
            *is_zero = 0;
        (*pos)++;
        chr = **pos;
    }
    if (strlen(str) > 8) {
        struct in6_addr src;

        *addr_type = INET(ipv6);
        sscanf(str, "%8x%8x%8x%8x", src.s6_addr32,
               src.s6_addr32 + 1, src.s6_addr32 + 2,
               src.s6_addr32 + 3);
        if (reorder) {
            src.s6_addr32[0] = ntohl(src.s6_addr32[0]);
            src.s6_addr32[1] = ntohl(src.s6_addr32[1]);
            src.s6_addr32[2] = ntohl(src.s6_addr32[2]);
            src.s6_addr32[3] = ntohl(src.s6_addr32[3]);
        }
        addr->addr_len = sizeof(src.s6_addr32);
        cc = memcpy(addr->address, src.s6_addr32, addr->addr_len);
    } else {
        struct in_addr src;

        *addr_type = INET(ipv4);
        sscanf(str, "%X", &src.s_addr);
        addr->addr_len = sizeof(src.s_addr);
        cc = memcpy(addr->address, &src.s_addr, addr->addr_len);
    }
    if (!cc) {
        error("\n%s - Failed sinet_ntop(...); : err \"%s\"", __FUNCTION__,
              strerror(errno));
    }
    return 0;
}

int get_ip_addr_hex(char **pos, inet_address_t * addr, int *addr_type,
                    int *is_zero)
{
    return _get_ip_addr_hex(pos, addr, addr_type, is_zero, 0);
}

int get_ip_addr_hex_h(char **pos, inet_address_t * addr, int *addr_type,
                      int *is_zero)
{
    return _get_ip_addr_hex(pos, addr, addr_type, is_zero, 1);
}

int empty_line(char *line)
{
    int i;

    if (!line)
        return 1;
    for (i = 0; i < strlen(line); i++) {
        if (!isspace(line[i]))
            return 0;
    }
    return 1;
}

int hex_val(char chr)
{
    int val = chr - '0';

    if ((val >= 0) && (val < 10))
        return val;
    val = chr - 'A';
    if ((val >= 0) && (val < 6))
        return val + 10;
    val = chr - 'a';
    if ((val >= 0) && (val < 6))
        return val + 10;
    return -1;
}
