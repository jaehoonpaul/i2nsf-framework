#ifndef _UTILS_H_
#define _UTILS_H_

#include <stdint.h>
#include <sys/socket.h>

#include <linux/netlink.h>
#include <linux/rtnetlink.h>


#include "INET-ADDRESS-MIB.h"
#define INET(el) _NSCONCAT(INET_ADDRESS_MIB, el)
#define ADDRLEN 256                // InetAddress length
typedef struct
{
    int inode;
    int process_id;
} inode_pid_pair_t;

#define UPDATE_COUNTER_PAIR(new, ctr32, ctr64) do {     \
        long diff = new - ctr32;                        \
        ctr64 += diff >= 0 ? diff : (1ll << 32) + diff; \
        ctr32 = new;                                    \
    } while(0)


/**
 * Get the value from given file. Only int32 supported (for now).
 */
#define GET_FILEENTRY(fld, file) do {            \
        FILE *fd;                                \
        if ((fd = fopen(file, "r")) == NULL) {   \
            error("Could not open %s", file);    \
            fld = 0;                             \
        } else {                                 \
            if (fscanf(fd, "%i", &fld) == 0)     \
                warn("Failed to match number."); \
            fclose(fd);                          \
        }                                        \
    } while(0)

#define GET_PAR_FILEENTRY(fld, buf, fmt, ...) do {    \
        (void) sprintf(buf, fmt, ##__VA_ARGS__);       \
        GET_FILEENTRY(fld, buf);                    \
    } while(0)

/**
 * For the list of inode numbers, fill in appropriate process ids. If
 * a process owning an inode cannot be found, the process_id remains
 * unchanged.
 *
 * @param pairs list of inode/process id pairs to be completed
 * @param size length of the list
 */
void find_process_ids(inode_pid_pair_t * pairs, int size);

typedef union
{
    struct in_addr addr4;
    struct in6_addr addr6;
} any_addr_t;

typedef struct
{
    u_int8_t address[ADDRLEN];
    int addr_len;
} inet_address_t;

#define ADDR_CPY(dst, src) memcpy((dst).address,                        \
                                  (src).address,                        \
                                  ((dst).addr_len = (src).addr_len))

/**
 * Comparison of (unsigned) integers.
 */
#define UINT_CMP(v1, v2) ((v1) < (v2) ? -1 : ((v1) == (v2) ? 0 :1))

/**
 * String comparison as required by SNMP. In SNMP, table entries must
 * be ordered lexicographically, but dynamic-length types (such as
 * string) include their length in their representation, therefore the
 * comparison must first compare the length.
 */
#define SNMPSTRCMP(f1, f2) (strlen(f1) - strlen(f2) ? : strcmp(f1, f2))


/**
 * InetAddr comparison as required by SNMP. In SNMP, table entries must
 * be ordered lexicographically, but dynamic-length types (such as
 * string) include their length in their representation, therefore the
 * comparison must first compare the length.
 */
#define INET_ADDR_CMP(f1, f2) (f1.addr_len - f2.addr_len ? :            \
                               memcmp(f1.address, f2.address, f1.addr_len))


/**
 * Fetch a byte representation of an IP address from the supplied
 * string containing its numerical representation. Only IPv4 and IPv6
 * address types are supported.
 *
 * @param dst the output value
 * @param dst_size
 * @param src a string with numeric representation of the address;
 *        see inet_ntop for format
 * @param type ConfD/INET-ADDRESS representation of the address
 *        family identification
 * @return -1, if something fails, length of the copied part
 */
ssize_t scanaddr(inet_address_t * addr, const char *src, uint32_t type);

/**
 * Same as scanaddr, but the address string is expected in network byte order.
 */
ssize_t scanaddr_nbo(inet_address_t * addr, const char *src, uint32_t type);

/**
 * Same as scanaddr, plus scans for the port number as well. Assumes
 * string in the form addr:port.
 */
ssize_t scan_addr_port(inet_address_t * addr, uint32_t * port, const char *src,
                       uint32_t type);

/**
 * Get proto's entry line in /proc/net/snmp.
 * @param buf on successful return contains whole protocol line
 * @param len length of buf
 * @param proto protocol name (Udp/Tcp/Ip/...)
 *
 * @return NULL if line not found, pointer into buf where actual stats
 * begin otherwise
 */
char *get_net_snmp_line(char *buf, size_t len, const char *proto);

/**
 * Get socket to be used for ioctl calls (for AF_INET operations only).
 */
int get_ioctl_socket();

/**
 * Send netlink message of given type, process replies.
 */
int netlink_process(int nlmsg_type,
                    int (*process) (struct nlmsghdr * nlmsghdr_p,
                                    void *arg),
                    void *arg);

// Parsing utilities

int get_uint_dec(char **pos, unsigned int *value);
int get_uint64_dec(char **pos, uint64_t * value);
int get_name(char **pos, char *name);
int get_uint_hex(char **pos, unsigned int *value);
int get_byte_field_hex(char **pos, uint8_t * field, int max_size);
int get_int_dec(char **pos, int *value);
int get_ip_addr_hex(char **pos, inet_address_t * addr, int *addr_type,
                    int *is_zero);
int get_ip_addr_hex_h(char **pos, inet_address_t * addr, int *addr_type,
                      int *is_zero);
int hex_val(char chr);
int empty_line(char *line);

#endif
