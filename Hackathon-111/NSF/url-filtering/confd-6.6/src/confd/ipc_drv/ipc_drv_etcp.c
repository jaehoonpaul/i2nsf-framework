#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include "ipc_drv.h"

static int ret_err(char **errstr, const char *fmt, ...)
{
    char buf[256];
    va_list args;

    va_start(args, fmt);
    vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);
    *errstr = strdup(buf);
    return -1;
}

/* confd_ext_ipc_cbs callbacks */

/*
 * getaddrinfo() - parse address, creating params for socket() and
 * bind(2)/connect(2)
 * - return 0, or -1 on error
 */
/* string form "1.2.3.4:4565", "0.0.0.0", "[fc80::12:34]:4565", "[::]", ... */
static int etcp_getaddrinfo(char *address,
                            int *family, int *type, int *protocol,
                            struct sockaddr **addr, socklen_t *addrlen,
                            char **errstr)
{
    char *p;
    struct sockaddr_in in_addr;
    struct sockaddr_in6 in6_addr;

    if (address[0] == '[' && (p = strchr(&address[1], ']')) != NULL) {
        /* IPv6 */
        *p++ = '\0';
        memset(&in6_addr, 0, sizeof(in6_addr));
        if (inet_pton(AF_INET6, &address[1], &in6_addr.sin6_addr) == 1) {
            in6_addr.sin6_family = AF_INET6;
            if (*p != '\0')
                in6_addr.sin6_port = htons(atoi(p));
            else
                in6_addr.sin6_port = 0;
            *family = PF_INET6;
            *type = SOCK_STREAM;
            *protocol = 0;
            *addrlen = sizeof(in6_addr);
            *addr = malloc(*addrlen);
            memcpy(*addr, &in6_addr, *addrlen);
            return 0;
        }
    } else {
        /* IPv4 */
        if ((p = strchr(address, ':')) != NULL)
            *p++ = '\0';
        memset(&in_addr, 0, sizeof(in_addr));
        if (inet_pton(AF_INET, address, &in_addr.sin_addr) == 1) {
            in_addr.sin_family = AF_INET;
            if (p != NULL)
                in_addr.sin_port = htons(atoi(p));
            else
                in_addr.sin_port = 0;
            *family = PF_INET;
            *type = SOCK_STREAM;
            *protocol = 0;
            *addrlen = sizeof(in_addr);
            *addr = (struct sockaddr *)malloc(*addrlen);
            memcpy(*addr, &in_addr, *addrlen);
            return 0;
        }
    }
    return ret_err(errstr, "Invalid address");
}

/*
 * socket() - create a socket
 * - return socket fd, or -1 on error
 */
static int etcp_socket(int family, int type, int protocol, char **errstr)
{
    int s;

    if ((s = socket(family, type, protocol)) < 0)
        return ret_err(errstr, "socket: %s", strerror(errno));
    // setsockopt(s, SOL_SOCKET, SO_VRF, &vrf, sizeof(vrf));
    return s;
}

/*
 * getpeeraddr() - get peer address for socket fd in string form
 * - return 0, or -1 on error
 */
static int etcp_getpeeraddr(int fd, char **address, char **errstr)
{
    struct sockaddr_storage ss_addr;
    socklen_t addrlen = sizeof(ss_addr);
    char buf[INET6_ADDRSTRLEN];

    if (getpeername(fd, (struct sockaddr*)&ss_addr, &addrlen) == 0) {
        switch (ss_addr.ss_family) {
        case AF_INET:
            inet_ntop(AF_INET, &((struct sockaddr_in *)&ss_addr)->sin_addr,
                      buf, sizeof(buf));
            break;
        case AF_INET6:
            inet_ntop(AF_INET6, &((struct sockaddr_in6 *)&ss_addr)->sin6_addr,
                      buf, sizeof(buf));
            break;
        default:
            return ret_err(errstr, "Unknown address family %d",
                           ss_addr.ss_family);
        }
        *address = strdup(buf);
        return 0;
    }
    return ret_err(errstr, "getpeername: %s", strerror(errno));
}


static struct confd_ext_ipc_cbs etcp_ext_ipc_cbs;

/*
 * confd_ext_ipc_init() - return pointer to callback structure
 * must be external, i.e. not 'static'
 */
struct confd_ext_ipc_cbs *confd_ext_ipc_init(void)
{
    memset(&etcp_ext_ipc_cbs, '\0', sizeof(etcp_ext_ipc_cbs));
    etcp_ext_ipc_cbs.getaddrinfo = etcp_getaddrinfo;
    etcp_ext_ipc_cbs.socket = etcp_socket;
    etcp_ext_ipc_cbs.getpeeraddr = etcp_getpeeraddr;
    return &etcp_ext_ipc_cbs;
}
