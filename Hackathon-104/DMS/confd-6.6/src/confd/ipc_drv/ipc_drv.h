#ifndef _IPC_DRV_H
#define _IPC_DRV_H 1

#include <sys/types.h>
#include <sys/socket.h>

/* if getaddrinfo() and socket() are implemented,
   connect() and bind() are optional - and vice versa */
struct confd_ext_ipc_cbs {
    int (*getaddrinfo)(char *address,
                       int *family, int *type, int *protocol,
                       struct sockaddr **addr, socklen_t *addrlen,
                       char **errstr);
    int (*socket)(int family, int type, int protocol, char **errstr);
    int (*getpeeraddr)(int fd, char **address, char **errstr); /* optional */
    int (*connect)(char *address, char **errstr);
    int (*bind)(char *address, char **errstr);
    void (*unbind)(int fd);                                    /* optional */
};

typedef struct confd_ext_ipc_cbs *confd_ext_ipc_init_func_t(void);
extern confd_ext_ipc_init_func_t confd_ext_ipc_init;


/* for backward compatibility */

struct confd_ipc_cbs {
    int (*connect)(char *address);
    int (*bind)(char *address);
    void (*unbind)(int fd);     /* optional */
    char *errstr;
};

typedef struct confd_ipc_cbs *confd_ipc_init_func_t(void);
extern confd_ipc_init_func_t confd_ipc_init;

#endif

