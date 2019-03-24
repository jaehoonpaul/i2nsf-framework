#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
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

/* Housekeeping to be able to unlink the file on unbind() */

struct sockname {
    struct sockname *next;
    int fd;
    char name[1];
};
static struct sockname *socknames;

static void save_name(int fd, char *name)
{
    struct sockname *new;

    if ((new = malloc(sizeof(struct sockname) + strlen(name))) != NULL) {
        new->next = socknames;
        new->fd = fd;
        strcpy(new->name, name);
        socknames = new;
    }
}

static void del_name(int fd)
{
    struct sockname **p = &socknames, *np;

    while (*p != NULL) {
        if ((*p)->fd == fd) {
            np = *p;
            *p = (*p)->next;
            unlink(np->name);
            free(np);
            return;
        }
        p = &((*p)->next);
    }
}

/* Actual confd_ext_ipc_cbs callbacks */

/*
 * bind() - create a socket and bind it to the given address
 * - return socket fd ready for listen()+accept(), or -1 on error
 */
static int unix_ipc_bind(char *address, char **errstr)
{
    int s;
    int on = 1;
    struct sockaddr_un un;

    if ((s = socket(PF_UNIX, SOCK_STREAM, 0)) >= 0) {
        setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));
        memset(&un, '\0', sizeof(un));
        un.sun_family = AF_UNIX;
        snprintf(un.sun_path, sizeof(un.sun_path), "%s", address);
        unlink(un.sun_path);
        if (bind(s, (struct sockaddr *)&un, sizeof(un)) == 0) {
            save_name(s, un.sun_path);
            return s;
        }
        close(s);
        return ret_err(errstr, "bind to %s: %s",
                       un.sun_path, strerror(errno));
    }
    return ret_err(errstr, "socket: %s", strerror(errno));
}

/*
 * unbind() - close the socket fd and do any other required cleanup
 * optional callback, set confd_ipc_cbs unbind elem to NULL if not needed
 * - ConfD will close the fd in that case
 */
static void unix_ipc_unbind(int fd)
{
    close(fd);
    del_name(fd);
}

/*
 * connect() - create a socket and connect it to the given address
 * - return socket fd, or -1 on error
 */
static int unix_ipc_connect(char *address, char **errstr)
{
    int s;
    struct sockaddr_un un;

    if ((s = socket(PF_UNIX, SOCK_STREAM, 0)) >= 0) {
        memset(&un, '\0', sizeof(un));
        un.sun_family = AF_UNIX;
        snprintf(un.sun_path, sizeof(un.sun_path), "%s", address);
        if (connect(s, (struct sockaddr *)&un, sizeof(un)) == 0) {
            return s;
        }
        close(s);
        return ret_err(errstr, "connect to %s: %s",
                       un.sun_path, strerror(errno));
    }
    return ret_err(errstr, "socket: %s", strerror(errno));
}


static struct confd_ext_ipc_cbs unix_ipc_cbs;

/*
 * confd_ext_ipc_init() - return pointer to callback structure
 * must be external, i.e. not 'static'
 */
struct confd_ext_ipc_cbs *confd_ext_ipc_init(void)
{
    memset(&unix_ipc_cbs, '\0', sizeof(unix_ipc_cbs));
    unix_ipc_cbs.connect = unix_ipc_connect;
    unix_ipc_cbs.bind = unix_ipc_bind;
    unix_ipc_cbs.unbind = unix_ipc_unbind;
    return &unix_ipc_cbs;
}
