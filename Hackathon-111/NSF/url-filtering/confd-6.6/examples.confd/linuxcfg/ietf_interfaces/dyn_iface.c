#include <errno.h>
#include <poll.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <linux/netlink.h>
#include <linux/rtnetlink.h>
#include <net/if.h>
#include <sys/types.h>
#include <sys/socket.h>

#include <confd.h>
#include <confd_maapi.h>
#include <confd_cdb.h>

#include "linuxcfg_api.h"
#include "linuxcfg_util.h"

#include "dyn_iface.h"

/* Some maximum buffer sizes */
#define NLBUFSIZE   (8*1024)

static void pending_ifaces_init(void);
static char *iface_indextoname(unsigned int index, char *name);

/* Interface state list definitions and helper functions */
struct ifstate {
    struct ifstate     *next;
    unsigned int        if_index;
    char                if_name[IFNAMELEN];
};

static struct ifstate *new_ifstate(int ifidx);
static struct ifstate *find_ifstate_i(unsigned int if_index);
static int del_ifstate_i(int ifidx);
static void update_if(int ifidx, int event, int flags);

static struct ifstate *configured_ifs = NULL;


/* Subscription list */
struct subscriber {
    struct subscriber *next;
    dyn_callback *cb;
    int mask;
};
static struct subscriber *configured_subs = NULL;

int dyn_iface_subscribe(dyn_callback *cb, int mask) {
    struct subscriber *new_sub = malloc(sizeof(struct subscriber));
    struct ifstate    *ifp;

    new_sub->cb     = cb;
    new_sub->mask = mask;

    new_sub->next   = configured_subs;
    configured_subs = new_sub;

    /* Call for every element in the list */
    for (ifp = configured_ifs; ifp != NULL; ifp = ifp->next) {
        cb(ifp->if_index, ifp->if_name, DYN_IF_CREATE, 0);
    }

    return CONFD_OK;
}

void dyn_iface_init(void) {
    struct if_nameindex *if_ni, *i;
    int e;

    /* Create a thread for handling pending interfaces */
    pending_ifaces_init();

    /* Gather the initial list of interfaces */
    if (NULL == (if_ni = if_nameindex())) {
        e = errno;
        error("%s(): if_nameindex() failed: %s (%d)",
              __FUNCTION__, strerror(e), e);
    }

    for (i = if_ni; i->if_index != 0; i++) {
        update_if(i->if_index, DYN_IF_CREATE, 0);
    }

    if_freenameindex(if_ni);

}

/* Update a specific interface in the if list, and notify
 * subscribers. */
static void update_if(int ifidx, int event, int flags) {
    struct ifstate *p;
    struct subscriber *subp;

    p = find_ifstate_i(ifidx);

    if (p == NULL) {
        if (event == DYN_IF_DELETE)
            warn("strange: deleting non-existing interface: %d.", ifidx);
        else
            p = new_ifstate(ifidx);
    } else {
        /* If we get the create event on an existing interface it is
         * really a status update */
        if (event == DYN_IF_CREATE)
            event = DYN_IF_STATUS;
    }

    LOG("event: 0x%X, ifidx: %d, ifname: %s, flags: 0x%X",
        event, ifidx, p ? p->if_name : "unknown", flags);

    /* Iterate through the subscriber list. */
    for (subp = configured_subs; subp != NULL; subp = subp->next) {
        if (subp->mask & event) {
            subp->cb(ifidx,p ? p->if_name : "unknown", event, flags);
        }
    }

    /* Delete if the event is DELETE */
    if (event == DYN_IF_DELETE) {
        del_ifstate_i(ifidx);
    }

}

/* Handles the reception of new messages on the netlink socket */
static void nl_handler(int fd, int revents) {
    int e;
    int len;
    struct ifinfomsg* ifi = NULL;
    char ifname[IFNAMSIZ] = "";
    char nlbuf[NLBUFSIZE];
    struct nlmsghdr *nlh;
    static int count = 0;

    nlh = (struct nlmsghdr *) nlbuf;

    /* Main message reception handler */
    len = recv(fd, nlh, NLBUFSIZE, 0);
    if (len >= 0)
    {
        if (0 == len) {
            LOG("recv() returned 0 (count=%d)", count);

            if (count > 10) {
                fail("%s(): Too many unexpected 0 return values "
                      "from recv(), terminating.",
                      __FUNCTION__);

            }
            usleep(500);
            count++;
            return;
        }

        count = 0;

        for (; (NLMSG_OK (nlh, len))
                 && (nlh->nlmsg_type != NLMSG_DONE);
             nlh = NLMSG_NEXT(nlh, len)) {

            LOG("Got NLMSG: nlh->nlmsg_len=%d, "
                 "nlh->nlmsg_type=%d, nlh->nlmsg_flags=0x%.2x",
                 nlh->nlmsg_len,
                 nlh->nlmsg_type, nlh->nlmsg_flags);

            ifi = (struct ifinfomsg *) NLMSG_DATA (nlh);

            if (NULL == ifi) {
                warn("%s(): ifi == NULL! This is strange, "
                     "skipping this NLMSG.", __FUNCTION__);
                continue;
            }

            LOG("IFI: ifi_type=0x%.4x, ifi_index=0x%.2x, "
                 "ifi_flags=0x%.4x, ifi_change=0x%.4x",
                 ifi->ifi_type, ifi->ifi_index,
                 ifi->ifi_flags, ifi->ifi_change);

            iface_indextoname(ifi->ifi_index, ifname);

            LOG("Interface %s (ifi_index=0x%.2x).",
                ifname, ifi->ifi_index);

            switch (nlh->nlmsg_type) {

                case RTM_NEWLINK:
                    update_if(ifi->ifi_index, DYN_IF_CREATE, ifi->ifi_flags);
                    continue;

                case RTM_DELLINK:
                    update_if(ifi->ifi_index, DYN_IF_DELETE, ifi->ifi_flags);
                    continue;

                default:
                    // Other type of announcement, ignoring.
                    info("Ignoring this NLMSG.");
                    continue;
            }
        }
    } else { /* If recv fails */
        /* Fell out from the recv() loop */
        e = errno;
        error("%s(): recv() failed: %s (%d)",
              __FUNCTION__, strerror(e), e);

        fail("%s() failed", __FUNCTION__);
    }
}

static void pending_ifaces_init(void)
{
    TRACE_ENTRY();

    struct sockaddr_nl addr;
    int e;
    int nls;

    /* Create a netlink(3) socket. */
    if (-1 == (nls = socket(PF_NETLINK, SOCK_RAW, NETLINK_ROUTE))) {
        e = errno;
        fail("%s(): socket() failed: %s (%d)",
              __FUNCTION__, strerror(e), e);
    }

    memset(&addr, 0, sizeof(addr));
    addr.nl_family = AF_NETLINK;
    addr.nl_groups = RTMGRP_LINK;

    if (-1 == bind(nls, (struct sockaddr *)&addr, sizeof(addr))) {
        e = errno;
        fail("%s(): bind() failed: %s (%d)", __FUNCTION__, strerror(e), e);
    }

    register_fd_handler(nls, POLLIN, nl_handler);
}

static char *iface_indextoname(unsigned int index, char *name)
{

    struct if_nameindex *if_ni, *i;
    int found = 0;
    int e;

    if (NULL == (if_ni = if_nameindex())) {
        e = errno;
        error("%s(): if_nameindex() failed: %s (%d)",
              __FUNCTION__, strerror(e), e);
        return NULL;
    }

    for (i = if_ni; i->if_index != 0; i++) {
        if (i->if_index == index) {
            LOG("index=0x%.2x iface=%s", i->if_index, i->if_name);
            XSTRNCPY(name, i->if_name, IFNAMSIZ);
            found = 1;
            break;
        }
    }

    if_freenameindex(if_ni);

    if (found)
        return name;
    else
        return NULL;
}

static struct ifstate *find_ifstate_i(unsigned int idx)
{
    struct ifstate *p;

    for (p = configured_ifs; p != NULL; p = p->next) {
        if (p->if_index == idx)
            return p;
    }
    return NULL;
}

static struct ifstate *new_ifstate(int ifidx)
{
    char ifname[IFNAMELEN] = "";
    TRACE_ENTRY("ifidx=%d", ifidx);

    struct ifstate **p;

    for (p = &configured_ifs; *p != NULL; p = &((*p)->next))
        ;

    *p = xmalloc(sizeof(struct ifstate));
    memset(*p, 0, sizeof(struct ifstate));

    (*p)->if_index = ifidx;

    if (iface_indextoname(ifidx, ifname) == NULL)
        warn("Couldn't convert ifidx %d to name.", ifidx);

    XSTRNCPY((*p)->if_name, ifname, IFNAMELEN);

    return *p;
}

static int del_ifstate_i(int ifidx)
{
    TRACE_ENTRY("ifidx=%d", ifidx);
    int rv;
    struct ifstate *p;
    struct ifstate **np;

    /* Default: Not found, not deleted */
    rv = 1;

    for (p = configured_ifs, np = &configured_ifs; p != NULL; ) {
                        if (p->if_index == ifidx) {
            *np = p->next;
            free(p);
            rv = 0;
            break;
        } else {
            np = &p->next;
            p = p->next;
        }
    }

    TRACE_EXIT("rv=", rv);
    return rv;
}
