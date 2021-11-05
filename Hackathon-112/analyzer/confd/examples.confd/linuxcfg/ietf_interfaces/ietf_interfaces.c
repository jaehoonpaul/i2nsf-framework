/*
 * Copyright 2014 Tail-F Systems AB
 * Tail-F customers are permitted to redistribute in binary form, with
 * or without modification, for use in customer products.
 */

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/sysinfo.h>
#include <sys/types.h>
#include <sys/utsname.h>

#include <confd.h>
#include <confd_maapi.h>
#include <confd_cdb.h>

#include "ietf-interfaces.h"
#include "iana-if-type.h"

#include "linuxcfg_api.h"
#include "linuxcfg_util.h"

#include "dyn_iface.h"
#include "caches.h"

/* ipmibs functions */
#include "ipmibs.h"
#include "if.h"

#include "ietf_interfaces_api.h"

static void init(void);
static void setup(struct confd_daemon_ctx *dctx, int rsock);
static void handle_update(int rsock, int ssock, int spoint);

static int if_get_elem(struct confd_trans_ctx *tctx,
                       confd_hkeypath_t *kp);
static int if_get_next(struct confd_trans_ctx *tctx,
                       confd_hkeypath_t *kp,
                       long next);

/* CLI completion for interface names */
static int if_complete(struct confd_user_info *uinfo,
                       int cli_style, char *token, int completion_char,
                       confd_hkeypath_t *kp,
                       char *cmdpath, char *cmdparam_id,
                       struct confd_qname *simpleType, char *extra);

/* Action points */
static struct confd_action_cbs actionpoints[] = {
    {.actionpoint = "comp_if_name",  .completion = if_complete },
    {.actionpoint = ""}
};

static struct confd_data_cbs transpoints[] = {
    { .callpoint = "if_state_dp", .get_elem = if_get_elem,
      .get_next = if_get_next },
    { .callpoint = "" }
};


static struct external_cmd ip_cmd = {
    .descr_name  = "ip",
    .cmd         = NULL,
    .args        = NULL,
    .env_var     = "IP_CMD",
#ifdef USER_IP_CMD
    .cc_macro    = USER_IP_CMD,
#else
    .cc_macro    = NULL,
#endif
    .binary_name = "ip",
    .paths       = { "/usr/sbin/ip", "/sbin/ip", NULL }
};

#define IP_CMD       GET_EXT_CMD(ip_cmd)

/* Interface list */
struct ifinfo {
    /* Shared info */
    struct ifinfo *next;
    char *ifname;

    /* Info from CDB */
    int   iftype;
    int   adminState;
    int   exist_cdb;

    /* Info from dyn_iface */
    int   exist_iflist;
};

static struct ifinfo *iflist = NULL;

/* iflist manipulation functions */
static void iflist_update_cdb(char *ifname, int enabled, int iftype);
static void iflist_remove_cdb(char *ifname);
static void iflist_update_if(char *ifname);
static void iflist_remove_if(char *ifname);
static void set_ifstate(char *ifname, int state);
static int  iflist_len();
static struct ifinfo *iflist_find(char *ifname);

/* Interface state subscribers */
struct ifsubs {
    struct ifsubs *next;
    if_state_handler handler;
};
static struct ifsubs *ifsublist = NULL;

/* Subscribers to tags under /interfaces */
struct if_subscriber {
    struct if_subscriber *next;
    int                   tag;
    iter_handler          handler;
};
static struct if_subscriber *ifs_root = NULL;

/* This is the subscription point for interfaces/. */
static int spoint = 0;

/* The component declaration that provides the hooks for linuxcfg */
const struct component ietf_interfaces = {
    init,         /* init */
    NULL,         /* setup0 */
    setup,        /* setup */
    NULL,         /* valpoints */
    transpoints,  /* transpoints */
    actionpoints, /* actionpoints */
    NULL,         /* init_validation */
    NULL,         /* stop_validation */
    NULL,         /* init_data */
    NULL,         /* finish_data */
};

/* Triggered whenever something happens to an actual interface on the
   system. This is called through dyn_iface. */
static void
if_callback(int ifindex, char *ifname, int change, int flags) {
    LOG("ifindex: %d, ifname: %s, change: %d", ifindex, ifname, change);

    if (change == DYN_IF_DELETE) {
        iflist_remove_if(ifname);
    } else {
        iflist_update_if(ifname);
    }
}

/* Initialize the component */
static void init(void) {
    /* Start interface monitoring.*/
    dyn_iface_init();

    /* Subscribe to all interface events */
    dyn_iface_subscribe(if_callback, DYN_IF_ALL);
}

/* Setup this component on startup */
static void setup(struct confd_daemon_ctx *dctx, int rsock)
{
    /* Subscribe to all changes for interfaces/ */
    spoint = subscribe(NORMAL_PRIO, handle_update, "interfaces");

    LOG("ietf_interfaces started, spoint: %d.", spoint);
}

/* This is the central iterator function, it will be called once for
 * each change to the /interfaces subtree. */
static enum cdb_iter_ret iter(confd_hkeypath_t *kp,
                              enum cdb_iter_op op,
                              confd_value_t *oldv,
                              confd_value_t *newv,
                              void *state)
{
    int   tag, idx;
    int   rsock  = *((int *) state);
    int   enabled;
    char *ifname;
    struct confd_identityref type;
    struct if_subscriber *ifs;

    enum cdb_iter_ret status = ITER_CONTINUE;

    LOG("keypath: %s.", KPSTR(kp));

    /* Top level entity */
    idx = kp->len;

    tag = GET_NEXT_TAG;

    if (tag == if_interfaces) {
        tag = GET_NEXT_TAG;

        /* interfaces/interface{X} */
        if (tag == if_interface) {
            idx = idx - 1;
            ifname = (char *) CONFD_GET_BUFPTR(&kp->v[idx][0]);

            if (idx == 0) {
                if (op == MOP_DELETED) {
                    LOG("Deleting: %s.", ifname);
                    iflist_remove_cdb(ifname);
                } else {
                    /* Update or add. */
                    CHECK3(cdb_pushd(rsock, "%h", kp),
                           "Pushd to interface failed", ITER_STOP);

                    CHECK3(cdb_get_bool(rsock, &enabled, "enabled"),
                           "Failed to get enabled state.", ITER_STOP);

                    CHECK3(cdb_get_identityref(rsock, &type, "type"),
                           "Failed to get interface type.", ITER_STOP);

                    if (type.ns != ianaift__ns) {
                        error("Identity ref not in iana-if-type!");
                    }

                    LOG("if: %s, enabled: %d, iref: %d",
                        ifname, enabled, type.id);

                    iflist_update_cdb(ifname, enabled, type.id);

                    cdb_popd(rsock);

                    status = ITER_RECURSE;
                }
            } else {
                tag = GET_NEXT_TAG;

                /* Something below /interfaces/interface{X} */

                /* We ignore the elements from the default model. */
                if ((tag == if_enabled)
                    || (tag == if_name)
                    || (tag == if_type)
                    || (tag == if_description)
                    || (tag == if_link_up_down_trap_enable)) {
                    return status;
                }

                /* For all the other tags we look for a registered
                 * handler. */
                for(ifs = ifs_root; ifs != NULL; ifs = ifs->next) {
                    if (ifs->tag == tag) {
                        return ifs->handler(kp, op, oldv, newv, state);
                    }
                }

                error("Couldn't find a handler for %s.", KPSTR(kp));
            }
        }
    }

    return status;
}

/* This function is called on every update to the subscription. */
static void handle_update(int rsock, int ssock, int spoint)
{
    LOG("rsock: %d, spoint: %d.", rsock, spoint);

    CHECK2(cdb_diff_iterate(ssock, spoint, iter, 0, &rsock),
           "Failed to call cdb_diff_iterate.");

}

/* Set the interface to the configured state. */
static void set_ifstate(char *ifname, int state) {
    LOG("Triggered for %s, state: %d", ifname, state);

    run(NULL, 0, "%s link set %s %s", IP_CMD, ifname,
        state ? "up" : "down");
}

/*
 * Helpers to manage the iflist updates from either a CBD or dyn-iface
 * direction.
 */
void if_register_if_handler(if_state_handler handler) {
    struct ifsubs *ifs;
    struct ifinfo *p;

    ifs           = xmalloc(sizeof(struct ifsubs));
    ifs->handler  = handler;
    ifs->next     = ifsublist;
    ifsublist     = ifs;

    /* Push existing states */
    for (p=iflist; p != NULL; p = p->next) {
        ifs->handler(p->ifname, p->adminState,
                     p->exist_cdb,
                     p->exist_iflist);
    }

}

/*
 * Signal all interface state subscribers about the current state of
 * ifp.
 */
static void signal_if_subs(struct ifinfo *ifp) {
    struct ifsubs *ifs;

    LOG("Interface: %s", ifp->ifname);

    for (ifs = ifsublist; ifs != NULL; ifs = ifs->next) {
        ifs->handler(ifp->ifname, ifp->adminState,
                     ifp->exist_cdb,
                     ifp->exist_iflist);
    }
}

/* Find an interface in the interface list */
static struct ifinfo *iflist_find(char *ifname) {
    struct ifinfo *p;

    for (p = iflist; p != NULL; p = p->next) {
        if (strcmp(ifname, p->ifname) == 0)
            break;
    }

    return p;
}

/* Update the cdb status for a given interface */
static void iflist_update_cdb(char *ifname, int enabled, int iftype) {
    struct ifinfo *p;
    int    signal = 0;

    p = iflist_find(ifname);

    /* Allocate a new record if it didn't exist. */
    if (p == NULL) {
        p               = xmalloc(sizeof(struct ifinfo));

        p->next         = iflist;
        p->ifname       = strdup(ifname);
        p->exist_iflist = 0;
        iflist          = p;
        signal          = 1;
    } else {
        if ((!p->exist_cdb) || (enabled != p->adminState))
            signal = 1;
    }

    p->iftype     = iftype;
    p->adminState = enabled;
    p->exist_cdb  = 1;


    /* It was existing and has now been configured */
    if (p->exist_iflist) {
        set_ifstate(ifname, p->adminState);
    }

    if (signal) {
        signal_if_subs(p);
    }
}

/* Update iflist when an interface is removed from cdb. */
static void iflist_remove_cdb(char *ifname) {
    struct ifinfo *p, *prev;

    for (p = iflist, prev = NULL; p != NULL; prev = p, p = p->next) {
        if (strcmp(ifname, p->ifname) == 0) {
            if (p->exist_cdb) {
                p->exist_cdb = 0;
                signal_if_subs(p);
            }

            if (! p->exist_iflist) {
                /* Trash any element that exists neither in cdb nor
                 * iflist */
                if (prev == NULL)
                    iflist = p->next;
                else
                    prev->next = p->next;
                XFREE(p->ifname);
                XFREE(p);
            }
            return;
        }
    }

    warn("Interface %s not found!", ifname);
}

/* Update the interface list when an interface has appeared in the
 * system (from the OS). */
static void iflist_update_if(char *ifname) { struct ifinfo *p;
    int signal = 0;

    p = iflist_find(ifname);

    /* Allocate a new record if it didn't exist. */
    if (p == NULL) {
        p               = xmalloc(sizeof(struct ifinfo));

        p->next         = iflist;
        p->ifname       = strdup(ifname);
        p->exist_cdb    = 0;
        p->adminState   = 0;
        iflist          = p;
        signal          = 1;
    } else {
        if (! p->exist_iflist )
            signal = 1;
    }

    p->exist_iflist  = 1;

    /* It was pre-provisioned and has now appeared */
    if (p->exist_cdb) {
        set_ifstate(ifname, p->adminState);
    }

    if (signal) {
        signal_if_subs(p);
    }
}

/* Called when an interface has been removed from the system (from the
 * OS). */
static void iflist_remove_if(char *ifname) {
    struct ifinfo *p, *prev;

    for (p = iflist, prev = NULL; p != NULL; prev = p, p = p->next) {
        if (strcmp(ifname, p->ifname) == 0) {
            if (p->exist_iflist) {
                p->exist_iflist = 0;
                signal_if_subs(p);
            }

            if (! p->exist_cdb) {
                /* Trash any element that exists neither in cdb nor
                 * iflist */
                if (prev == NULL)
                    iflist = p->next;
                else
                    prev->next = p->next;
                XFREE(p->ifname);
                XFREE(p);
            }
            return;
        }
    }

    warn("Interface %s not found!", ifname);
}

/* Calculate the current length of the interface list. */
static int iflist_len() {
    struct ifinfo *p;
    int    cnt;

    for(p = iflist, cnt = 0; p != NULL; p = p->next, cnt = cnt + 1)
        ;

    return cnt;
}

/* Data provider for /interfaces-state/ */
static int if_get_elem(struct confd_trans_ctx *tctx,
                       confd_hkeypath_t *kp) {
    int tag, idx;
    if_entry_t *ife;
    char *ifname;
    struct confd_identityref idref;
    confd_value_t v;
    struct sysinfo sys_info;
    struct confd_datetime dt;
    time_t last_time;

    LOG("%s", KPSTR(kp));
    idx = kp->len;
    tag = GET_NEXT_TAG;

    /* /interfaces-state/ */
    if (tag == if_interfaces_state) {
        tag = GET_NEXT_TAG;

        /* /interfaces-state/interface{X}/ */
        if (tag == if_interface) {
            idx = idx - 1;
            ifname = (char *) CONFD_GET_BUFPTR(&kp->v[idx][0]);
            tag = GET_NEXT_TAG;
            LOG("ifName: %s, tag: %s (%d)",
                ifname, confd_hash2str(tag), tag);

            ife = get_if_entry_n(ifname);

            if (ife == NULL) {
                warn("Didn't find if");
                NOT_FOUND;
            }

            switch (tag) {

            case if_name:
                CONFD_SET_STR(&v, ifname);
                break;
            case if_type:
                idref.ns = ianaift__ns;
                idref.id = ife->ifIanaType;
                CONFD_SET_IDENTITYREF(&v, idref);
                break;

            case if_admin_status:
                CONFD_SET_ENUM_VALUE(&v, ife->ifAdminStatus);
                break;
            case if_oper_status:
                CONFD_SET_ENUM_VALUE(&v, ife->ifOperStatus);
                break;

            case if_last_change:
                // XXX: Check on implementation.
                if (ife->ifLastChange == 0) {
                    NOT_FOUND;
                }

                sysinfo(&sys_info);
                time(&last_time);
                LOG("Time: %d, ut: %d lt: %d",
                    ife->ifLastChange, sys_info.uptime, last_time);
                last_time = last_time + ife->ifLastChange - sys_info.uptime;

                time_to_dt(last_time, &dt);

                CONFD_SET_DATETIME(&v, dt);
                break;

            case if_if_index:
                CONFD_SET_INT32(&v, ife->ifIndex);
                break;

            case if_phys_address:
                CONFD_SET_BINARY(&v, ife->ifPhysAddress,
                                 ife->ifPhysAddress_len);
                break;

            case if_higher_layer_if:
                /* Nothing implemented by default, as we add support
                 * for specific interfaces we (may) need to extend this */
                NOT_FOUND;
                break;

            case if_lower_layer_if:
                /* Nothing implemented by default, as we add support
                 * for specific interfaces we (may) need to extend this */
                NOT_FOUND;
                break;

            case if_speed:
                CONFD_SET_UINT64(&v, ife->ifSpeed);
                break;

            /* interfaces-state/statistics/  sub-tree*/
            case if_statistics:
                tag = GET_NEXT_TAG;

                switch (tag) {
                case if_discontinuity_time:
                    // XXX: Check on implementation
                    if (ife->ifCounterDiscontinuityTime == 0)
                        NOT_FOUND;
                    time_to_dt(ife->ifCounterDiscontinuityTime, &dt);
                    CONFD_SET_DATETIME(&v, dt);

                case if_in_octets:
                    CONFD_SET_UINT64(&v, ife->ifInOctets);
                    break;

                case if_in_unicast_pkts:
                    CONFD_SET_UINT64(&v, ife->ifInUcastPkts);
                    break;

                case if_in_broadcast_pkts:
                    CONFD_SET_UINT64(&v, ife->ifInBroadcastPkts);
                    break;

                case if_in_multicast_pkts:
                    CONFD_SET_UINT64(&v, ife->ifInMulticastPkts);
                    break;

                case if_in_discards:
                    CONFD_SET_UINT32(&v, ife->ifInDiscards);
                    break;

                case if_in_errors:
                    CONFD_SET_UINT32(&v, ife->ifInErrors);
                    break;

                case if_in_unknown_protos:
                    CONFD_SET_UINT32(&v, ife->ifInUnknownProtos);
                    break;

                case if_out_octets:
                    CONFD_SET_UINT64(&v, ife->ifOutOctets);
                    break;

                case if_out_unicast_pkts:
                    CONFD_SET_UINT64(&v, ife->ifOutUcastPkts);
                    break;

                case if_out_broadcast_pkts:
                    CONFD_SET_UINT64(&v, ife->ifOutBroadcastPkts);
                    break;

                case if_out_multicast_pkts:
                    CONFD_SET_UINT64(&v, ife->ifOutMulticastPkts);
                    break;

                case if_out_discards:
                    CONFD_SET_UINT32(&v, ife->ifOutDiscards);
                    break;

                case if_out_errors:
                    CONFD_SET_UINT32(&v, ife->ifOutErrors);
                    break;

                default:
                    warn("Unhandled tag %s!", confd_hash2str(tag));
                    NOT_FOUND;
                }
                break;

            default:
                warn("Unhandled tag %s!", confd_hash2str(tag));
                NOT_FOUND;
            }

            confd_data_reply_value(tctx, &v);
            return CONFD_OK;

        }
    }

    error("No handler available for %s", KPSTR(kp));

    NOT_FOUND;

}

/* This is the get_next for the if oper data. It uses an index to
 * keep track of the next interface in the if/table. */
static int if_get_next(struct confd_trans_ctx *tctx,
                       confd_hkeypath_t *kp,
                       long next) {

    LOG("%s - %d", KPSTR(kp), next);

    /* We re-use the ipmibs implementation */
    return if_status_get_next_name(tctx, kp, next);
}

/* CLI completion for interface names */
static int if_complete(struct confd_user_info *uinfo,
                       int cli_style, char *token, int completion_char,
                       confd_hkeypath_t *kp,
                       char *cmdpath, char *cmdparam_id,
                       struct confd_qname *simpleType, char *extra)
{
    int num = iflist_len();
    int ix;
    struct confd_completion_value *values;
    struct ifinfo *p;

    LOG("Number of interfaces: %d.", num);

    values = malloc( sizeof(struct confd_completion_value) * num );

    /* Offer up all the interfaces in iflist, both those from cdb
     * and those from dyn_iface. */
    for(p = iflist, ix = 0; p != NULL; p = p->next, ix = ix + 1) {
        values[ix].type  = CONFD_COMPLETION;
        values[ix].value = strdup(p->ifname);
        values[ix].extra = NULL;

        if (ix > num) {
            error("Extra interfaces in list?");
            break;
        }

    }

    CHECK(confd_action_reply_completion(uinfo, values, num),
          "Failed to send completion reply.");

    for(ix=0; ix < num; ix++)
        XFREE(values[ix].value);
    XFREE(values);

    return CONFD_OK;
}

/* Interface to register a sub handler for interface data. */
void if_register_sub_handler(int tag,
                             iter_handler handler) {
    struct if_subscriber *newifs;

    newifs = xmalloc(sizeof(struct if_subscriber));
    newifs->next    = ifs_root;
    newifs->handler = handler;
    newifs->tag     = tag;

    ifs_root = newifs;
}
