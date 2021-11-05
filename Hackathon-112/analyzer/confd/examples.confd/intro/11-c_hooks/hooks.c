/*********************************************************************
 * ConfD hooks example
 *
 * (C) 2005-2017 Tail-f Systems
 * Permission to use this code as a starting point hereby granted
 * This is ConfD Sample Code.
 *
 * See the README file for more information
 ********************************************************************/

#include <arpa/inet.h>
#include <netinet/in.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>

#include <confd_lib.h>
#include <confd_dp.h>
#include <confd_maapi.h>

#define _TRACE_DECLARE
#include <traceh.h>
#include "hooks.h"

/* Our daemon context as a global variable */
static struct confd_daemon_ctx *dctx;
static int ctlsock;
static int workersock;
static int maapisock;

/*
 Both transaction init and validation init will do maapi_attach().
 We keep track of how many times the init function has been called
 so that we can do maapi_detach() in the correct place.
 */
struct trans_data {
    int refc;
};

static void trace_confd_val(const char *txt, const confd_value_t *val)
{
#ifdef T_LOG_TRACE
    char buf[512];
    confd_pp_value(buf, sizeof(buf), val);
    TRACE("%s%s", txt, buf);
#endif
}

static void trace_confd_kp(const char *txt, confd_hkeypath_t *kp)
{
#ifdef T_LOG_TRACE
    if (kp) {
        char buf[1024];
        confd_pp_kpath(buf, sizeof(buf), kp);
        TRACE("%s%s", txt, buf);
    } else {
        TRACE("%s%p", txt, kp);
    }
#endif
}

static int t_init(struct confd_trans_ctx *tctx)
{
    DEBUG_ENTER("");
    int rv = CONFD_OK;
    struct trans_data *td = (struct trans_data*) tctx->t_opaque;

    if (td == NULL) {
        td = (struct trans_data*) malloc(sizeof(struct trans_data));
        memset(td, 0, sizeof(struct trans_data));
        tctx->t_opaque = td;
        if (maapi_attach(maapisock, 0, tctx) != CONFD_OK) {
            return CONFD_ERR;
        }
    }
    td->refc++;
    confd_trans_set_fd(tctx, workersock);

    DEBUG_EXIT("rv=%i", rv);
    return rv;
}

static int t_finish(struct confd_trans_ctx *tctx)
{
    DEBUG_ENTER("");
    int rv = CONFD_OK;
    struct trans_data *td = (struct trans_data*) tctx->t_opaque;

    td->refc--;
    if (td->refc == 0) {
        tctx->t_opaque = NULL;
        free(td);
        if (maapi_detach(maapisock, tctx) != CONFD_OK) {
            return CONFD_ERR;
        }
    }

    DEBUG_EXIT("rv=%i", rv);
    return rv;
}

static int hook_ip_mask_set(struct confd_trans_ctx *tctx,
        confd_hkeypath_t *keypath, confd_value_t *newval)
{
    DEBUG_ENTER("");
    int rv = CONFD_ERR;
    trace_confd_kp("keypath=", keypath);
    trace_confd_val("newval=", newval);
    INFO("ip_mask: Set hook detected.");

    char path[1024];
    confd_pp_kpath(path, sizeof(path), keypath);
    char *last_slash = strrchr(path, '/');
    if (last_slash) {
        last_slash[0] = '\0';
    } else {
        FATAL("Missing '/' in keypath!");
        goto term;
    }
    INFO("ip_mask: Host path %s", path);

    confd_value_t ip, netmask;
    CONFD_SET_NOEXISTS(&ip);
    CONFD_SET_NOEXISTS(&netmask);
    if (maapi_exists(maapisock, tctx->thandle, "%s/ip", path)) {
        TRACE("ip_mask: ip exits");
        if (CONFD_OK
                != maapi_get_elem(maapisock, tctx->thandle, &ip, "%s/ip",
                        path)) {
            FATAL("Failed to get ip!");
            goto term;
        }
    }
    if (maapi_exists(maapisock, tctx->thandle, "%s/netmask", path)) {
        TRACE("ip_mask: netmask exists");
        if (CONFD_OK
                != maapi_get_elem(maapisock, tctx->thandle, &netmask,
                        "%s/netmask", path)) {
            FATAL("Failed to get netmask!");
            goto term;
        }
    }
    trace_confd_val("ip=", &ip);
    trace_confd_val("netmask=", &netmask);

    if (ip.type != C_NOEXISTS && netmask.type != C_NOEXISTS) {
        if (!maapi_exists(maapisock, tctx->thandle, "%s/gw", path)) {
            TRACE("ip_mask: gw not set");
            struct in_addr ip_addr = CONFD_GET_IPV4(&ip);
            struct in_addr netmask_addr = CONFD_GET_IPV4(&netmask);
            struct in_addr gw_addr;
            TRACE("ip_addr.s_addr %x netmask_addr.s_addr %x", ip_addr.s_addr,
                    netmask_addr.s_addr);
            gw_addr.s_addr = (ip_addr.s_addr & netmask_addr.s_addr)
                    + 0x01000000;
            confd_value_t gw;
            CONFD_SET_IPV4(&gw, gw_addr);
            if (CONFD_OK
                    != maapi_set_elem(maapisock, tctx->thandle, &gw, "%s/gw",
                            path)) {
                FATAL("Failed to set gw!");
                goto term;
            }
        } else {
            INFO("ip_mask: gw for host %s already set.", path);
        }
    }

    rv = CONFD_OK;

term:
    DEBUG_EXIT("rv=%i", rv);
    return rv;
}

static int hook_ip_mask_create(struct confd_trans_ctx *tctx,
        confd_hkeypath_t *keypath)
{
    DEBUG_ENTER("");
    int rv = CONFD_ERR;
    trace_confd_kp("keypath=", keypath);

    WARN("ip_mask: This 'create hook' function  should not be called as create "
            "is invoked only on list elements!");

    DEBUG_EXIT("rv=%i", rv);
    return rv;
}

static int hook_ip_mask_remove(struct confd_trans_ctx *tctx,
        confd_hkeypath_t *keypath)
{
    DEBUG_ENTER("");
    int rv = CONFD_OK;
    trace_confd_kp("keypath=", keypath);

    INFO("ip_mask: Remove hook detected, no change done.");

    DEBUG_EXIT("rv=%i", rv);
    return rv;
}

/**
 * Convert IPv4 ConfD value to IPv6 ConfD value.
 * mapping uses following IPv6 prefix 0:0:0:0:0:FFFF:xxxx:xxxx
 * example mapping: 1.2.3.4 -> 0:0:0:0:0:FFFF:0102:0304
 * @param ipv4_val input
 * @param ipv6_val output
 * @return CONFD_OK or CONFD_ERR
 */
static int confd_value_ipv4_to_ipv6(const confd_value_t *ipv4_val,
        confd_value_t *ipv6_val)
{
    TRACE_ENTER("");
    int rv = CONFD_OK;
    struct in6_addr ip6;

    inet_pton(AF_INET6, "0:0:0:0:0:FFFF:0:0", &ip6);
    ((char*) (&ip6))[12] = ((char*) (&ipv4_val->val.ip))[0];
    ((char*) (&ip6))[13] = ((char*) (&ipv4_val->val.ip))[1];
    ((char*) (&ip6))[14] = ((char*) (&ipv4_val->val.ip))[2];
    ((char*) (&ip6))[15] = ((char*) (&ipv4_val->val.ip))[3];
     CONFD_SET_IPV6(ipv6_val, ip6);

    TRACE_EXIT("rv=%i", rv);
    return rv;
}

static int convert_ip_elem(const int th, const confd_value_t *key,
        const char* elem)
{
    TRACE_ENTER("elem=%s", elem);
    int rv = CONFD_ERR;
    confd_value_t ipv4, ipv6;

    if (maapi_exists(maapisock, th, "/hosts{%x}/%s", key, elem)) {
        if (CONFD_OK != maapi_get_elem(maapisock, th, &ipv4, "/hosts{%x}/%s",
                key, elem)) {
            FATAL("Failed to get ipv4 address!");
            goto term;
        }
        if (CONFD_OK != confd_value_ipv4_to_ipv6(&ipv4, &ipv6)) {
            FATAL("Failed to convert to ipv6 address!");
            goto term;
        }
        trace_confd_val("ip ipv4=", &ipv4);
        trace_confd_val("ip ipv6=", &ipv6);
        if (CONFD_OK != maapi_set_elem(maapisock, th, &ipv6,
                "/hosts_ipv6{%x}/%s", key, elem)) {
            FATAL("Failed to set elem=%s!", elem);
            goto term;
        }
    }
    rv = CONFD_OK;

term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

/**
 * Create ipv6 hosts according to (ipv4) hosts that do not exists
 * @param th transaction handle
 * @return CONFD_OK or CONFD_ERR
 */
static int create_ipv6_hosts(const int th)
{
    TRACE_ENTER("");
    int rv = CONFD_ERR;
    struct maapi_cursor mc;

    TRACE("Initializing  maapi cursor");
    maapi_init_cursor(maapisock, th, &mc, "/hosts");
    maapi_get_next(&mc);
    while (mc.n != 0) {
        trace_confd_val("mc.keys[0] ", &mc.keys[0]);
        if (!maapi_exists(maapisock, th, "/hosts_ipv6{%x}",
                &mc.keys[0])) {
            TRACE("Ipv6 host does not exists, creating...");
            if (CONFD_OK
                    != maapi_create(maapisock, th, "/hosts_ipv6{%x}",
                            &mc.keys[0])) {
                FATAL("Failed to create ipv6 host!");
                goto term;
            }
            if (CONFD_OK != convert_ip_elem(th, &mc.keys[0], "ip")) {
                FATAL("Failed to convert to ip to ipv6 address!");
                goto term;
            }
            if (CONFD_OK != convert_ip_elem(th, &mc.keys[0], "gw")) {
                FATAL("Failed to convert to gw to ipv6 address!");
                goto term;
            }
        }
        maapi_get_next(&mc);
    }
    maapi_destroy_cursor(&mc);

    rv = CONFD_OK;

term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

/**
 * Delete ipv6 hosts that do not have corresponding (ipv4) host
 * @param th transaction handle
 * @return CONFD_OK or CONFD_ERR
 */
static int delete_ipv6_hosts(const int th)
{
    TRACE_ENTER("");
    int rv = CONFD_ERR;
    struct maapi_cursor mc;

    TRACE("Initializing  maapi cursor");
    maapi_init_cursor(maapisock, th, &mc, "/hosts_ipv6");
    maapi_get_next(&mc);
    while (mc.n != 0) {
        trace_confd_val("mc.keys[0] ", &mc.keys[0]);
        if (!maapi_exists(maapisock, th, "/hosts{%x}", &mc.keys[0])) {
            TRACE("Ipv4 host does not exists, deleting ipv6 host");
            if (CONFD_OK
                    != maapi_delete(maapisock, th, "/hosts_ipv6{%x}",
                            &mc.keys[0])) {
                FATAL("Failed to delete ipv6 host!");
                goto term;
            }
        }
        maapi_get_next(&mc);
    }
    maapi_destroy_cursor(&mc);

    rv = CONFD_OK;

term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

static int hook_hosts_write_all(struct confd_trans_ctx *tctx,
        confd_hkeypath_t *keypath)
{
    DEBUG_ENTER("keypath %p", keypath);
    int rv = CONFD_ERR;
    trace_confd_kp("keypath=", keypath);

    if (CONFD_OK != create_ipv6_hosts(tctx->thandle)) {
        FATAL("Failed to create ipv6 hosts!");
        goto term;
    }

    if (CONFD_OK != delete_ipv6_hosts(tctx->thandle)) {
           FATAL("Failed to create ipv6 hosts!");
           goto term;
    }

    rv = CONFD_OK;

term:
    DEBUG_EXIT("rv=%i", rv);
    return rv;
}

int main(int argc, char *argv[])
{
    DEBUG_ENTER("");
    struct sockaddr_in addr;
    int debuglevel = CONFD_DEBUG;
    struct confd_trans_cbs trans;
    struct confd_data_cbs data;

    /* initialize confd library */
    confd_init("hooks_daemon", stderr, debuglevel);

    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(CONFD_PORT);

    if (confd_load_schemas((struct sockaddr*) &addr,
            sizeof(struct sockaddr_in)) != CONFD_OK)
        confd_fatal("Failed to load schemas from confd\n");

    if ((dctx = confd_init_daemon("hooks_daemon")) == NULL)
        confd_fatal("Failed to initialize confdlib\n");

    /* Create the first control socket, all requests to */
    /* create new transactions arrive here */

    if ((ctlsock = socket(PF_INET, SOCK_STREAM, 0)) < 0)
        confd_fatal("Failed to open ctlsocket\n");
    if (confd_connect(dctx, ctlsock, CONTROL_SOCKET, (struct sockaddr*) &addr,
            sizeof(struct sockaddr_in)) < 0)
        confd_fatal("Failed to confd_connect() to confd \n");

    /* Also establish a workersocket, this is the most simple */
    /* case where we have just one ctlsock and one workersock */

    if ((workersock = socket(PF_INET, SOCK_STREAM, 0)) < 0)
        confd_fatal("Failed to open workersocket\n");
    if (confd_connect(dctx, workersock, WORKER_SOCKET, (struct sockaddr*) &addr,
            sizeof(struct sockaddr_in)) < 0)
        confd_fatal("Failed to confd_connect() to confd \n");

    /* Establish a MAAPI socket */
    if ((maapisock = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
        confd_fatal("Failed to open socket\n");
    }

    if (maapi_connect(maapisock, (struct sockaddr*) &addr,
            sizeof(struct sockaddr_in)) != CONFD_OK) {
        confd_fatal("Failed to confd_connect() to confd\n");
    }

    memset(&trans, 0, sizeof(trans));
    trans.init = t_init;
    trans.finish = t_finish;
    if (confd_register_trans_cb(dctx, &trans) == CONFD_ERR)
        confd_fatal("Failed to register trans cb \n");

    memset(&data, 0, sizeof(data));
    data.create = hook_ip_mask_create;
    data.remove = hook_ip_mask_remove;
    data.set_elem = hook_ip_mask_set;
    strcpy(data.callpoint, hooks__callpointid_ip_mask);

    if (confd_register_data_cb(dctx, &data) == CONFD_ERR)
        confd_fatal("Failed to register data cb \n");

    memset(&data, 0, sizeof(data));
    data.write_all = hook_hosts_write_all;
    strcpy(data.callpoint, hooks__callpointid_trans_hosts);
    if (confd_register_data_cb(dctx, &data) == CONFD_ERR)
        confd_fatal("Failed to register data cb \n");

    if (confd_register_done(dctx) != CONFD_OK)
        confd_fatal("Failed to complete registration \n");

    DEBUG("Initialization complete");
    while (1) {
        struct pollfd set[2];
        int ret;

        set[0].fd = ctlsock;
        set[0].events = POLLIN;
        set[0].revents = 0;

        set[1].fd = workersock;
        set[1].events = POLLIN;
        set[1].revents = 0;

        if (poll(set, sizeof(set) / sizeof(*set), -1) < 0) {
            perror("Poll failed:");
            continue;
        }

        /* Check for I/O */
        if (set[0].revents & POLLIN) {
            if ((ret = confd_fd_ready(dctx, ctlsock)) == CONFD_EOF) {
                confd_fatal("Control socket closed\n");
            } else if (ret == CONFD_ERR && confd_errno != CONFD_ERR_EXTERNAL) {
                confd_fatal("Error on control socket request: %s (%d): %s\n",
                        confd_strerror(confd_errno), confd_errno,
                        confd_lasterr());
            }
        }
        if (set[1].revents & POLLIN) {
            if ((ret = confd_fd_ready(dctx, workersock)) == CONFD_EOF) {
                confd_fatal("Worker socket closed\n");
            } else if (ret == CONFD_ERR && confd_errno != CONFD_ERR_EXTERNAL) {
                confd_fatal("Error on worker socket request: %s (%d): %s\n",
                        confd_strerror(confd_errno), confd_errno,
                        confd_lasterr());
            }
        }
    }
    INFO_EXIT("");
}
