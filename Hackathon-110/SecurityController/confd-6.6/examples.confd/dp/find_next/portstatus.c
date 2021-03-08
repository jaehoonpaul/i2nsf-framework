#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/poll.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>
#include <time.h>
#include <string.h>

#include <confd_lib.h>
#include <confd_dp.h>
#include "ports.h"

/********************************************************************/

/* Our list - this simple example just uses a static list */
struct entry {
    int8_t slot;
    int8_t port;
    int32_t status;
};
static struct entry list[] = {
    { .slot = 1, .port = 0, .status = ports_disabled },
    { .slot = 1, .port = 1, .status = ports_active },
    { .slot = 1, .port = 2, .status = ports_error },
    { .slot = 2, .port = 1, .status = ports_disabled },
    { .slot = 2, .port = 2, .status = ports_active },
    { .slot = 4, .port = 0, .status = ports_error },
    { .slot = 4, .port = 2, .status = ports_disabled }
};
static int n_list_entries = sizeof(list)/sizeof(list[0]);

/* Our daemon context as a global variable */
static struct confd_daemon_ctx *dctx;
static int ctlsock;
static int workersock;

/********************************************************************/

static int get_entry(int8_t slot, int8_t port)
{
    int pos;

    for (pos = 0; pos < n_list_entries; pos++) {
        if (list[pos].slot == slot && list[pos].port == port)
            return pos;
    }
    return -1;
}

/********************************************************************/

static int s_init(struct confd_trans_ctx *tctx)
{
    confd_trans_set_fd(tctx, workersock);
    return CONFD_OK;
}

/********************************************************************/

static int find_next(struct confd_trans_ctx *tctx,
                     confd_hkeypath_t *kp,
                     enum confd_find_next_type type,
                     confd_value_t *keys, int nkeys)
{
    confd_value_t v[2];
    int8_t slot, port;
    int pos = -1;
    int i;

    switch (nkeys) {
    case 0:
        /* no keys provided => the first entry will always be "after" */
        if (n_list_entries > 0) {
            pos = 0;
        }
        break;
    case 1:
        /* one key provided => find the first entry "after"
           - since there can never be a "same" entry, 'type' is not relevant,
           and an entry with same first key is also "after" */
        slot = CONFD_GET_INT8(&keys[0]);
        for (i = 0; i < n_list_entries; i++) {
            if (list[i].slot >= slot) {
                pos = i;
                break;
            }
        }
        break;
    case 2:
        /* both keys provided => find first entry "after" or "same",
           depending on 'type' */
        slot = CONFD_GET_INT8(&keys[0]);
        port = CONFD_GET_INT8(&keys[1]);
        switch (type) {
        case CONFD_FIND_NEXT:
            /* entry must be "after" */
            for (i = 0; i < n_list_entries; i++) {
                if (list[i].slot > slot ||
                    (list[i].slot == slot && list[i].port > port)) {
                    pos = i;
                    break;
                }
            }
            break;
        case CONFD_FIND_SAME_OR_NEXT:
            /* entry must be "same" or "after" */
            for (i = 0; i < n_list_entries; i++) {
                if (list[i].slot > slot ||
                    (list[i].slot == slot && list[i].port >= port)) {
                    pos = i;
                    break;
                }
            }
            break;
        }
        break;
    default:
        confd_trans_seterr(tctx, "invalid number of keys: %d", nkeys);
        return CONFD_ERR;
    }

    if (pos >= 0) {
        /* matching entry found - return its keys and 'pos' for next entry */
        CONFD_SET_INT8(&v[0], list[pos].slot);
        CONFD_SET_INT8(&v[1], list[pos].port);
        confd_data_reply_next_key(tctx, v, 2, (long)(pos + 1));
    } else {
        /* no matching entry - i.e. end-of-list */
        confd_data_reply_next_key(tctx, NULL, -1, -1);
    }

    return CONFD_OK;
}


static int get_next(struct confd_trans_ctx *tctx,
                    confd_hkeypath_t *keypath,
                    long next)
{
    confd_value_t v[2];
    int pos;

    if (next == -1) {  /* first call */
        pos = 0;
    } else {
        pos = (int)next;
    }
    if (pos >= n_list_entries) { /* We have reached the end of the list*/
        confd_data_reply_next_key(tctx, NULL, -1, -1);
        return CONFD_OK;
    }
    /* return the keys from the list entry and 'pos' for next entry */
    CONFD_SET_INT8(&v[0], list[pos].slot);
    CONFD_SET_INT8(&v[1], list[pos].port);
    confd_data_reply_next_key(tctx, v, 2, (long)(pos + 1));

    return CONFD_OK;
}


/*
    path:     /port{<slot> <port>}/<leaf>
    kp index:  2    1              0
*/
static int get_elem(struct confd_trans_ctx *tctx,
                    confd_hkeypath_t *keypath)
{
    int8_t slot = CONFD_GET_INT8(&keypath->v[1][0]);
    int8_t port = CONFD_GET_INT8(&keypath->v[1][1]);
    int32_t leaf = CONFD_GET_XMLTAG(&keypath->v[0][0]);
    int pos = get_entry(slot, port);
    confd_value_t v;

    if (pos < 0) {
        confd_data_reply_not_found(tctx);
        return CONFD_OK;
    }

    switch (leaf) {
    case ports_slot:
        CONFD_SET_INT8(&v, list[pos].slot);
        break;
    case ports_port:
        CONFD_SET_INT8(&v, list[pos].port);
        break;
    case ports_status:
        CONFD_SET_ENUM_VALUE(&v, list[pos].status);
        break;
    default:
        confd_trans_seterr(tctx, "unknown leaf %d", leaf);
        return CONFD_ERR;
    }
    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}


/********************************************************************/

int main(int argc, char *argv[])
{
    struct sockaddr_in addr;
    int debuglevel = CONFD_TRACE;
    struct confd_trans_cbs trans;
    struct confd_data_cbs data;

    /* Initialize confd library */

    confd_init("portstatus", stderr, debuglevel);

    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(CONFD_PORT);

    if (confd_load_schemas((struct sockaddr*)&addr,
                           sizeof (struct sockaddr_in)) != CONFD_OK)
        confd_fatal("Failed to load schemas from confd\n");

    if ((dctx = confd_init_daemon("portstatus")) == NULL)
        confd_fatal("Failed to initialize daemon\n");

    /* Create and connect the control and worker sockets */

    if ((ctlsock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        confd_fatal("Failed to open ctlsocket\n");
    if (confd_connect(dctx, ctlsock, CONTROL_SOCKET, (struct sockaddr*)&addr,
                      sizeof (struct sockaddr_in)) < 0)
        confd_fatal("Failed to confd_connect() to confd \n");

    if ((workersock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        confd_fatal("Failed to open workersocket\n");
    if (confd_connect(dctx, workersock, WORKER_SOCKET,(struct sockaddr*)&addr,
                      sizeof (struct sockaddr_in)) < 0)
        confd_fatal("Failed to confd_connect() to confd \n");

    /* Register callbacks */

    memset(&trans, 0, sizeof(trans));
    trans.init = s_init;
    if (confd_register_trans_cb(dctx, &trans) == CONFD_ERR)
        confd_fatal("Failed to register trans cb\n");

    memset(&data, 0, sizeof (struct confd_data_cbs));
    data.get_elem = get_elem;
    data.get_next = get_next;
    data.find_next = find_next;
    strcpy(data.callpoint, ports__callpointid_ps);
    if (confd_register_data_cb(dctx, &data) == CONFD_ERR)
        confd_fatal("Failed to register data cb\n");

    if (confd_register_done(dctx) != CONFD_OK)
        confd_fatal("Failed to complete registration \n");

    while(1) {
        struct pollfd set[2];
        int ret;

        set[0].fd = ctlsock;
        set[0].events = POLLIN;
        set[0].revents = 0;

        set[1].fd = workersock;
        set[1].events = POLLIN;
        set[1].revents = 0;

        if (poll(set, sizeof(set)/sizeof(set[0]), -1) < 0) {
            perror("Poll failed:");
            continue;
        }

        /* Check for I/O */
        if (set[0].revents & POLLIN) {
            if ((ret = confd_fd_ready(dctx, ctlsock)) == CONFD_EOF) {
                confd_fatal("Control socket closed\n");
            } else if (ret == CONFD_ERR && confd_errno != CONFD_ERR_EXTERNAL) {
                confd_fatal("Error on control socket request: %s (%d): %s\n",
                     confd_strerror(confd_errno), confd_errno, confd_lasterr());
            }
        }
        if (set[1].revents & POLLIN) {
            if ((ret = confd_fd_ready(dctx, workersock)) == CONFD_EOF) {
                confd_fatal("Worker socket closed\n");
            } else if (ret == CONFD_ERR && confd_errno != CONFD_ERR_EXTERNAL) {
                confd_fatal("Error on worker socket request: %s (%d): %s\n",
                     confd_strerror(confd_errno), confd_errno, confd_lasterr());
            }
        }
    }
}

/********************************************************************/
