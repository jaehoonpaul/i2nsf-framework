/*********************************************************************
 * ConfD Transformation callpoint example
 *
 * This is ConfD Sample Code.
 *
 * (C) 2017 Tail-f Systems
 * Permission to use this code as a starting point hereby granted
 *
 * See the README file for more information
 ********************************************************************/

#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/poll.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <confd_lib.h>
#include <confd_dp.h>
#include <confd_maapi.h>

// for enabling traceh.h (examples.confd/include)
#define _TRACE_DECLARE
#include <traceh.h>

#include "common.h"
#include "transform_cb.h"

#define DAMEON_NAME_STR "transform"

// return the struct sockaddr_in with defined IPv4 address / port
struct sockaddr_in get_sockaddr_by_ip_port(in_addr_t addr, in_port_t port) {
    struct sockaddr_in sock_addr;
    sock_addr.sin_addr.s_addr = addr;
    sock_addr.sin_family = AF_INET;
    sock_addr.sin_port = htons(port);
    return sock_addr;
}

// initialize socket connection of specific type towards ConfD
int confd_sock_init(
    in_addr_t addr, in_port_t port,
    struct confd_daemon_ctx *dctx,
    enum confd_sock_type type,
    int *out_sock
) {
    struct sockaddr_in dest_addr = get_sockaddr_by_ip_port(addr, port);

    *out_sock = socket(PF_INET, SOCK_STREAM, 0);
    if (*out_sock < 0) {
        confd_fatal("Failed to open socket\n");
    }

    int res = confd_connect(dctx, *out_sock, type,
                (struct sockaddr*)&dest_addr, sizeof (struct sockaddr_in));
    if (res < 0) {
        confd_fatal("Failed to confd_connect() to confd \n");
    }

    return CONFD_OK;
}

// initialize MAAPI socket towards ConfD to read/write the transaction data
int maapi_sock_init(in_addr_t addr, in_port_t port, int *out_sock)
{
    struct sockaddr_in dest_addr = get_sockaddr_by_ip_port(addr, port);

    *out_sock = socket(PF_INET, SOCK_STREAM, 0);
    if (*out_sock < 0) {
        confd_fatal("Failed to open MAAPI socket\n");
    }

    int res = maapi_connect(*out_sock, (struct sockaddr*)&dest_addr,
                      sizeof (struct sockaddr_in));
    if (res < 0) {
        confd_fatal("Failed to maapi_connect() to confd \n");
    }

    return CONFD_OK;
}

// check for log level setting coming in from command line parameters
static int get_debug_level(int argc, char **argv) {
    int debuglevel = CONFD_DEBUG;
    int c;
    while ((c = getopt(argc, argv, "tdpsr")) != -1) {
        switch(c) {
        case 't':
            debuglevel = CONFD_TRACE;
            break;
        case 'd':
            debuglevel = CONFD_DEBUG;
            break;
        case 'p':
            debuglevel = CONFD_PROTO_TRACE;
            break;
        case 's':
            debuglevel = CONFD_SILENT;
            break;
        }
    }

    return debuglevel;
}

static int init_transformation(struct confd_trans_ctx *tctx)
{
    TRACE_ENTER("");
    OK(maapi_attach(glob.maapi_socket, 0, tctx));
    confd_trans_set_fd(tctx, glob.workersock);
    tctx->t_opaque = transform_alloc_opaque_data();
    TRACE_EXIT("");
    return CONFD_OK;
}

static int stop_transformation(struct confd_trans_ctx *tctx)
{
    TRACE_ENTER("");
    transform_free_opaque_data(tctx->t_opaque);
    tctx->t_opaque = NULL;
    TRACE_EXIT("");
    return CONFD_OK;
}

// main daemon execution
int main(int argc, char **argv)
{
    int res = CONFD_OK;

    // initialize the library as a first mandatory step
    confd_init(DAMEON_NAME_STR, stderr, get_debug_level(argc, argv));

    // ConfD address to be contacted
    in_addr_t confd_addr = inet_addr("127.0.0.1");

    struct confd_daemon_ctx *dctx = confd_init_daemon(DAMEON_NAME_STR);
    if (NULL == dctx) {
        confd_fatal("Failed to initialize confd\n");
    }

    int dflags = CONFD_DAEMON_FLAG_LEAF_LIST_AS_LEAF;
    confd_set_daemon_flags(dctx, dflags);

    // load schemas to get a nicer prints (keypath tag names etc.)
    struct sockaddr_in confd_sock_addr = get_sockaddr_by_ip_port(confd_addr,
                                        CONFD_PORT);
    res = confd_load_schemas((struct sockaddr*)&confd_sock_addr,
                           sizeof (struct sockaddr_in));
    if (res != CONFD_OK) {
        confd_fatal("Failed to load schemas from confd\n");
    }
    TRACE("Schemas loaded.");

    // create the first control socket;
    // all requests to create new transactions arrive here.
    OK(confd_sock_init(confd_addr, CONFD_PORT, dctx, CONTROL_SOCKET,
        &glob.ctlsock));

    // also establish a workersocket, this is the most simple
    // case where we have just one ctlsock and one workersock.
    OK(confd_sock_init(confd_addr, CONFD_PORT, dctx, WORKER_SOCKET,
        &glob.workersock));

    // register the transformation callpoint
    struct confd_trans_cbs tcb;
    memset(&tcb, 0x00, sizeof(tcb));
    tcb.init = init_transformation;
    tcb.finish = stop_transformation;
    confd_register_trans_cb(dctx, &tcb);

    struct confd_data_cbs * data = transform_cb();

    if (confd_register_data_cb(dctx, data) == CONFD_ERR)
        confd_fatal("Failed to register data cb \n");

    if (confd_register_done(dctx) != CONFD_OK)
        confd_fatal("Failed to complete registration \n");

    // initialize the MAAPI socket required for transform code
    OK(maapi_sock_init(confd_addr, 4565, &glob.maapi_socket));

    INFO("All registrations/init done, entering poll loop...");

    // handle infinite socket loop
    while (1) {
        struct pollfd set[2];
        int ret;

        set[0].fd = glob.ctlsock;
        set[0].events = POLLIN;
        set[0].revents = 0;

        set[1].fd = glob.workersock;
        set[1].events = POLLIN;
        set[1].revents = 0;

        if (poll(&set[0], 2, -1) < 0) {
            perror("Poll failed:");
            continue;
        }

        if (set[0].revents & POLLIN) {
            if ((ret = confd_fd_ready(dctx, glob.ctlsock)) == CONFD_EOF) {
                confd_fatal("Control socket closed\n");
            } else if (ret == CONFD_ERR && confd_errno != CONFD_ERR_EXTERNAL) {
                confd_fatal("Error on control socket request: %s (%d): %s\n",
                     confd_strerror(confd_errno), confd_errno, confd_lasterr());
            }
        }
        if (set[1].revents & POLLIN) {
            if ((ret = confd_fd_ready(dctx, glob.workersock)) == CONFD_EOF) {
                confd_fatal("Worker socket closed\n");
            } else if (ret == CONFD_ERR && confd_errno != CONFD_ERR_EXTERNAL) {
                confd_fatal("Error on worker socket request: %s (%d): %s\n",
                     confd_strerror(confd_errno), confd_errno, confd_lasterr());
            }
        }

    }

    return 0;
}