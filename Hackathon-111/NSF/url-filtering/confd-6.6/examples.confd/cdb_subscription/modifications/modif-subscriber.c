/*********************************************************************
 * ConfD cdb_get_modifications() example
 *
 * (C) 2017 Tail-f Systems
 * Permission to use this code as a starting point hereby granted
 * This is ConfD Sample Code
 *
 * See the README file for more information
 ********************************************************************/

#include <sys/poll.h>
#include <sys/socket.h>
#include <string.h>
#include <stdlib.h>
#include <confd.h>
#include <confd_cdb.h>

#define _TRACE_DECLARE
#include <traceh.h>

#include "user-folders.h"

#include "modif-printer.h"

/********************************************************************/
#define DAEMON_NAME "subscriber-modifications"
#define ROOT_PATH "/folder-user"
static int subscr_socket = -1;
static int subscr_point = -1;


int init_subscriber(void)
{
    INFO_ENTER("");

    int ret = CONFD_ERR;

    confd_init(DAEMON_NAME, stderr, CONFD_TRACE);

    struct sockaddr_in addr_in;
    addr_in.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr_in.sin_family = AF_INET;
    addr_in.sin_port = htons(CONFD_PORT);

    struct sockaddr *addr = (struct sockaddr *)&addr_in;

    ret = confd_load_schemas(addr, sizeof(*addr));
    if (CONFD_OK != ret) {
        confd_fatal("Failed to load schemas from ConfD\n");
    }

    if ((subscr_socket = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
        FATAL("Failed to open socket\n");
        goto term;
    }

    ret = cdb_connect(subscr_socket, CDB_SUBSCRIPTION_SOCKET,
            addr, sizeof(*addr));
    if (CONFD_OK != ret) {
        FATAL("Failed to cdb_connect() to ConfD\n");
        goto term;
    }

    ret = cdb_subscribe(subscr_socket, 1, folders__ns, &subscr_point,
            ROOT_PATH);
    if (CONFD_OK != ret) {
        FATAL("subscribe %d\n");
        goto term;
    }

    ret = cdb_subscribe_done(subscr_socket);
    if (CONFD_OK != ret) {
        FATAL("cdb_subscribe_done() failed");
        goto term;
    }

    TRACE("subscription point = %d\n", subscr_point);

term:
    INFO_EXIT("(%i)", ret);
    return ret;
}


int process_modifications(void)
{
    INFO_ENTER("");
    int ret = CONFD_OK;

    confd_tag_value_t *values = NULL;
    int values_cnt = 0;

    // get all the data in the subtree, including the sublists...
    int flags = CDB_GET_MODS_INCLUDE_LISTS;

    // get list of all the changes done in triggering commit
    ret = cdb_get_modifications(subscr_socket, subscr_point, flags,
            &values, &values_cnt, NULL);
    if (ret != CONFD_OK) {
        FATAL("cdb_get_modifications() failed");
        goto term;
    }

    TRACE("number of modifications: %d", values_cnt);

    // pass the changes to a custom "writer" to create a NETCONF-like
    // content for corresponding edit-config message
    char *result_str = pr_write_value_array(values, values_cnt);
    printf("modifications read by subscriber:\n%s", result_str);

    free(result_str);
    result_str = NULL;

    int i;
    for (i = 0; i < values_cnt; i++) {
        confd_free_value(CONFD_GET_TAG_VALUE(&values[i]));
    }

    if (NULL != values) {
        free(values);
        values = NULL;
        values_cnt = 0;
    }

term:
    INFO_EXIT("(%x)", ret);
    return ret;
}


int main(int argc, char *argv[])
{
    INFO_ENTER("argc %i", argc);
    int ret = CONFD_OK;

    ret = init_subscriber();
    if (CONFD_OK != ret) {
        FATAL("Failed to initialize subscriber! Exiting");
        goto term;
    }

    const int subs_cnt = cdb_active_subscriptions;

    while (1) {
        struct pollfd set[subs_cnt];
        set[0].fd = subscr_socket;
        set[0].events = POLLIN;
        set[0].revents = 0;

        if (poll(set, sizeof(set)/sizeof(*set), -1) < 0) {
            FATAL("Poll failed:");
            break;
        }

        if (!(set[0].revents & POLLIN)) {
            continue;
        }

        int sub_points[subs_cnt];
        int reslen;

        ret = cdb_read_subscription_socket(subscr_socket, &sub_points[0],
                &reslen);
        if (CONFD_OK != ret) {
            FATAL("sub_read: %d\n", ret);
            break;
        }

        if (sub_points[0] == subscr_point) {
            TRACE("subscription triggered");
            ret = process_modifications();
        }

        ret = cdb_sync_subscription_socket(subscr_socket, CDB_DONE_PRIORITY);
        if (CONFD_OK != ret) {
            FATAL("failed to sync subscription: %d\n", ret);
            break;
        }
    }

term:
    INFO_EXIT("ret %i", ret);
    return ret;
}