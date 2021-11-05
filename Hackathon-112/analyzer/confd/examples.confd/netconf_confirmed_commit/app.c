/*********************************************************************
 * ConfD netconf confirmed commit example
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
#include <stdlib.h>
#include <sys/socket.h>

#define _TRACE_DECLARE
#include <traceh.h>
#include <confd_lib.h>
#include <confd_cdb.h>

/* include generated file */
#include "host.h"

int process_subscription(const int subsock, const int spoint) {
    TRACE_ENTER("subsock=%i  spoint=%i", subsock, spoint);
    int rv = CONFD_ERR;
    char *cli_values=NULL;

    if (CONFD_OK !=
        cdb_get_modifications_cli(subsock, spoint, 0, &cli_values)) {
        FATAL("Failed to get modifications");
        goto term;
    }
    if (cli_values) {
        INFO("Printing modifications");
        printf("%s", cli_values);
        fflush(stdout);
        free(cli_values);
    } else {
        TRACE("No modifications found");
    }
    rv = CONFD_OK;

term:
    TRACE_EXIT("rv=%i", rv);
    return rv;
}

int main(int argc, char **argv)
{
    INFO_ENTER("argc=%i", argc);
    struct sockaddr_in addr;
    int subsock;
    int rv = 1;
    int spoint;

    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(CONFD_PORT);

    confd_init(argv[0], stderr, CONFD_TRACE);

    DEBUG("Setting subscriptions");
    if ((subsock = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
        FATAL("Failed to open socket");
        goto term;
    }

    if (cdb_connect(subsock, CDB_SUBSCRIPTION_SOCKET, (struct sockaddr*) &addr,
                    sizeof(struct sockaddr_in)) < 0) {
        FATAL("Failed to cdb_connect() to confd");
        goto term;
    }

    if (CONFD_OK != cdb_subscribe(subsock, 3, hst__ns, &spoint, "/hosts")) {
        FATAL("Failed to make subscription");
        goto term;
    }
    if (CONFD_OK != cdb_subscribe_done(subsock)) {
        FATAL("cdb_subscribe_done() failed");
        goto term;
    }
    DEBUG("Subscription point = %d", spoint);

    INFO("Entering ConfD poll loop");
    while (1) {
        struct pollfd set[1];

        set[0].fd = subsock;
        set[0].events = POLLIN;
        set[0].revents = 0;

        if (poll(&set[0], 1, -1) < 0) {
            FATAL("Poll failed, terminating");
            goto term;
        }

        if (set[0].revents & POLLIN) {
            int sub_points[1];
            int reslen;

            if (CONFD_OK
                != cdb_read_subscription_socket(subsock, &sub_points[0],
                                                &reslen)) {
                FATAL("Failed to read subscription socket!");
                goto term;
            }
            if (sub_points[0] == spoint) {
                INFO("Subscription for /hosts triggered");
                if (CONFD_OK != process_subscription(subsock, spoint)) {
                    FATAL("Failed to process subscriptions");
                    goto term;
                }
            } else {
                WARN("Unexpected subscription point=%d triggered, expected %d",
                     sub_points[0], spoint);
            }

            if (CONFD_OK
                != cdb_sync_subscription_socket(subsock,
                                                CDB_DONE_PRIORITY)) {
                FATAL("Failed to sync subscription socket");
                goto term;
            }
        }
    }

term:
    INFO_EXIT("rv=%i", rv);
    exit(rv);
}
