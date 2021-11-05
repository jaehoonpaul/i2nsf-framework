/*
 * Copyright 2005-2008 Tail-F Systems AB
 */


#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/poll.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <confd_lib.h>
#include <confd_cdb.h>
#include "smp.h"


struct server {
    char name[256];
    struct in_addr ip;
    unsigned int port;
};
static struct server running_db[256];
static int num_servers = 0;

static int read_conf(struct sockaddr_in *addr)
{
    int rsock, i, n, st = CONFD_OK;
    struct in_addr ip;
    u_int16_t port;
    char buf[BUFSIZ];

    if ((rsock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        return CONFD_ERR;
    if (cdb_connect(rsock, CDB_READ_SOCKET, (struct sockaddr*)addr,
                    sizeof (struct sockaddr_in)) < 0)
        return CONFD_ERR;
    if (cdb_start_session(rsock, CDB_RUNNING) != CONFD_OK)
        return CONFD_ERR;
    cdb_set_namespace(rsock, smp__ns);
    num_servers = 0;
    if ((n = cdb_num_instances(rsock, "/servers/server")) < 0) {
        cdb_end_session(rsock);
        cdb_close(rsock);
        return n;
    }

    for(i=0; i<n; i++) {
        char tmppath[400];
        sprintf(tmppath, "/servers/server[%d]/name", i);
        if ((st = cdb_get_str(rsock, buf, BUFSIZ, tmppath)) != CONFD_OK)
            break;
        sprintf(tmppath, "/servers/server[%d]/ip", i);
        if ((st = cdb_get_ipv4(rsock, &ip, tmppath))!= CONFD_OK)
            break;
        sprintf(tmppath, "/servers/server[%d]/port", i);
        if ((st = cdb_get_u_int16(rsock, &port, tmppath)) != CONFD_OK)
            break;
        strcpy(running_db[num_servers].name, buf);
        running_db[num_servers].ip.s_addr = ip.s_addr;
        running_db[num_servers].port = port;
        ++num_servers;
    }
    cdb_end_session(rsock),
    cdb_close(rsock);
    return st;
}

static void display_config()
{
    /* This function is called initially and whenever there has been  */
    /* a configuration chage. In this simple example, we just dump it */
    /* on stdout */

    int i;

    printf("----- Working with these settings now -----\n");
    for(i=0; i<num_servers; i++) {
        u_int8_t *ip = (u_int8_t *)
            &running_db[i].ip.s_addr; /* this is in NBO */
        printf("%2d: %20s %d.%d.%d.%d:%d\n", i, running_db[i].name,
               ip[0],ip[1],ip[2],ip[3], running_db[i].port);
    }
    printf("-------------------------------------------\n");
}

int main(int argc, char **argv)
{
    struct sockaddr_in addr;
    int subsock;
    int status;
    int spoint;

    setvbuf(stdout, NULL, _IOLBF, BUFSIZ);
    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(CONFD_PORT);

    confd_init(argv[0], stderr, CONFD_TRACE);

    if ((subsock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        confd_fatal("Failed to open socket\n");
    if (cdb_connect(subsock, CDB_SUBSCRIPTION_SOCKET, (struct sockaddr*)&addr,
                    sizeof (struct sockaddr_in)) < 0)
        confd_fatal("Failed to confd_connect() to confd \n");
    if ((status = cdb_subscribe(subsock, 3, smp__ns, &spoint,"/servers"))
        != CONFD_OK) {
        fprintf(stderr, "Terminate: subscribe %d\n", status);
        exit(0);
    }
    if (cdb_subscribe_done(subsock) != CONFD_OK)
        confd_fatal("cdb_subscribe_done() failed");

    printf("Subscription point = %d\n", spoint);

    if ((status = read_conf(&addr)) != CONFD_OK) {
        fprintf(stderr, "Terminate: read_conf %d\n", status);
        exit(0);
    }

    display_config();

    /* Poll loop */

    while (1) {
        struct pollfd set[1];

        set[0].fd = subsock;
        set[0].events = POLLIN;
        set[0].revents = 0;


        if (poll(set, sizeof(set)/sizeof(*set), -1) < 0) {
            perror("Poll failed:");
            continue;
        }

        /* Check for I/O */
        if (set[0].revents & POLLIN) {
            int sub_points[1];
            int reslen;
            if ((status = cdb_read_subscription_socket(subsock,
                                                       &sub_points[0],
                                                       &reslen)) != CONFD_OK)
                exit(status);
            if (reslen > 0) {
                if ((status = read_conf(&addr)) != CONFD_OK)
                    exit(0);
            }
            /* This is the place where we may act on the newly read configuration */
            display_config();

            if ((status = cdb_sync_subscription_socket(subsock, CDB_DONE_PRIORITY))
                != CONFD_OK) {
                exit(status);
            }
        }
    }
}
