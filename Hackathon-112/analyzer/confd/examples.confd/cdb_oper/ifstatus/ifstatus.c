/*
 * Copyright 2006 Tail-F Systems AB
 */


#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <confd_lib.h>
#include <confd_cdb.h>
#include "if.h"

#define INTERVAL 10

#define GET_COUNTER() {                         \
        if ((p = strtok(NULL, " \t")) == NULL)  \
            continue;                           \
        counter = atoll(p);                     \
    }

static int update_status(int sock)
{
    FILE *proc;
    int ret;
    char buf[BUFSIZ];
    char *ifname, *p;
    long long counter;
    confd_value_t val[1 + 4 + 1 + 5];
    int i;

    if ((ret = cdb_start_session(sock, CDB_OPERATIONAL)) != CONFD_OK)
        return ret;
    if ((ret = cdb_set_namespace(sock, if__ns)) != CONFD_OK)
        return ret;

    if ((proc = fopen("/proc/net/dev", "r")) == NULL)
        return CONFD_ERR;
    while (ret == CONFD_OK && fgets(buf, sizeof(buf), proc) != NULL) {
        if ((p = strchr(buf, ':')) == NULL)
            continue;
        *p = ' ';
        if ((ifname = strtok(buf, " \t")) == NULL)
            continue;

        i = 0;

        CONFD_SET_XMLTAG(&val[i], if_receive, if__ns); i++;
        GET_COUNTER();          /* rx bytes */
        CONFD_SET_UINT64(&val[i], counter); i++;
        GET_COUNTER();          /* rx packets */
        CONFD_SET_UINT64(&val[i], counter); i++;
        GET_COUNTER();          /* rx errs */
        CONFD_SET_UINT32(&val[i], counter); i++;
        GET_COUNTER();          /* rx drop  */
        CONFD_SET_UINT32(&val[i], counter); i++;
        /* skip remaining rx counters */
        GET_COUNTER(); GET_COUNTER(); GET_COUNTER(); GET_COUNTER();

        CONFD_SET_XMLTAG(&val[i], if_transmit, if__ns); i++;
        GET_COUNTER();          /* tx bytes */
        CONFD_SET_UINT64(&val[i], counter); i++;
        GET_COUNTER();          /* tx packets */
        CONFD_SET_UINT64(&val[i], counter); i++;
        GET_COUNTER();          /* tx errs */
        CONFD_SET_UINT32(&val[i], counter); i++;
        GET_COUNTER();          /* tx drop  */
        CONFD_SET_UINT32(&val[i], counter); i++;
        GET_COUNTER();          /* skip */
        GET_COUNTER();          /* tx colls */
        CONFD_SET_UINT32(&val[i], counter); i++;

        ret = cdb_set_object(sock, val, i,
                             "/interfaces/interface{%s}/status", ifname);
        if (ret == CONFD_ERR && confd_errno == CONFD_ERR_BADPATH)
            /* assume interface doesn't exist in config */
            ret = CONFD_OK;
    }
    fclose(proc);

    cdb_end_session(sock);

    return ret;
}

int main(int argc, char **argv)
{
    int interval = 0;
    struct sockaddr_in addr;
    int sock;

    if (argc > 1)
        interval = atoi(argv[1]);
    if (interval == 0)
        interval = INTERVAL;

    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(CONFD_PORT);

    confd_init(argv[0], stderr, CONFD_SILENT);
    if (confd_load_schemas((struct sockaddr*)&addr,
                           sizeof (struct sockaddr_in)) != CONFD_OK)
        confd_fatal("%s: Failed to load schemas from confd\n", argv[0]);
    if ((sock = socket(PF_INET, SOCK_STREAM, 0)) < 0)
        confd_fatal("%s: Failed to create socket", argv[0]);
    if (cdb_connect(sock, CDB_DATA_SOCKET, (struct sockaddr *)&addr,
                    sizeof(struct sockaddr_in)) < 0)
        confd_fatal("%s: Failed to connect to ConfD", argv[0]);

    while (1) {
        if (update_status(sock) != CONFD_OK)
            confd_fatal("%s: Failed to update status\n", argv[0]);
        sleep(interval);
    }
}
