/*
 * Copyright 2014 Tail-F Systems AB Tail-F customers are permitted to
 * redistribute in binary form, with or without modification, for use
 * in customer products.
 */

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/signal.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <confd.h>
#include <confd_maapi.h>
#include <confd_cdb.h>

#include "ietf-system.h"
#include "linuxcfg_api.h"
#include "linuxcfg_util.h"

#include "dns.h"

/* Which file to update */
#define DNS_RESOLV_CONF "/etc/resolv.conf"

/*
 * Current DNS configuration, this is what we will write on the next
 * update.
 */

#define SEARCH_SIZE 1024
static char search[SEARCH_SIZE] = "";

static int timeout     = 0;
static int attempts    = 0;

/* Flag that signifies pending changes to dns. */
static int dns_changes = 0;

#define MAX_ADDR_LEN 128
static char **dns_servers = NULL;
static int num_dns_servers = 0;

/* Prepare dns_servers to hold n servers. */
static void prepare_dns_servers(int n) {
    int ix;

    /* Free old memory */
    for (ix=0; ix < num_dns_servers; ix++) {
        XFREE(dns_servers[ix]);
    }
    XFREE(dns_servers);

    /* Allocate new memory */
    dns_servers     = calloc(n, sizeof(char *));
    num_dns_servers = n;

}

/* Handles an update to system/dns-resolver */
enum confd_iter_op dns_handle(confd_hkeypath_t *kp,
                              confd_value_t *val,
                              enum cdb_iter_op op, int sock)
{
    int tag, len, i;
    confd_value_t addr;

    LOG("%s.", KPSTR(kp));

    /* No action for dns-resolver/ itself, only for sub-elements. */
    if (kp->len == 0)
        return ITER_RECURSE;

    dns_changes = 1;

    tag = CONFD_GET_XMLTAG(&kp->v[kp->len-1][0]);

    if (tag == sys_search) {
        if (op == MOP_DELETED) {
            search[0] = '\0';
        } else {
            LOG("search: %s => %s.", search, VALSTR(val));

            confd_pp_value(search, SEARCH_SIZE, val);
        }
    }

    if (tag == sys_options) {
        kp->len = kp->len - 1;

        tag = CONFD_GET_XMLTAG(&kp->v[kp->len-1][0]);

        /* options/timeout */
        if (tag == sys_timeout) {
            timeout = CONFD_GET_UINT8(val);
        }

        /* options/attempts */
        if (tag == sys_attempts) {
            attempts = CONFD_GET_UINT8(val);
        }
    }

    /* server/ is a list of dns servers, we iterate through it. */
    if (tag == sys_server) {
        len = cdb_num_instances(sock, "system/dns-resolver/server");

        LOG("num inst: %d.", len);

        prepare_dns_servers(len);

        for(i = 0; i < len; i++) {
            CHECK3(cdb_pushd(sock,
                             "system/dns-resolver/server[%d]", i),
                   "Pushd to system/dns-resolver/server failed",
                   ITER_STOP);

            CHECK3(cdb_get(sock, &addr,
                          "udp-and-tcp/address"),
                   "Failed to get address.", ITER_STOP);

            dns_servers[i] = malloc(MAX_ADDR_LEN);
            confd_pp_value(dns_servers[i], MAX_ADDR_LEN, &addr);

            LOG("adding: %s.", dns_servers[i]);

            cdb_popd(sock);
        }

        /* We only need to do this once, regardless of how many
         * changes were made to the list. */
        return ITER_UP;
    }

    return ITER_CONTINUE;
}

static int write_resolvconf(void) {
    FILE *fp = fopen(DNS_RESOLV_CONF, "w");
    int   ix;

    if (fp == NULL) {
        error("Failed to open %s, errorcode: %d",
              DNS_RESOLV_CONF, errno);
        return CONFD_ERR;
    }

    if (search[0] != '\0') {
        fprintf(fp, "search %s\n",search);
    }

    fprintf(fp, "options timeout:%d\n", timeout);
    fprintf(fp, "options attempts:%d\n\n", attempts);

    for (ix = 0; ix < num_dns_servers; ix++) {
        fprintf(fp, "nameserver %s\n", dns_servers[ix]);
    }

    fclose(fp);
    return CONFD_OK;
}


int dns_finish(void) {
    int rv = CONFD_OK;

    if (dns_changes) {
        rv = write_resolvconf();
    }

    dns_changes = 0;
    return rv;
}
