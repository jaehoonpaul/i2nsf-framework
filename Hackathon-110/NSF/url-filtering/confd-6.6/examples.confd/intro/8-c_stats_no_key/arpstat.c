/*********************************************************************
 * ConfD Stats intro example
 * Implements an operational data provider
 *
 * (C) 2005-2009 Tail-f Systems
 * Permission to use this code as a starting point hereby granted
 *
 * See the README file for more information
 ********************************************************************/

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

#ifdef __QNX__
/* too hard to figure out which #defines
   are needed for this to get included */
extern char *strdup(const char *string);
#endif

#include <confd_lib.h>
#include <confd_dp.h>
#include "arpe.h"

#if !defined(Linux) && !defined(__NetBSD__) && !defined(__FreeBSD__) && \
    !defined(__QNX__) && !defined(Darwin)
#warning "arpstat: Not tested on this OS"
#endif

/********************************************************************/

/* Our daemon context as a global variable */
static struct confd_daemon_ctx *dctx;
static int ctlsock;
static int workersock;

struct aentry {
    struct in_addr ip4;
    char *hwaddr;
    int perm;
    int pub;
    char *iface;
    struct aentry *next;
};

struct arpdata {
    struct aentry *arp_entries;
    struct timeval lastparse;
};


/********************************************************************/

/* do we need to rerun the arp command */
static int need_arp(struct arpdata *dp)
{
    struct timeval now;

    gettimeofday(&now, NULL);
    if ((dp->lastparse.tv_sec == 0) ||
        (now.tv_sec > dp->lastparse.tv_sec + 2) ||
        (dp->arp_entries == NULL))
        return 1;
    return 0;
}

static void free_arp(struct arpdata *dp)
{
    struct aentry *ae = dp->arp_entries;

    while (ae) {
        struct aentry *next = ae->next;
        if(ae->hwaddr) free(ae->hwaddr);
        if(ae->iface) free(ae->iface);
        free(ae);
        ae = next;
    }
    dp->arp_entries = NULL;
}

/* parse output fom arp -an for this transaction */
static int run_arp(struct arpdata *dp)
{
    char *sep = " ?()<>\n";
    struct aentry *ae = NULL;
    FILE *fp;
    char buf[BUFSIZ];

    free_arp(dp);

    if ((fp = popen("arp -an", "r")) == NULL)
        return CONFD_ERR;
    while (fgets(&buf[0], BUFSIZ, fp) != NULL) {
        char *cp = strtok(&buf[0], sep);

        if ((ae = (struct aentry*) malloc(sizeof(struct aentry))) == NULL)
            return CONFD_ERR;
        memset((void*)ae, 0, sizeof(struct aentry));

        /* Now lazy parse lines like */
        /* ? (192.168.1.1) at 00:0F:B5:EF:11:00 [ether] on eth0 */
        /* slightly different arp output on Linux and BSD */

        ae->ip4.s_addr = inet_addr(cp);
        /* skip "at" */
        assert(strcmp(strtok(NULL, sep), "at") == 0);
        cp = strtok(NULL, sep);

        if ((strcmp(cp, "incomplete") == 0)) {
            assert(strcmp(strtok(NULL, sep), "on") == 0);
            cp = strtok(NULL, sep);
        } else if ((strcmp(cp, "<from_interface>") == 0)) {
            cp = strtok(NULL, sep);
            while (cp) {
                if (strcmp(cp, "on") == 0) {
                    cp = strtok(NULL, sep);
                    break;
                }
                cp = strtok(NULL, sep);
            }
        } else {
            /* some common error cases handled, get real hw addr */
            ae->hwaddr = strdup(cp);

            while (1) {
                cp = strtok(NULL, sep);
                if (cp == NULL)
                    break;
                else if (strcmp(cp, "PERM") == 0)
                    ae->perm = 1;
                else if (strcmp(cp, "PUB") == 0)
                    ae->pub = 1;
                else if (strcmp(cp, "[ether]") == 0)
                    ;
                else if (strcmp(cp, "on") == 0) {
                    cp = strtok(NULL, sep);
                    break;
                }
            }
        }

        /* cp should now point to the interface name
           - this is required since it is a key */
        if (cp) {
            ae->iface = strdup(cp);

            /* Some OSes have perm/pub after interface name */
            while ((cp = strtok(NULL, sep)) != NULL) {
                if (strcmp(cp, "permanent") == 0)
                    ae->perm = 1;
                else if (strcmp(cp, "published") == 0)
                    ae->pub = 1;
            }

            ae->next = dp->arp_entries;
            dp->arp_entries = ae;
        } else {
            /* skip this entry */
            free(ae);
        }
    }
    /* remember the time when we did this */
    gettimeofday(&dp->lastparse, NULL);
    return CONFD_OK;
}

/********************************************************************/

static int s_init(struct confd_trans_ctx *tctx)
{
    struct arpdata *dp;

    if ((dp = malloc(sizeof(struct arpdata))) == NULL)
        return CONFD_ERR;
    memset(dp, 0, sizeof(struct arpdata));
    if (run_arp(dp) == CONFD_ERR) {
        free(dp);
        return CONFD_ERR;
    }
    tctx->t_opaque = dp;
    confd_trans_set_fd(tctx, workersock);
    return CONFD_OK;
}

static int s_finish(struct confd_trans_ctx *tctx)
{
    struct arpdata *dp = tctx->t_opaque;

    if (dp != NULL) {
        free_arp(dp);
        free(dp);
    }
    return CONFD_OK;
}

/********************************************************************/

static int get_next(struct confd_trans_ctx *tctx,
                         confd_hkeypath_t *keypath,
                         long next)
{
    struct arpdata *dp = tctx->t_opaque;
    struct aentry *curr;
    confd_value_t v[2];

    if (next == -1) {  /* first call */
        if (need_arp(dp)) {
            if (run_arp(dp) == CONFD_ERR)
                return CONFD_ERR;
        }
        curr = dp->arp_entries;
    } else {
        curr = (struct aentry *)next;
    }
    if (curr == NULL) {
        confd_data_reply_next_key(tctx, NULL, -1, -1);
        return CONFD_OK;
    }

    /* Since no keys are defined in the data model, we are free to
       make up our own key.  The only requirement is that we are able
       to use the key in the get_elem and exist callbacks.

       For this example, we send a pointer to the entry as a single key.
    */
    CONFD_SET_INT64(&v[0], curr);
    confd_data_reply_next_key(tctx, &v[0], 1, (long)curr->next);
    return CONFD_OK;
}


struct aentry *find_ae(confd_hkeypath_t *keypath, struct arpdata *dp)
{
    return (struct aentry *)CONFD_GET_INT64(&keypath->v[1][0]);
}

/* Keypath example */
/* /arpentries/arpe{<pointer to next entry>}/hwaddr */
/*    3         2         1                    0    */

static int get_elem(struct confd_trans_ctx *tctx,
                    confd_hkeypath_t *keypath)
{
    confd_value_t v;

    struct aentry *ae = find_ae(keypath, tctx->t_opaque);
    if (ae == NULL) {
        confd_data_reply_not_found(tctx);
        return CONFD_OK;
    }
    switch (CONFD_GET_XMLTAG(&(keypath->v[0][0]))) {
    case arpe_hwaddr:
        if (ae->hwaddr == NULL) {
            confd_data_reply_not_found(tctx);
            return CONFD_OK;
        }
        CONFD_SET_STR(&v, ae->hwaddr);
        break;
    case arpe_permanent:
        CONFD_SET_BOOL(&v, ae->perm);
        break;
    case arpe_published:
        CONFD_SET_BOOL(&v, ae->pub);
        break;
    case arpe_ip:
        CONFD_SET_IPV4(&v, ae->ip4);
        break;
    case arpe_ifname:
        CONFD_SET_STR(&v, ae->iface);
        break;
    default:
        return CONFD_ERR;
    }
    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}

/* will be called for the list entry only */
static int exists(struct confd_trans_ctx *tctx,
                  confd_hkeypath_t *keypath)
{
    struct aentry *ae = find_ae(keypath, tctx->t_opaque);
    if (ae == NULL) {
        confd_data_reply_not_found(tctx);
    } else {
        confd_data_reply_found(tctx);
    }
    return CONFD_OK;
}

/********************************************************************/

int main(int argc, char *argv[])
{
    struct sockaddr_in addr;
    int debuglevel = CONFD_TRACE;
    struct confd_trans_cbs trans;
    struct confd_data_cbs data;

    memset(&trans, 0, sizeof (struct confd_trans_cbs));
    trans.init = s_init;
    trans.finish = s_finish;

    memset(&data, 0, sizeof (struct confd_data_cbs));
    data.get_elem = get_elem;
    data.get_next = get_next;
    data.exists_optional = exists;
    strcpy(data.callpoint, arpe__callpointid_arpe2);

    /* initialize confd library */
    confd_init("arpe_daemon", stderr, debuglevel);

    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(CONFD_PORT);

    if (confd_load_schemas((struct sockaddr*)&addr,
                           sizeof (struct sockaddr_in)) != CONFD_OK)
        confd_fatal("Failed to load schemas from confd\n");

    if ((dctx = confd_init_daemon("arpe_daemon")) == NULL)
        confd_fatal("Failed to initialize confdlib\n");

    /* Create the first control socket, all requests to */
    /* create new transactions arrive here */

    if ((ctlsock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        confd_fatal("Failed to open ctlsocket\n");
    if (confd_connect(dctx, ctlsock, CONTROL_SOCKET, (struct sockaddr*)&addr,
                      sizeof (struct sockaddr_in)) < 0)
        confd_fatal("Failed to confd_connect() to confd \n");

    /* Also establish a workersocket, this is the most simple */
    /* case where we have just one ctlsock and one workersock */

    if ((workersock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        confd_fatal("Failed to open workersocket\n");
    if (confd_connect(dctx, workersock, WORKER_SOCKET,(struct sockaddr*)&addr,
                      sizeof (struct sockaddr_in)) < 0)
        confd_fatal("Failed to confd_connect() to confd \n");

    if (confd_register_trans_cb(dctx, &trans) == CONFD_ERR)
        confd_fatal("Failed to register trans cb \n");

    if (confd_register_data_cb(dctx, &data) == CONFD_ERR)
        confd_fatal("Failed to register data cb \n");

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

        if (poll(set, sizeof(set)/sizeof(*set), -1) < 0) {
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
