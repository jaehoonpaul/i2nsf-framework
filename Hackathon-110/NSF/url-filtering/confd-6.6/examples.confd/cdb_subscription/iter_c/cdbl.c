/*
 * Copyright 2007 Tail-F Systems AB
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
#include <stdio.h>
#include <signal.h>
#include <errno.h>


#include <confd_lib.h>
#include <confd_cdb.h>
#include "root.h"


static int signaled = 0;

static void sighdlr(int sig)
{
    signaled++;
}


/* my internal db */

#define MAXH 3
#define MAXC 2

struct child {
    int64_t dn;
    char childattr[255];
    int inuse;
};

struct rfhead {
    int64_t dn;
    char sector_id[255];
    struct child children[MAXC];
    int inuse;
};
struct rfhead rfheads[MAXH];


/* read a given head structure from cdb and write the data into */
/* our array */

static void read_head(int cdbsock, confd_value_t *headkey)
{

    int i = 0;
    int pos = -1;
    /* which position should we overwrite */
    for (i=0; i< MAXH; i++) {
        if (CONFD_GET_INT64(headkey) == rfheads[i].dn) {
            pos = i;
            break;
        }
    }
    if (pos == -1) { /* pick first */
        for (i=0; i< MAXH; i++) {
            if (!rfheads[i].inuse) {
                pos = i;
                break;
            }
        }
    }
    fprintf(stderr, "Picking %d\n", pos);
    struct rfhead *hp = &rfheads[pos];
    if (cdb_cd(cdbsock, "/root/NodeB/RFHead{%x}", headkey) != CONFD_OK)
        confd_fatal("Failed to cd");
    hp->dn = CONFD_GET_INT64(headkey);
    hp->inuse = 1;
    if (cdb_get_str(cdbsock, hp->sector_id, 244, "SECTORID_ID") !=
        CONFD_OK)
        confd_fatal("Failed to get val");
    int n = cdb_num_instances(cdbsock, "Child");
    for(i=0; i<MAXC; i++) hp->children[i].inuse = 0;
    for(i=0; i<n; i++) {
        if (cdb_get_int64(cdbsock, &hp->children[i].dn,
                          "Child[%d]/cdn", i) != CONFD_OK)
            confd_fatal("Failed to get val");
        if (cdb_get_str(cdbsock, hp->children[i].childattr, 255,
                        "Child[%d]/childAttr", i) != CONFD_OK)
            confd_fatal("Failed to get val");
        hp->children[i].inuse = 1;
    }
}


/* read the entire db */

static void read_db(int cdbsock)
{
    int ret, i;
    confd_value_t key;

    if ((ret = cdb_start_session(cdbsock, CDB_RUNNING)) != CONFD_OK)
        confd_fatal("Cannot start session\n");
    if ((ret = cdb_set_namespace(cdbsock, root__ns)) != CONFD_OK)
        confd_fatal("Cnnot set namespace\n");

    int n = cdb_num_instances(cdbsock, "/root/NodeB/RFHead");
    for (i=0; i<MAXH; i++) rfheads[i].inuse = 0;
    for(i=0; i<n; i++) {
        if (cdb_get(cdbsock, &key, "/root/NodeB/RFHead[%d]/dn", i) !=
            CONFD_OK) confd_fatal("Can't get key");
        read_head(cdbsock, &key);
    }
    cdb_end_session(cdbsock);

}

static enum cdb_iter_ret iter(confd_hkeypath_t *kp,
                              enum cdb_iter_op op,
                              confd_value_t *oldv,
                              confd_value_t *newv,
                              void *state)
{
    char buf[BUFSIZ];
    int cdbsock = *((int *)state);
    confd_pp_kpath(buf, BUFSIZ, kp);
    switch (op) {
        case MOP_CREATED: {
            fprintf(stderr, "Create: %s\n", buf);
            confd_value_t *ctag = &kp->v[1][0];
            switch (CONFD_GET_XMLTAG(ctag)) {
                case root_RFHead:  { /* an rfhead was created */
                    /* keypath is /root/NodeB/RFHead{$key}  */
                    /*              3     2      1    0     */

                    confd_value_t *hkey = &kp->v[0][0];
                    read_head(cdbsock, hkey);
                    return ITER_CONTINUE;
                }
                case root_Child: {
                    /* a child to en existing rfhead was created */
                    /* keypath is /root/NodeB/RFHead{$key}/Child{$key2}  */
                    /*              5      4      3    2      1    0     */
                    /* we can here choose to read the  new child or reread */
                    /* the entire head structure                           */
                    confd_value_t *hkey = &kp->v[2][0];
                    read_head(cdbsock, hkey);
                    return ITER_CONTINUE;
                }
            }
            break;
        }


        case MOP_DELETED:
            fprintf(stderr, "Delete: %s\n", buf);
            confd_value_t *dtag = &kp->v[1][0];
            switch (CONFD_GET_XMLTAG(dtag)) {
                case root_RFHead:  { /* an rfhead was deleted */
                    /* keypath is /root/NodeB/RFHead{$key}  */
                    /*              3     2      1    0     */
                    confd_value_t *headkey = &kp->v[0][0];
                    /* Now given the key here, identifying an rfhead */
                    /* we find our rfhead and delete it */
                    int headpos = 0;
                    while (headpos < MAXH) {
                        struct rfhead *hp = &rfheads[headpos++];
                        if (CONFD_GET_INT64(headkey) == hp->dn) {
                            /* we found the head to remove */
                            /* mark it as not used; */
                            int i;
                            for (i=0; i<MAXC; i++)
                                hp->children[i].inuse = 0;
                            hp->inuse = 0;
                            return ITER_CONTINUE;
                        }
                    }
                    break;
                }

                case root_Child: {
                    /* a child of an existing head was removed */
                    /* keypath is /root/NodeB/RFHead{$key}/Child{$key2}  */
                    /*              5      4      3    2      1    0     */
                    /* we can here choose to read the  new child or reread */
                    /* the entire head structure                           */
                    confd_value_t *headkey = &kp->v[2][0];
                    confd_value_t *childkey = &kp->v[0][0];
                    /* Now given the key here, identifying an rfhead */
                    /* we find the rfhead which contains our child */
                    int headpos = 0;
                    while (headpos < MAXH) {
                        struct rfhead *hp = &rfheads[headpos++];
                        if (CONFD_GET_INT64(headkey) == hp->dn) {
                            int cpos = 0;
                            while (cpos < MAXC) {
                                struct child *cp = &hp->children[cpos++];
                                if (CONFD_GET_INT64(childkey) ==  cp->dn) {
                                    cp->inuse = 0;
                                    return ITER_CONTINUE;
                                }
                            }
                        }
                    }
                }
            }
            break;
        case MOP_MODIFIED:
            fprintf(stderr, "Modified %s\n", buf);
            break;
        case MOP_VALUE_SET: {
            char nbuf[BUFSIZ];
            confd_pp_value(nbuf, BUFSIZ, newv);
            fprintf(stderr, "Value Set: %s --> (%s)\n",
                    buf, nbuf);
            confd_value_t *leaf = &kp->v[0][0];
            switch (CONFD_GET_XMLTAG(leaf)) {
                case root_SECTORID_ID:   {
                    /* keypath is /root/NodeB/RFHead{$key}/SECTORID_ID  */
                    /*              4     3      2     1       0        */
                    confd_value_t *headkey = &kp->v[1][0];
                    int headpos = 0;
                    while (headpos < MAXH) {
                        struct rfhead *hp = &rfheads[headpos++];
                        if (CONFD_GET_INT64(headkey) == hp->dn) {
                            /* copy in the new value */
                            memcpy(hp->sector_id, CONFD_GET_BUFPTR(newv),
                                   CONFD_GET_BUFSIZE(newv));
                            hp->sector_id[CONFD_GET_BUFSIZE(newv)] = 0;
                            return ITER_RECURSE;
                            /* are our children alsomodified */
                        }
                    }
                }
                    break;
                case root_childAttr: {
             /* keypath is /root/NodeB/RFHead{$key}/Child{$key2}/childAttr  */
             /*              6      5      4    3      2     1       0      */

                    confd_value_t *headkey = &kp->v[3][0];
                    confd_value_t *childkey = &kp->v[1][0];
                    /* Now given the key here, identifying an rfhead */
                    /* we find the rfhead which contains our child */
                    int headpos = 0;
                    while (headpos < MAXH) {
                        struct rfhead *hp = &rfheads[headpos++];
                        if (CONFD_GET_INT64(headkey) == hp->dn) {
                            int cpos = 0;
                            while (cpos < MAXC) {
                                struct child *cp = &hp->children[cpos++];
                                if (CONFD_GET_INT64(childkey) ==  cp->dn) {
                                    memcpy(cp->childattr,
                                           CONFD_GET_BUFPTR(newv),
                                           CONFD_GET_BUFSIZE(newv));
                                    cp->childattr[CONFD_GET_BUFSIZE(newv)] = 0;
                                    return ITER_CONTINUE;
                                }
                            }
                        }
                    }
                }
            }
            break;
        }
        default:
            /* We should never get MOP_MOVED_AFTER or MOP_ATTR_SET */
            fprintf(stderr, "Unexpected op %d for %s\n", op, buf);
            break;
    }
    return ITER_RECURSE;
}


static void dump_db()
{
    int i, j;
    fprintf(stderr, "\nDumping \n");
    for (i=0; i< MAXH; i++) {
        if (!rfheads[i].inuse) continue;
        fprintf(stderr, "HEAD %d  <%s>\n", (int)rfheads[i].dn,
                rfheads[i].sector_id);
        for (j=0; j<MAXC; j++) {
            if (!rfheads[i].children[j].inuse)
                continue;
            fprintf(stderr, "   Child %d  <<%s>>\n",
                    (int)rfheads[i].children[j].dn,
                    rfheads[i].children[j].childattr
                );
        }
    }
    fprintf(stderr, "---------- \n");
}


int main(int argc, char **argv)
{
    struct sockaddr_in addr;
    int c, status, subsock, sock;
    int headpoint;
    enum confd_debug_level dbgl = CONFD_SILENT;
    char *confd_addr = "127.0.0.1";
    int confd_port = CONFD_PORT;

    while ((c = getopt(argc, argv, "dta:p:")) != EOF) {
        switch (c) {
        case 'd': dbgl = CONFD_DEBUG; break;
        case 't': dbgl = CONFD_TRACE; break;
        case 'a': confd_addr = optarg; break;
        case 'p': confd_port = atoi(optarg); break;
        }
    }

    addr.sin_addr.s_addr = inet_addr(confd_addr);
    addr.sin_family = AF_INET;
    addr.sin_port = htons(confd_port);

    confd_init(argv[0], stderr, dbgl);

    if ((sock = socket(PF_INET, SOCK_STREAM, 0)) < 0)
        confd_fatal("%s: Failed to create socket", argv[0]);

    if (confd_load_schemas((struct sockaddr*)&addr,
                           sizeof (struct sockaddr_in)) != CONFD_OK)
        confd_fatal("%s: Failed to load schemas from confd\n", argv[0]);

    if (cdb_connect(sock, CDB_DATA_SOCKET, (struct sockaddr *)&addr,
                    sizeof(struct sockaddr_in)) != CONFD_OK)
        confd_fatal("%s: Failed to connect to ConfD", argv[0]);

    if ((subsock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        confd_fatal("Failed to open socket\n");

    if (cdb_connect(subsock, CDB_SUBSCRIPTION_SOCKET, (struct sockaddr*)&addr,
                      sizeof (struct sockaddr_in)) < 0)
        confd_fatal("Failed to cdb_connect() to confd \n");

    /* setup subscription point */

    if ((status = cdb_subscribe(subsock, 3, root__ns, &headpoint,
                                "/root/NodeB/RFHead"))
        != CONFD_OK) {
        confd_fatal("Terminate: subscribe %d\n", status);
    }
    if (cdb_subscribe_done(subsock) != CONFD_OK)
        confd_fatal("cdb_subscribe_done() failed");

    /* initialize db */
    read_db(sock);
    dump_db();

    /* "interactive" feature, catch SIGINT and dump db to stderr */
    signal(SIGINT, sighdlr);

    while (1) {
        int status;
        struct pollfd set[1];

        set[0].fd = subsock;
        set[0].events = POLLIN;
        set[0].revents = 0;

        if (poll(&set[0], sizeof(set)/sizeof(*set), -1) < 0) {
            if (errno != EINTR) {
                perror("Poll failed:");
                continue;
            }
        }

        if (set[0].revents & POLLIN) {
            int sub_points[1];
            int reslen;

            if ((status = cdb_read_subscription_socket(subsock,
                                                       &sub_points[0],
                                                       &reslen)) != CONFD_OK) {
                confd_fatal("terminate sub_read: %d\n", status);
            }
            if (reslen > 0) {
                fprintf(stderr, "*** Config updated \n");

                if ((status = cdb_start_session(sock,CDB_RUNNING)) != CONFD_OK)
                    confd_fatal("Cannot start session\n");
                if ((status = cdb_set_namespace(sock, root__ns)) != CONFD_OK)
                    confd_fatal("Cannot set namespace\n");

                cdb_diff_iterate(subsock, sub_points[0], iter,
                                 ITER_WANT_PREV, (void*)&sock);
                cdb_end_session(sock);


                /* Here is an alternative approach to checking a subtree */
                /* the function below will invoke cdb_diff_iterate */
                /* and check if any changes have beem made in the tagpath */
                /* described by tags[] */
                /* This still only applies to the subscription point which */
                /* is being used */


                struct xml_tag tags[] = {{root_root, root__ns},
                                         {root_NodeB, root__ns},
                                         {root_RFHead, root__ns},
                                         {root_Child, root__ns}};
                int tagslen = sizeof(tags)/sizeof(tags[0]);
                /* /root/NodeB/RFHead/Child */
                int retv = cdb_diff_match(subsock, sub_points[0],
                                          tags, tagslen);
                fprintf(stderr, "Diff match: %s\n", retv ? "yes" : "no");
            }

            if ((status = cdb_sync_subscription_socket(subsock,
                                                       CDB_DONE_PRIORITY))
                != CONFD_OK) {
                confd_fatal("failed to sync subscription: %d\n", status);
            }
            dump_db();
        }

        if (signaled) {  /* dump db to stderr when user hits Ctrl-C */
            dump_db();
            signaled = 0;
        }

    }
}

