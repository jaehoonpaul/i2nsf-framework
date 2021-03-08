
/*
 * Copyright 2005 Tail-F Systems AB
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
#include <confd_dp.h>
#include "hst.h"
#include "dlist.h"

/* This _is_ our database */

struct iface {
    char name[64];
    struct in_addr ip;
    struct in_addr mask;
    int enabled;
};

struct host {
    char name[256];
    char domain[256];
    struct in_addr defgw;
    Dlist *ifaces;
};




/* A structure to keep tabs on how many times */
/* we access the different cb functions */
/* to swow in the CLI */

struct access_stat {
    int get_elem;
    int get_next;
    int set_elem;
    int create;
    int remove;
};

static struct access_stat hcp_calls;
static struct access_stat icp_calls;

static Dlist *running_db = NULL;
static Dlist *cand_db = NULL;


/* Our daemon context as a global variable */
static struct confd_daemon_ctx *dctx;

static struct confd_trans_cbs trans;
static struct confd_db_cbs dbcbs;
static struct confd_data_cbs host_cbks, iface_cbks;

static int read_stdin = 1;
static int candidate_modified = 0;

/* My user data, we got to install opaque data into */
/* the confd_daemon_ctx, this data is then accesible from the */
/* trans callbacks and must thus not necessarily vae to  */
/* be global data. */

struct mydata {
    int ctlsock;
    int workersock;
    enum confd_dbname lock;
};


static void *copy_struct(void *ptr, int sz)
{
    if (!ptr) return NULL;
    void *x = malloc(sz);
    memcpy(x, ptr, sz);
    return x;
}



/* free a db */
static void clear_db(Dlist *list)
{
    Dlist *hptr, *ifaces, *iptr;
    for (hptr = list->flink; hptr != list;  hptr = hptr->flink) {
        ifaces = ((struct host*) hptr->val)->ifaces;
        for (iptr = ifaces->flink; iptr != ifaces; iptr = iptr->flink) {
            free(iptr->val);
        }
        free_dlist(ifaces);
    }
    free_dlist(list);
}

static void show_host(Dlist *hptr)
{
    Dlist *iptr, *ifaces;
    struct iface *iface;
    struct host* hp = (struct host*) hptr->val;

    printf ("Host %10s %10s %10s\n",
            hp->name, hp->domain, inet_ntoa(hp->defgw));
    ifaces = ((struct host*) hptr->val)->ifaces;
    for (iptr = ifaces->flink; iptr != ifaces; iptr = iptr->flink) {
        iface = (struct iface*) iptr->val;
        char *ipstr = strdup(inet_ntoa(iface->ip));
        printf("   iface: %7s %10s %10s %4d\n",
               iface->name, ipstr,
               inet_ntoa(iface->mask), iface->enabled);
    }
}


/* Help function which allocates a new host struct */
static struct host *new_host(char *name, char *domain, char *defgw)
{
    struct host *hp;
    if ((hp = (struct host*) calloc(1, sizeof(struct host))) == NULL)
        return NULL;
    strcpy(hp->name, name);
    strcpy(hp->domain, domain);
    hp->defgw.s_addr = inet_addr(defgw);
    hp->ifaces = new_dlist();
    return hp;
}

/* Help function which adds a new host, keeping the list ordered */
static void add_host(Dlist *list, struct host *new)
{
    Dlist *ptr;
    struct host *hp;

    dl_traverse(ptr, list) {
        hp = (struct host *) ptr->val;
        if (strcmp(new->name, hp->name) < 0) {
            break;
        }
    }
    dl_insert_b(ptr, new);
}

/* Help function which allocates a new iface struct */
static struct iface *new_iface(char *name, char *ip, char *mask, char *ena)
{
    struct iface *iface;
    if ((iface = (struct iface*) calloc(1, sizeof(struct iface))) == NULL)
        return NULL;
    strcpy(iface->name, name);
    iface->ip.s_addr = inet_addr(ip);
    iface->mask.s_addr = inet_addr(mask);
    if (strcmp(ena, "1") == 0)
        iface->enabled = 1;
    return iface;
}

/* Help function which adds a new interface, keeping the list ordered */
static void add_iface(Dlist *list, struct iface *new)
{
    Dlist *ptr;
    struct iface *ifp;

    dl_traverse(ptr, list) {
        ifp = (struct iface *) ptr->val;
        if (strcmp(new->name, ifp->name) < 0) {
            break;
        }
    }
    dl_insert_b(ptr, new);
}

static void show_db(Dlist *list)
{
    Dlist *hptr;
    for (hptr = list->flink; hptr != list; hptr = hptr->flink) {
        show_host(hptr);
    }
}




static int dump_db(Dlist *list, char *filename)
{
    FILE *fp;
    Dlist *hptr, *ifaces, *iptr;
    if ((fp = fopen(filename, "w+")) == NULL)
        return -1;
    for (hptr = list->flink; hptr != list; hptr = hptr->flink) {
        struct host *hp = (struct host*) hptr->val;
        fprintf(fp, "%s %s %s { ",
                hp->name, hp->domain, inet_ntoa(hp->defgw));
        ifaces = hp->ifaces;
        for (iptr = ifaces->flink; iptr != ifaces; iptr = iptr->flink) {
            struct iface *ipt = (struct iface*) iptr->val;
            char *ipstr = strdup(inet_ntoa(ipt->ip));
            fprintf(fp, " %s %s %s %d ",
                    ipt->name, ipstr,
                    inet_ntoa(ipt->mask), ipt->enabled);
            free(ipstr);
        }
        fprintf(fp, " }\n");
    }
    fclose(fp);
    return 1;
}



static void free_ifaces(Dlist *ifaces)
{
    Dlist *iptr;
    for (iptr = ifaces->flink; iptr != ifaces; iptr = iptr->flink) {
        free(iptr->val);
    }
    free_dlist(ifaces);
}


/* Find a specific host in a specific DB */
static Dlist *find_host(Dlist *list, confd_value_t *v)
{
    Dlist *hptr;
    for (hptr = list->flink; hptr != list; hptr = hptr->flink) {
        struct host *s = (struct host*) hptr->val;
        if (confd_svcmp(s->name, v) == 0)
            return hptr;
    }
    return NULL;
}

static Dlist *find_iface(Dlist *list,
                         confd_value_t *hostname,
                         confd_value_t *ifname)
{
    Dlist *hpl = find_host(list, hostname);
    if (hpl != NULL) {
        struct host *hp = (struct host*) hpl->val;
        Dlist *iptr;
        Dlist *ifaces = hp->ifaces;
        for (iptr = ifaces->flink; iptr != ifaces; iptr = iptr->flink) {
            struct iface *iface = (struct iface*) iptr->val;
            if (confd_svcmp(iface->name, ifname) == 0)
                return iptr;
        }
    }
    return NULL;
}

static Dlist **cand_or_running(enum confd_dbname n)
{
  static enum confd_dbname last_used = -1;

    if (n == CONFD_CANDIDATE) {
      if(n != last_used)
        printf("Using Candidate DB:\n");
      last_used = n;
      return &cand_db;
    }
    if(n != last_used)
      printf("Using Running DB:\n");
    last_used = n;
    return &running_db;
}



/* transaction callbacks  */

/* The installed init() function gets called everytime Confd */
/* wants to establish a new transaction, Each NETCONF */
/* command will be a transaction */

/* We can choose to create threads here or whatever, we */
/* can choose to allocate this transaction to an already existing */
/* thread. We must tell Confd which filedescriptor should be */
/* used for all future communication in this transaction */
/* this has to be done through the call confd_trans_set_fd(); */

static int tr_init(struct confd_trans_ctx *tctx)
{
    char buf[INET6_ADDRSTRLEN];
    inet_ntop(tctx->uinfo->af, &tctx->uinfo->ip, buf, sizeof(buf));
    struct mydata *md = (struct mydata*) tctx->dx->d_opaque;
    confd_trans_set_fd(tctx, md->workersock);
    return CONFD_OK;
}


/* This callback gets invoked at the end of the transaction */
/* when ConfD has accumulated all write operations */
/* we're guaranteed that */
/* a) no more read ops will occur */
/* b) no other transactions will run between here and tr_finish() */
/*    for this transaction, i.e ConfD will serialize all transactions */

/* since we need to be prepared for abort(), we may not write */
/* our data to the actual database, we can choose to either */
/* copy the entire database here and write to the copy in the */
/* following write operatons _or_ let the write operations */
/* accumulate operations create(), set(), delete() instead of actually */
/* writing */

/* If our db supports transactions (which it doesn't in this */
/* silly example, this is the place to do START TRANSACTION */

static int tr_writestart(struct confd_trans_ctx *tctx)
{
    return CONFD_OK;
}


static int tr_prepare(struct confd_trans_ctx *tctx)
{
    return CONFD_OK;
}

static int tr_commit(struct confd_trans_ctx *tctx)
{
    struct confd_tr_item *item = tctx->accumulated;
    struct host *hp;
    Dlist *dlist;
    Dlist *which_db = *(cand_or_running(tctx->dbname));

    while (item) {
        confd_hkeypath_t *keypath = item->hkp;
        confd_value_t *leaf = &(keypath->v[0][0]);
        if (strcmp(item->callpoint, "hcp") == 0) {
            switch (item->op) {
            case C_SET_ELEM:
                /* we're setting the elem of an already existing */
                /* host entry */
                /* keypath example: /hosts/host{hname}/defgw */
                if ((dlist = find_host(which_db,
                                       &(keypath->v[1][0]))) != NULL){
                    hp = (struct host*) dlist->val;
                    switch (CONFD_GET_XMLTAG(leaf)) {
                    case hst_domain:
                        strcpy(hp->domain, (char*) CONFD_GET_BUFPTR(item->val));
                        break;
                    case hst_defgw:
                        hp->defgw = CONFD_GET_IPV4(item->val);
                        break;
                    default:
                        break;
                    }
                }
                break;
            case C_CREATE:
                /* we're creating a brand new host entry */
                /* it will soon be populated with values */
                /* keypath example: /hosts/host{hname}   */

                hp = (struct host*) calloc(1, sizeof(struct host));
                strcpy(hp->name, (char *)CONFD_GET_BUFPTR(leaf));
                hp->ifaces = new_dlist();
                add_host(which_db, hp);
                break;
            case C_REMOVE:
                if ((dlist = find_host(which_db, leaf)) != NULL) {
                    hp = (struct host*) dlist->val;
                    free_ifaces(hp->ifaces);
                    free(hp);
                    dl_delete_node(dlist);
                }
                break;
            default:
                return CONFD_ERR;
            }
        }
        else  if (strcmp(item->callpoint, "icp") == 0) {
            struct iface *iface;
            switch (item->op) {
            case C_SET_ELEM:
                /* we're setting an item in an already existing interface*/
                /* keypath ex:  */
                /* /hosts/host{hname}/interfaces/interface{eth0}/ip */
                if ((dlist = find_iface(which_db,
                                        &(keypath->v[4][0]),
                                        &(keypath->v[1][0]))) != NULL) {
                    iface = (struct iface*) dlist->val;
                    switch (CONFD_GET_XMLTAG(leaf)) {
                    case hst_ip:
                        iface->ip = CONFD_GET_IPV4(item->val);
                        break;
                    case hst_mask:
                        iface->mask = CONFD_GET_IPV4(item->val);
                        break;
                    case hst_enabled:
                        iface->enabled = CONFD_GET_BOOL(item->val);
                        break;
                    }
                }
                break;
            case C_CREATE:
                /* we're creating a brand new new interface */
                /* keypath example is */
                /* /hosts/host{hname}/interfaces/interface{eth0} */
                if ((dlist = find_host(which_db, &(keypath->v[3][0])))
                    != NULL) {
                    hp = (struct host*) dlist->val;
                    iface = (struct iface*) calloc(1, sizeof(struct iface));
                    strcpy(iface->name, (char *)CONFD_GET_BUFPTR(leaf));
                    add_iface(hp->ifaces, iface);
                }
                break;
            case C_REMOVE:
                /* we're deleting an interface */
                /* keypath example */
                /* /hosts/host{hname}/interfaces/interface{eth0} */
                if ((dlist = find_iface(which_db,
                                        &(keypath->v[3][0]), leaf)) != NULL) {
                    free(dlist->val);
                    dl_delete_node(dlist);
                }
                break;
            default:
                return CONFD_ERR;
            }
        }
        item = item->next;
    }
    return CONFD_OK;
}


static int tr_abort(struct confd_trans_ctx *tctx)
{
    return CONFD_OK;
}

static int tr_finish(struct confd_trans_ctx *tctx)
{
    return CONFD_OK;
}




/* help function which restores a DB from a FILE* */
/* also used by the "load" cmd in the CLI */


static Dlist *restore(char *fname)
{
    char buf[BUFSIZ];
    char *sep = " \r\n";
    FILE *fp;
    if ((fp  = fopen(fname, "r")) == NULL)
        return NULL;
    Dlist *list = new_dlist();

    while (fgets(&buf[0], BUFSIZ, fp) != NULL) {
        char *name, *domain, *defgw;
        char *ip, *mask, *enabled;
        if ((name = strtok(buf, sep)) != NULL &&
            ((domain = strtok(NULL, sep)) != NULL) &&
            ((defgw = strtok(NULL, sep)) != NULL)) {
            struct host *hp = new_host(name, domain, defgw);

            /* eath the curly brace */
            assert(strcmp(strtok(NULL, sep), "{") == 0);
            while (((name =    strtok(NULL, sep)) != NULL) &&
                   ((ip =      strtok(NULL, sep)) != NULL) &&
                   ((mask =    strtok(NULL, sep)) != NULL) &&
                   ((enabled = strtok(NULL, sep)) != NULL)) {
                struct iface *iface = new_iface(name, ip,mask,enabled);
                dl_append(hp->ifaces, iface);
            }
            dl_append(list, hp);
        }
    }
    fclose(fp);
    return list;
}



/********************************************************************/
/* DB callbacks */

static int lock(struct confd_db_ctx *dbx, enum confd_dbname dbname)
{
    struct mydata *md = (struct mydata*) dbx->dx->d_opaque;
    md->lock = dbname;
    return CONFD_OK;
}


static int unlock(struct confd_db_ctx *dbx, enum confd_dbname dbname)
{
    struct mydata *md = (struct mydata*) dbx->dx->d_opaque;
    md->lock = 0;
    return CONFD_OK;
}

static int delete_config(struct confd_db_ctx *dbx,
                         enum confd_dbname dbname)
{

    Dlist **db_ptr = cand_or_running(dbname);
    clear_db(*db_ptr);
    *db_ptr = new_dlist();
    return CONFD_OK;
}


static int add_checkpoint_running(struct confd_db_ctx *dbx)
{
    if (dump_db(running_db, "RUNNING.ckp") < 0)
        return CONFD_ERR;
    return CONFD_OK;
}

static int del_checkpoint_running(struct confd_db_ctx *dbx)
{
    unlink("RUNNING.ckp");
    return CONFD_OK;
}

static int activate_checkpoint_running(struct confd_db_ctx *dbx)
{
    Dlist *list;
    if ((list = restore("RUNNING.ckp")) == NULL)
        return CONFD_ERR;
    unlink("RUNNING.ckp");
    clear_db(running_db);
    running_db = list;
    return CONFD_OK;
}

static int has_checkpoint_running()
{
    return access("RUNNING.ckp", F_OK) == 0;
}

/* has anything been written into the candidate that hasn't yet */
/* been copied into running trough either an invocation of      */
/* candidate_commit() callback or invocation in our own CLI of */
/* copytorunning command */


static int candidate_chk_not_modified(struct confd_db_ctx *dbx)
{
    if (candidate_modified == 1) return CONFD_ERR;
    return CONFD_OK;
}

/* copy candidate into running, slow but correct */
static void cand_copy()
{
    unlink("tmp_db");
    dump_db(cand_db, "tmp_db");
    clear_db(running_db);
    running_db = cand_db;  /* swap */
    cand_db = restore("tmp_db");
    unlink("tmp_db");
    candidate_modified = 0;
}


static int candidate_commit(struct confd_db_ctx *dbx, int timeout)
{
    if (timeout != 0 && !has_checkpoint_running()) {
        /* we must be prepared to rollback running */
        if (add_checkpoint_running(dbx) == CONFD_ERR)
            return CONFD_ERR;
    }
    /* now copy candidate to running */
    cand_copy();
    return CONFD_OK;
}


static int candidate_confirming_commit(struct confd_db_ctx *dbx)
{
    del_checkpoint_running(dbx);
    return CONFD_OK;
}

static int candidate_rollback_running(struct confd_db_ctx *dbx)
{
    return activate_checkpoint_running(dbx);
}

static int candidate_reset(struct confd_db_ctx *dbx)
{
    unlink("tmp_db");
    dump_db(running_db, "tmp_db");
    clear_db(cand_db);
    cand_db = running_db;
    running_db = restore("tmp_db");
    return CONFD_OK;
}

static int candidate_validate(struct confd_db_ctx *dbx)
{
    return CONFD_OK;
}


static int reteof(struct confd_trans_ctx *tctx)
{
    confd_data_reply_next_key(tctx, NULL, -1, -1);
    return CONFD_OK;
}

static int host_get_next(struct confd_trans_ctx *tctx,
                         confd_hkeypath_t *keypath,
                         long next)
{
    confd_value_t v;
    Dlist *list;
    struct host *hp;
    Dlist *which_db = *(cand_or_running(tctx->dbname));

    hcp_calls.get_next++;
    if (next == -1 && !dl_empty(which_db)) {    /* Get first key */
        Dlist *first = dl_first(which_db);
        hp = (struct host*) first->val;
        CONFD_SET_STR(&v, hp->name);
        /* Use  real ptr as next  */
        confd_data_reply_next_key(tctx, &v, 1, (long) dl_next(first));
        return CONFD_OK;
    }
    if (next == -1) {  /* First key from empty DB, */
        return reteof(tctx);
    }
    else {
        if ((list = (Dlist*) next) == which_db) {
            /* we went all the way around */
            return reteof(tctx);
        }
        hp = (struct host*) list->val;
        CONFD_SET_STR(&v, hp->name);
        /* Use  real ptr as next  */
        confd_data_reply_next_key(tctx, &v, 1, (long) dl_next(list));
        return CONFD_OK;
    }
}

/* keypath here will look like */
/* /hosts/host{myhostname}/interfaces/interface */

static int iface_get_next(struct confd_trans_ctx *tctx,
                         confd_hkeypath_t *keypath,
                         long next)
{
    struct iface *iface;
    confd_value_t v;
    Dlist *which_db = *(cand_or_running(tctx->dbname));

    if (next == -1) { /* first call */
        Dlist *hlink = find_host(which_db, &(keypath->v[2][0]));
        struct host *hp;
        if (hlink == NULL)
            return reteof(tctx);
        hp = (struct host*)  hlink->val;
        if (dl_empty(hp->ifaces))
            return reteof(tctx);
        /* here we utilize the t_opaque pointer in the */
        /* transaction context and store the original list head */
        /* pointer so that we know when we've traversed all keys */
        Dlist *first = dl_first(hp->ifaces);
        tctx->t_opaque = hp->ifaces;
        iface = (struct iface*) first->val;
        CONFD_SET_STR(&v, iface->name);
        confd_data_reply_next_key(tctx, &v, 1, (long) dl_next(first));
        return CONFD_OK;
    }
    else {
        Dlist *ilink;
        if ((ilink = (Dlist*) next) == tctx->t_opaque) {
            /* we went all the way around */
            return reteof(tctx);
        }
        iface = (struct iface*) ilink->val;
        CONFD_SET_STR(&v, iface->name);
        confd_data_reply_next_key(tctx, &v, 1, (long) ilink->flink);
        return CONFD_OK;
    }
}

static int host_get_elem(struct confd_trans_ctx *tctx,
                         confd_hkeypath_t *keypath)
{
    confd_value_t v;
    struct host *hp;
    Dlist *which_db = *(cand_or_running(tctx->dbname));
    Dlist *list = find_host(which_db, &(keypath->v[1][0]));

    hcp_calls.get_elem++;

    if (list ==  NULL) {
        confd_data_reply_not_found(tctx);
        return CONFD_OK;
    }
    hp = (struct host*) list->val;
    /* switch on xml elem tag */
    switch (CONFD_GET_XMLTAG(&(keypath->v[0][0]))) {
    case hst_name:
        CONFD_SET_STR(&v, hp->name);
        break;
    case hst_domain:
        CONFD_SET_STR(&v, hp->domain);
        break;
    case hst_defgw:
        CONFD_SET_IPV4(&v, hp->defgw);
        break;
    default:
        fprintf(stderr,"HERE %d\n", CONFD_GET_XMLTAG(&(keypath->v[0][0])));
        return CONFD_ERR;
    }
    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}



/* assuming the name of the host being configured is "earth"     */
/* the keypaths we get here will be like :                       */
/* /hosts/host{earth}/interfaces/interface{eth0}/ip              */
/*   [6]  [5]   [4]     [3]        [2]     [1]   [0]             */
/* thus keypath->v[4][0] will refer to the name of the           */
/* host being configured                                         */
/* and  keypath->v[1][0] will refer to the name of the interface */
/* being configured                                              */


static int iface_get_elem(struct confd_trans_ctx *tctx,
                          confd_hkeypath_t *keypath)
{
    confd_value_t v;
    Dlist *list;
    struct iface *iface;
    Dlist *which_db = *(cand_or_running(tctx->dbname));

    if ((list = find_iface(which_db, &(keypath->v[4][0]),
                           &(keypath->v[1][0]))) == NULL) {
        confd_data_reply_not_found(tctx);
        return CONFD_OK;
    }
    iface = (struct iface*) list->val;

    switch (CONFD_GET_XMLTAG(&(keypath->v[0][0]))) {
    case hst_name:
        CONFD_SET_STR(&v, iface->name);
        break;
    case hst_ip:
        CONFD_SET_IPV4(&v, iface->ip);
        break;
    case hst_mask:
        CONFD_SET_IPV4(&v, iface->mask);
        break;
    case hst_enabled:
        CONFD_SET_BOOL(&v, iface->enabled);
        break;
    default:
        return CONFD_ERR;
    }
    confd_data_reply_value(tctx, &v);
    return CONFD_OK;
}



static int host_set_elem(struct confd_trans_ctx *tctx,
                         confd_hkeypath_t *keypath,
                         confd_value_t *newval)
{
    hcp_calls.set_elem++;
    return CONFD_ACCUMULATE;
}
static int host_create(struct confd_trans_ctx *tctx,
                       confd_hkeypath_t *keypath)
{
    hcp_calls.create++;
    return CONFD_ACCUMULATE;
}

static int host_delete(struct confd_trans_ctx *tctx,
                       confd_hkeypath_t *keypath)
{
    hcp_calls.remove++;
    return CONFD_ACCUMULATE;
}


static int iface_set_elem(struct confd_trans_ctx *tctx,
                          confd_hkeypath_t *keypath,
                          confd_value_t *newval)
{
    icp_calls.set_elem++;
    return CONFD_ACCUMULATE;
}
static int iface_create(struct confd_trans_ctx *tctx,
                       confd_hkeypath_t *keypath)
{
    icp_calls.create++;
    return CONFD_ACCUMULATE;
}

static int iface_delete(struct confd_trans_ctx *tctx,
                       confd_hkeypath_t *keypath)
{
    icp_calls.remove++;
    return CONFD_ACCUMULATE;
}



static void init_cli()
{
    printf("> "); fflush(stdout);
    return;
}


/* Initialize db to 2 hosts */
static Dlist *default_db()
{
    struct host *buzz, *earth;
    Dlist *list;
    list = new_dlist();
    buzz = new_host("buzz", "tail-f.com", "192.168.1.1");
    earth = new_host("earth", "tailf-com", "192.168.1.1");
    dl_append(list, buzz);
    dl_append(list, earth);

    dl_append(buzz->ifaces, new_iface("eth0", "192.168.1.61",
                                      "255.255.255.0", "1"));
    dl_append(buzz->ifaces, new_iface("eth1", "10.77.1.44",
                                      "255.255.0.0", "0"));

    dl_append(buzz->ifaces, new_iface("lo", "127.0.0.1",
                                      "255.0.0.0", "1"));


    dl_append(earth->ifaces, new_iface("bge0", "192.168.1.61",
                                       "255.255.255.0", "1"));
    dl_append(earth->ifaces, new_iface("lo0", "127.0.0.1",
                                       "255.0.0.0", "1"));
    return list;
}

/* debug func */
static void ccc()
{
    dump_db(running_db, "/tmp/ccc");
}


/* Ultra primitive CLI which manipulates our "database" */
/* this is thus a way to manipulate the DB, both running and the candidate */
/* behind the back of ConfD */

static void handle_stdin()
{

    char *tok;
    char line[BUFSIZ];
    int rval;
    static char currhost[255];
    char currprompt[255] = "> ";
    Dlist *hostlink;
    struct host *hp;
    confd_value_t v;
    char *sep = " \r\n";
    static Dlist *curr_db;
    static enum confd_dbname dbname  = CONFD_RUNNING;

    curr_db = (dbname == CONFD_RUNNING ? running_db : cand_db);
    if ((rval = read(0, line, BUFSIZ)) <= 0) {
        fprintf(stderr, "EOF\n");
        read_stdin = 0;
        return;
    }
    line[rval] = 0;

    if ((tok = strtok(line, sep)) == NULL) {
        printf("[%s] %s",
               dbname == CONFD_RUNNING ? "running" : "cand",
               &currprompt[0]); fflush(stdout);
        return;
    }

    /* choose db */
    if (strcmp(tok, "db") == 0) {
        char *dbstr = strtok(NULL, sep);

        if (dbstr && strcmp(dbstr, "running") == 0) {
            curr_db = running_db;
            dbname = CONFD_RUNNING;
        }
        else if (dbstr && strcmp(dbstr, "candidate") == 0) {
            curr_db = cand_db;
            dbname = CONFD_CANDIDATE;
        }
        else if (dbname == CONFD_RUNNING) {
            printf ("Using the running DB\n");
        }
        else {
            printf ("Using the candidate DB\n");
        }
    }

    /* Show all database */
    else if (strcmp(tok,"show") == 0) {
        if (currhost[0] == 0) {
            show_db(curr_db);
        }
        else {
            CONFD_SET_STR(&v, currhost);
            if ((hostlink = find_host(curr_db, &v)) != NULL) {
                show_host(hostlink);
            }
        }
    }
    else if (strcmp (tok, "host") == 0) {
        if ((tok = strtok(NULL, sep)) == NULL) {
            printf("usage: host <hname> | host <hname domain defgw>\n");
            currhost[0] = 0;
            return;
        }
        CONFD_SET_STR(&v,tok);

        if ((hostlink = find_host(curr_db, &v)) == NULL) {
            /* create a new host */
            char *name, *domain, *defgw;
            name = tok;
            if (
                ((domain = strtok(NULL, sep)) != NULL) &&
                ((defgw =  strtok(NULL, sep)) != NULL)) {
                add_host(curr_db, new_host(name, domain, defgw));
                strcpy(&currhost[0], name);
            }
            else {
                printf("usage: host <newhost> <domain> <defgw>\n");
                currhost[0] = 0;
            }
        }
        else {
            struct host *hp = (struct host*) hostlink->val;
            strcpy(&currhost[0], hp->name);
        }
        if (dbname == CONFD_CANDIDATE) candidate_modified = 1;
    }
    else if (strcmp(tok, "iface") == 0) {
        if (currhost[0] == 0) {
            printf("Need to pick a host before we can create iface\n");
        }
        else {
            CONFD_SET_STR(&v, currhost);
            if ((hostlink = find_host(curr_db, &v)) == NULL)
                return;

            char *name, *ip, *mask, *ena;
            if (
                ((name = strtok(NULL, sep)) != NULL) &&
                ((ip = strtok(NULL, sep)) != NULL) &&
                ((mask = strtok(NULL, sep)) != NULL) &&
                ((ena = strtok(NULL, sep)) != NULL)
                ) {
                struct iface *iface = new_iface(name, ip, mask, ena);
                struct host *hp = (struct host*) hostlink->val;
                add_iface(hp->ifaces, iface);
                if (dbname == CONFD_CANDIDATE) candidate_modified = 1;
            }
            else {
                printf("usage: iface <name> <ip> <mask> <ena>\n");
            }
        }
    }


    else if (strcmp(tok, "del") == 0) {
        if (currhost[0] == 0) {
            /* we're deleting a host */
            if ((tok = strtok(NULL, sep)) == NULL) {
                printf("usage: del <hname | ifname>\n");
            }
            else {
                CONFD_SET_STR(&v,tok);
                if ((hostlink = find_host(curr_db, &v)) != NULL) {
                    hp = (struct host*) hostlink->val;
                    free_ifaces(hp->ifaces);
                    free(hp);
                    dl_delete_node(hostlink);
                    if (dbname == CONFD_CANDIDATE) candidate_modified = 1;
                }
            }
        }
        else {
            /* we have a current host choosen, we're deleting an interface */

            if ((tok = strtok(NULL, sep)) == NULL) {
                printf("usage: del <hname | ifname>\n");
            }
            confd_value_t h;
            Dlist *iflink;

            CONFD_SET_STR(&v, currhost);
            if ((hostlink = find_host(curr_db, &v)) == NULL)
                return;

            CONFD_SET_STR(&h, ((struct host*)hostlink->val)->name);
            CONFD_SET_STR(&v,tok);
            if ((iflink = find_iface(curr_db, &h, &v)) != NULL) {
                free(iflink->val);
                dl_delete_node(iflink);
                if (dbname == CONFD_CANDIDATE) candidate_modified = 1;
            }
            else {
                printf("No such interface <%s> \n", tok);
            }
        }
    }
    else if (strcmp(tok, "up") == 0) {
        currhost[0] = 0;
    }
    else if (strcmp(tok,"quit") == 0) {
        exit(0);
    }
    else if (strcmp(tok,"default") == 0) {
        clear_db(running_db);
        running_db = default_db();
        clear_db(cand_db);
        cand_db = default_db();
        curr_db = running_db;
        dbname = CONFD_RUNNING;
    }
    else if (strcmp(tok,"load") == 0) {
        char *fname = strtok(NULL, sep);
        if (!fname) {
            printf("usage: load <file>\n");
            return;
        }
        if ((hostlink = restore(fname)) == NULL) {
            printf("failed to open %s for reading \n", fname);
            return;
        }
        if (dbname == CONFD_RUNNING) {
            clear_db(running_db);
            running_db = hostlink;
        }
        else {
            clear_db(cand_db);
            cand_db = hostlink;
            if (dbname == CONFD_CANDIDATE) candidate_modified = 1;

        }
    }
    else if (strcmp(tok,"dump") == 0) {
        char *fname = strtok(NULL, sep);
        if (!fname) {
            if (dbname == CONFD_RUNNING)
                fname = "RUNNING.db";
            else
                fname = "CANDIDATE.db";
        }
        if (dump_db(curr_db, fname) < 0) {
            printf("failed to dump to %s \n", fname);
            return;
        }
        printf("dumped to %s\n", fname);
    }
    else if (strcmp(tok,"copytorunning") == 0) {
        cand_copy();
    }

    else {
        printf (
            "show \n"
            "db   [running | candidate]      - set which db we work against\n"
            "host [hostname]\n"
            "host <name> <domain> <defgw>    - to create new host\n"
            "iface <name> <ip> <mask> <ena>  - to create new iface\n"
            "del <hostname | ifacename>\n"
            "up \n"
            "quit \n"
            "default      -  to load default db values\n"
            "load <file>  -  to load db from <file> \n"
            "dump <file>  -  to dump db to <file> \n"
            );
    }
    if (currhost[0] != 0) {
        confd_value_t v;
        CONFD_SET_STR(&v, &currhost[0]);

        if ((hostlink = find_host(curr_db, &v)) == NULL) {
            strcpy(currprompt, "> ");
            currhost[0] = 0;
        }
        else {
            hp = (struct host*) hostlink->val;
            sprintf(&currprompt[0], "[%s] > ", hp->name);
        }
    }
    else {
        strcpy(currprompt, "> ");
    }

    printf("[%s] %s",
           dbname == CONFD_RUNNING ? "running" : "cand",
           &currprompt[0]);
    fflush(stdout);
    return;
}

int main(int argc, char *argv[]) {
    int ctlsock;
    int workersock;
    struct sockaddr_in addr;
    struct mydata *md;
    int debuglevel = CONFD_TRACE;
    Dlist *list;
    int oc;             /* option character */

    while ((oc = getopt(argc, argv, "qdtpci")) != -1) {
        switch (oc) {
        case 'q':
            debuglevel = CONFD_SILENT;
            break;
        case 'd':
            debuglevel = CONFD_DEBUG;
            break;
        case 't':
            debuglevel = CONFD_TRACE;
            break;
        case 'p':
            debuglevel = CONFD_PROTO_TRACE;
            break;
        case 'i':
            read_stdin = 0;
            break;
        default:
            fprintf(stderr, "usage: hosts [-qdtpi]\n");
            exit(1);
        }
    }


    /* These are our transaction callbacks */
    trans.init = tr_init;
    trans.prepare = tr_prepare;
    trans.commit = tr_commit;
    trans.abort = tr_abort;
    trans.finish = tr_finish;


    /* And these are our candidate and db callbacks */
    dbcbs.lock = lock;
    dbcbs.unlock = unlock;
    dbcbs.delete_config = delete_config;
    /* next three are only called when ConfD owns the candidate */
    dbcbs.add_checkpoint_running = add_checkpoint_running;
    dbcbs.del_checkpoint_running = del_checkpoint_running;
    dbcbs.activate_checkpoint_running = activate_checkpoint_running;

    dbcbs.candidate_commit = candidate_commit;
    dbcbs.candidate_confirming_commit = candidate_confirming_commit;
    dbcbs.candidate_rollback_running = candidate_rollback_running;
    dbcbs.candidate_reset = candidate_reset;
    dbcbs.candidate_chk_not_modified = candidate_chk_not_modified;
    dbcbs.candidate_validate = candidate_validate;

    /* And finallly these are our read/write callbacks for  */
    /* the database */
    host_cbks.exists_optional = NULL;
    host_cbks.get_elem = host_get_elem;
    host_cbks.get_next = host_get_next;
    host_cbks.set_elem = host_set_elem;
    host_cbks.create   = host_create;
    host_cbks.remove   = host_delete;
    strcpy(host_cbks.callpoint, "hcp");


    iface_cbks.exists_optional = NULL;
    iface_cbks.get_elem = iface_get_elem;
    iface_cbks.get_next = iface_get_next;
    iface_cbks.set_elem = iface_set_elem;
    iface_cbks.create   = iface_create;
    iface_cbks.remove   = iface_delete;
    strcpy(iface_cbks.callpoint, "icp");

    /* Init library  */
    confd_init("hosts_daemon", stderr, debuglevel);

    /* Initialize our simple database  */
    if ((list = restore("RUNNING.ckp")) != NULL) {
        printf("Restoring running from checkpoint\n");
        running_db = list;
    }
    else if ((list = restore("RUNNING.db")) != NULL) {
        printf("Restoring from RUNNING.db\n");
        running_db = list;
    }
    else {
        printf("Starting with empty running DB\n");
        running_db = new_dlist();
    }
    unlink("RUNNING.ckp");

    /* Initialize candidate */
    if ((list = restore("CANDIDATE.db")) != NULL) {
        printf("Restoring from CANDIDATE.db\n");
        cand_db = list;
    }
    else {
        printf("Starting with empty candidate DB\n");
        cand_db = new_dlist();
    }


    /* Initialize daemon context */
    if ((dctx = confd_init_daemon("hosts_daemon")) == NULL)
        confd_fatal("Failed to initialize confd\n");

    if ((ctlsock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        confd_fatal("Failed to open ctlsocket\n");

    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(CONFD_PORT);

    if (confd_load_schemas((struct sockaddr*)&addr,
                           sizeof (struct sockaddr_in)) != CONFD_OK)
        confd_fatal("Failed to load schemas from confd\n");

    /* Create the first control socket, all requests to */
    /* create new transactions arrive here */

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


    /* Create a user datastructure and connect it to the */
    /* daemon struct so that we can always get to it */
    if ((md = dctx->d_opaque = (struct mydata*)
         calloc(1, sizeof(struct mydata))) == NULL)
        confd_fatal("Failed to malloc");
    md->ctlsock = ctlsock;
    md->workersock = workersock;


    confd_register_trans_cb(dctx, &trans);
    confd_register_db_cb(dctx, &dbcbs);

    /* we also need to register our read/write callbacks */

    if (confd_register_data_cb(dctx, &host_cbks) == CONFD_ERR)
        confd_fatal("Failed to register host cb \n");
    if (confd_register_data_cb(dctx, &iface_cbks) == CONFD_ERR)
        confd_fatal("Failed to register iface cb \n");

    if (confd_register_done(dctx) != CONFD_OK)
        confd_fatal("Failed to complete registration \n");

    /* First prompt */
    init_cli();

    while (1) {
        struct pollfd set[3];
        int ret;

        set[0].fd = ctlsock;
        set[0].events = POLLIN;
        set[0].revents = 0;

        set[1].fd = workersock;
        set[1].events = POLLIN;
        set[1].revents = 0;

        set[2].fd = 0;
        set[2].events = POLLIN;
        set[2].revents = 0;

        if (poll(&set[0], read_stdin ? 3 : 2, -1) < 0) {
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
        if (read_stdin && set[2].revents & POLLIN) {
            handle_stdin();
        }
    }
}
