/*
 * Copyright 2017 Tail-F Systems AB
 * Tail-F customers are permitted to redistribute in binary form, with
 * or without modification, for use in customer products.
 */

#include <confd.h>
#include <confd_cdb.h>

#include <stdlib.h>
#include <string.h>

#include "ietf-routing.h"

#include "ietf_routing_system.h"
#include "ietf_routing_utils.h"

#include "linuxcfg_api.h"

#define MALLOC_FAILED "malloc failed!"

#define MAX_BUFF_SIZE 4096

#define ROUTE_PATH "/routing/control-plane-protocols" \
                   "/control-plane-protocol{%x %x}" \
                   "/static-routes/%s/route{%x}/next-hop"

static const size_t nh4list_size = sizeof(struct ipv4_nexthop_list);
static const size_t nh6list_size = sizeof(struct ipv6_nexthop_list);
static const size_t nh4_size = sizeof(struct ipv4_nexthop);
static const size_t nh6_size = sizeof(struct ipv6_nexthop);

// debug trace the confd value
static void trace_val(char * msg, confd_value_t * v)
{
    char buff[MAX_BUFF_SIZE];
    confd_pp_value(buff, MAX_BUFF_SIZE, v);
    LOG("%s: %s", msg, buff);
}

// entry identifying a change to specific route record, with all the keys
// required to uniquely identify the route instance
struct key_entry {
    confd_value_t prot_type;
    confd_value_t prot_name;
    confd_value_t dest_prefix;
    struct key_entry * next;
};

// cleanup the values inside entry
static void clean_key_entry(struct key_entry * ke)
{
    if (NULL != ke) {
        confd_free_value(&ke->prot_type);
        confd_free_value(&ke->prot_name);
        confd_free_value(&ke->dest_prefix);
    }
}

// debug trace the key entry
static void trace_key_entry(char * msg, struct key_entry * ke)
{
    if (NULL == ke) {
        LOG("NULL");
    } else {
        LOG("%s (%p)", msg, ke);
        trace_val("\ttype", &ke->prot_type);
        trace_val("\tname", &ke->prot_name);
        trace_val("\tprefix", &ke->dest_prefix);
    }
}

// debug trace the array of key entries
static void trace_key_entries(struct key_entry * ke, char * msg)
{
    LOG("%s", msg);

    if (NULL == ke) {
        return;
    }

    struct key_entry * p = ke;
    while (NULL != p) {
        trace_key_entry("", p);
        p = p->next;
    }
}

// return 1 if two key entries "match" - their route keys are equal, 0 otherwise
static int key_entries_match(struct key_entry * a, struct key_entry * b)
{
    if (NULL == a && NULL == b) {
        return 1;
    }

    if ((NULL == a && NULL != b) || (NULL != a && NULL == b)) {
        return 0;

    }

    if ((0 == confd_val_eq(&a->prot_type, &b->prot_type))
        || (0 == confd_val_eq(&a->prot_name, &b->prot_name))
        || (0 == confd_val_eq(&a->dest_prefix, &b->dest_prefix))
            ) {
        return 0;
    }

    return 1;
}

// fill the key_entry data from the input keypath
static int key_entry_from_kp(confd_hkeypath_t * kp, struct key_entry *ke)
{
    CONFD_SET_NOEXISTS(&ke->prot_type);
    CONFD_SET_NOEXISTS(&ke->prot_name);
    CONFD_SET_NOEXISTS(&ke->dest_prefix);
    ke->next = NULL;

    if (NULL == kp || kp->len < 8) {
        return 0;
    }

    int kp_prot_index = kp->len - 4;
    int kp_pref_index = kp->len - 8;

    confd_value_dup_to(&kp->v[kp_prot_index][0], &ke->prot_type);
    confd_value_dup_to(&kp->v[kp_prot_index][1], &ke->prot_name);
    confd_value_dup_to(&kp->v[kp_pref_index][0], &ke->dest_prefix);
    return 1;
}

// duplicate key_entry data between source and destination
static int key_entry_clone(struct key_entry * src, struct key_entry * dst)
{
    if (NULL == src || NULL == dst) {
        return 0;
    }

    confd_value_dup_to(&src->prot_type, &dst->prot_type);
    confd_value_dup_to(&src->prot_name, &dst->prot_name);
    confd_value_dup_to(&src->dest_prefix, &dst->dest_prefix);
    dst->next = src->next;

    return 1;
}

// state data carried over the cdb_diff_iterate() procedure
struct iter_data {
    struct key_entry * deletes;
    struct key_entry * updates;
};

// clean all the state data
static void iter_data_clean(struct iter_data * data)
{
    if (NULL == data) {
        return;
    }
    struct key_entry * p, * n;
    if (NULL != data->deletes) {
        p = data->deletes;
        while (NULL != p) {
            n = p->next;
            clean_key_entry(p);
            free(p);
            p = n;
        }
        data->deletes = NULL;
    }
    if (NULL != data->updates) {
        p = data->updates;
        while (NULL != p) {
            n = p->next;
            clean_key_entry(p);
            free(p);
            p = n;
        }
        data->updates = NULL;
    }
}

// helper for building CDB path
// return ipv4 string be default (def != 0), ipv6 otherwise
static char * get_ipv_str(int def)
{
   return (def) ? "ipv4" : "ipv6";
}

// fill in the "key_entry" structure with data from input keypath
// return 1 on successful job, 0 if keypath does not include route key info
static int route_key_from_val(confd_value_t * val, struct route_key * key)
{
    memset(key, 0x00, sizeof(struct route_key));

    if (NULL == val
        || (C_IPV4PREFIX != val->type && C_IPV6PREFIX != val->type)
    ) {
        error("Invalid route key source data!");
        return 0;
    }

    if (C_IPV4PREFIX == val->type) {
        struct confd_ipv4_prefix pref4 = CONFD_GET_IPV4PREFIX(val);
        key->afi = ipv4;
        key->addr.ipv4_addr = pref4.ip;
        key->prefix_len = pref4.len;
    } else {
        struct confd_ipv6_prefix pref6 = CONFD_GET_IPV6PREFIX(val);
        key->afi = ipv6;
        key->addr.ipv6_addr = pref6.ip6;
        key->prefix_len = pref6.len;
    }

    return 1;
}

// extract the key_entry struct from the input keypath;
// if successful, return the pointer to new struct prepended to the "entries";
// BEWARE - do NOT do "entries = prepend_key_from_kp(entries, kp)" !
// (mem leak in case of failure)
struct key_entry * prepend_key_entry_from_kp(
    struct key_entry * entries,
    confd_hkeypath_t * kp
) {
    TRACE_ENTRY("");
    struct key_entry kp_ke;

    if (!key_entry_from_kp(kp, &kp_ke)) {
        error("Failed to get key entry from keypath!");
        return NULL;
    }

    struct key_entry * ret_ke = NULL;

    if (key_entries_match(entries, &kp_ke)) {
        // "same" key entry already on the list head, do not add duplicate...
        // just return input as the output...
        ret_ke = entries;
    } else {
        // add new key entry to the head of list
        ret_ke = malloc(sizeof(struct key_entry));
        if (NULL == ret_ke) {
            error("%s", MALLOC_FAILED);
            ret_ke = NULL;
            goto term;
        }
        key_entry_clone(&kp_ke, ret_ke);
        ret_ke->next = entries;
    }

term:
    clean_key_entry(&kp_ke);
    TRACE_EXIT("(%p)", ret_ke);
    return ret_ke;
}

// helper - check whether input keypath points to a route list entry
static int is_route_entry_kp(confd_hkeypath_t * kp)
{
    int ret = 0;

    if (NULL != kp && kp->len > 2) {
        // TODO - check whether other combos possible?
        uint32_t list_tag = CONFD_GET_XMLTAG(&kp->v[1][0]);
        ret = (rt_route == list_tag);
    }

    return ret;
}

// iterator procedure for cdb_diff_iterate()
enum cdb_iter_ret routing_iterator(
    confd_hkeypath_t * kp,
    enum cdb_iter_op op,
    confd_value_t * oldv,
    confd_value_t * newv,
    void * initstate
) {
    int ret = ITER_RECURSE;

    LOG("OP == %d; PATH == %s", op, kpath2str(kp));

    struct iter_data * data = (struct iter_data *) initstate;

    // skip processing of anything other than the sub-tree:
    // "/routing/control-plane-protocols"
    if (!check_kp_tag_pos(kp, rt_routing, kp->len-1)
        || !check_kp_tag_pos(kp, rt_control_plane_protocols, kp->len-2))
    {
        ret = ITER_CONTINUE;
        goto term;
    }

    const struct confd_identityref idr_static = {
        .id = rt_static,
        .ns = rt__ns
    };

    confd_value_t v_static;
    CONFD_SET_IDENTITYREF(&v_static, idr_static);

    int static_route = ((NULL != kp) && (kp->len >= 4)
                        && (0 < confd_val_eq(&kp->v[kp->len - 4][0], &v_static))
                       );
    if (!static_route) {
        LOG("not a static route, skipping...");
        ret = ITER_CONTINUE;
        goto term;
    }

    int kplen = kp->len;

    switch (op) {
        case MOP_CREATED:
        case MOP_MODIFIED:
            // process changes to "control-plane-protocol" list differently
            if (kplen < 7) {
                // TODO - add as necessary for the target system...
                warn("Implementation for protocol changes not implemented!");
            }
            else {
                if (C_XMLTAG == kp->v[0][0].type) {
                    error("CREATE/MODIFY of the leaf!");
                    ret = ITER_STOP;
                    goto term;
                }
                struct key_entry * new_ke = prepend_key_entry_from_kp(
                                                data->updates, kp);
                if (NULL != new_ke) {
                    data->updates = new_ke;
                } else {
                    error("Failed to prepend new key entry!");
                    ret = ITER_STOP;
                }
                // nothing to be done - not interested in internal subtree
                // changes at this point
            }
            break;

        case MOP_DELETED:
            if (C_XMLTAG == kp->v[0][0].type) {
                // TODO - to be implemented - some group delete - need to delete
                // all the routes of specific protocol...
                // OR this is delete of the LEAF only - safely ignore
                // (MOP_MODIFIED must have been called prior to this one)
            } else {
                // regular route delete vs a modification of existing route
                // (e.g. when deleting some internal lists ~ "next-hop")
                int do_delete = is_route_entry_kp(kp);

                struct key_entry * ptr = NULL;
                ptr = (do_delete) ? data->deletes : data->updates;

                struct key_entry * new_ke = prepend_key_entry_from_kp(ptr, kp);
                if (NULL == new_ke) {
                    // TODO do some CLI error stuff..
                    ret = ITER_STOP;
                } else {
                    if (do_delete) {
                        data->deletes = new_ke;
                    } else {
                        data->updates = new_ke;
                    }
                }
            }
            break;

        case MOP_VALUE_SET:
            // nothing to do here;
            // we'll read full info of modified route later from CDB
            break;

        case MOP_MOVED_AFTER:
            // nothing to do here;
            // ordering of records does not have effect on the routing
            break;

        default:
            error("Unexpected CDB iter operation! (%d)", op);
            ret = ITER_STOP;
    }

term:
    return ret;
}

// read the single confd_value_t from CDB using the formatted path and arguments
static int get_cdb_elem(int rsock, confd_value_t * output,
                                const char * fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    TRACE_ENTRY("%s", fmt);

    int ret = CONFD_OK;
    ret = cdb_vget(rsock, output, fmt, args);

    if (CONFD_OK != ret && CONFD_ERR_NOEXISTS == confd_errno) {
        ret = CONFD_OK;
        CONFD_SET_NOEXISTS(output);
    }

    va_end(args);

    TRACE_EXIT("(%d)", ret);
    return ret;
}

// main CDB subscriber execution of the route deletions
static int process_deletes(struct key_entry * entries)
{
    TRACE_ENTRY("");
    int ret = CONFD_OK;

    if (NULL == entries) {
        goto term_nocdb;
    }

    int pre_sock = start_read_old_session();
    if (-1 == pre_sock) {
        error("Failed to open pre-commit CDB session!");
        goto term_nocdb;
    }

   struct key_entry * ptr = entries;

    while (NULL != ptr) {
        trace_key_entry("processing DELETE of entry", ptr);

        confd_value_t v;
        ret = get_cdb_elem(pre_sock, &v, ROUTE_PATH "/special-next-hop",
                            &ptr->prot_type, &ptr->prot_name,
                            get_ipv_str(C_IPV4PREFIX == ptr->dest_prefix.type),
                            &ptr->dest_prefix);

        enum special_nexthop spec_nh;

        if (CONFD_OK != ret) {
            error("Failed to retrieve special-next-hop from pre-commit CDB!");
            goto term;
        } else if (C_NOEXISTS == v.type) {
            spec_nh = none;
        } else {
            spec_nh = spec_nexthop_hash_to_system(CONFD_GET_ENUM_VALUE(&v));
        }
        LOG("special-next-hop system val == %d", spec_nh);

        struct route_key key;
        route_key_from_val(&ptr->dest_prefix, &key);
        trace_route_key(&key);

        enum routing_ret_val sys_ret = delete_route(&key, spec_nh);
        if (ok != sys_ret) {
            error("Failed to delete route!");
            ret = CONFD_ERR;
            break;
        }

        ptr = ptr->next;
    }

term:
    cdb_close(pre_sock);

term_nocdb:
    TRACE_EXIT("(%d)", ret);
    return ret;
}

// fill the nexthop.val data with the interface/address info
static void fill_ipv_nexthop(
    enum route_afi afi,
    void * output,
    confd_value_t * v_iface,
    confd_value_t * v_addr
) {
    TRACE_ENTRY("(%d)", afi);
    trace_val("interface", v_iface);
    trace_val("address", v_addr);

    int has_iface = (NULL != v_iface && C_NOEXISTS != v_iface->type);
    int has_addr  = (NULL != v_addr && C_NOEXISTS != v_addr->type);

    char * if_buff = NULL;
    if (has_iface) {
        int if_len = CONFD_GET_BUFSIZE(v_iface);
        if_buff = malloc(if_len+1); // + one null terminator
        memcpy(if_buff, CONFD_GET_CBUFPTR(v_iface), if_len);
        if_buff[if_len] = 0;
    }

    if (ipv4 == afi) {
        struct ipv4_nexthop out4;
        memset(&out4, 0x00, sizeof(struct ipv4_nexthop));
        if (has_iface) {
            out4.iface = if_buff;
        }
        if (has_addr) {
            out4.has_addr = 1;
            out4.addr =  CONFD_GET_IPV4(v_addr);
        }
        memcpy(output, &out4, sizeof(struct ipv4_nexthop));
    } else {
        struct ipv6_nexthop out6;
        memset(&out6, 0x00, sizeof(struct ipv6_nexthop));
        if (has_iface) {
            out6.iface = if_buff;
        }
        if (has_addr) {
            out6.has_addr = 1;
            out6.addr = CONFD_GET_IPV6(v_addr);
        }
        memcpy(output, &out6, sizeof(struct ipv6_nexthop));
    }
    TRACE_EXIT("");
}

// get the specific nexthop.val data with info from CDB
static int get_cdb_ipv_nexthop(
    int rsock,
    char * cdb_path,
    enum route_afi afi,
    void * output
) {
    TRACE_ENTRY("afi == %d;\n%s", afi, cdb_path);
    int ret = CONFD_OK;

    confd_value_t v_iface, v_addr;
    CONFD_SET_NOEXISTS(&v_iface);
    CONFD_SET_NOEXISTS(&v_addr);

    ret = cdb_cd(rsock, cdb_path);
    if (CONFD_OK != ret) {
        error("Failed to access CDB path!\n%s", cdb_path);
        goto term;
    }

    ret = get_cdb_elem(rsock, &v_iface, "outgoing-interface");
    if (CONFD_OK != ret) {
        error("reading %s not ok!", "outgoing-interface");
        goto term;
    }

    ret = get_cdb_elem(rsock, &v_addr, "next-hop-address");
    if (CONFD_OK != ret) {
        error("reading %s not ok!", "next-hop-address");
        goto term;
    }

    fill_ipv_nexthop(afi, output, &v_iface, &v_addr);

term:
    confd_free_value(&v_iface);
    confd_free_value(&v_addr);

    TRACE_EXIT("(%d)", ret);
    return ret;
}

// get the nexthop-list data from CDB for specific route
static int get_cdb_nexthop_list(int rsock, struct key_entry * ke,
                                union nexthop * output)
{
    TRACE_ENTRY("");
    int ret = CONFD_OK;

    enum route_afi afi = (C_IPV4PREFIX == ke->dest_prefix.type) ? ipv4 : ipv6;

    char * ip_str = get_ipv_str(ipv4 == afi);

    char path_buffer[MAX_BUFF_SIZE];

    confd_format_keypath(path_buffer, MAX_BUFF_SIZE,
                         ROUTE_PATH "/next-hop-list/next-hop",
                         &ke->prot_type, &ke->prot_name, ip_str,
                         &ke->dest_prefix);
    LOG("extract next-hop-list for:\n%s", path_buffer);

    int n = 0;
    n = cdb_num_instances(rsock, path_buffer);
    LOG("has %d instances", n);

    if (0 >= n) {
        LOG("nothing to do...");
        goto term;
    }

    char inst_path[MAX_BUFF_SIZE];

    // pointer to the last nexthop_list entry, to quickly append new ones
    void * last_nh_list = NULL;
    last_nh_list = (ipv4 == afi) ? (void *) output->ipv4.list
                                 : (void *) output->ipv6.list;

    // the currently processed nexthop_list entry
    void * curr_nh_list = NULL;

    int i;
    for (i=0; i < n; i++) {

        curr_nh_list = malloc((ipv4 == afi) ? nh4list_size : nh6list_size);
        if (NULL == curr_nh_list) {
            error("%s (nexthop_list)", MALLOC_FAILED);
            goto term;
            // TODO ? cleanup etc...
        }

        snprintf(inst_path, MAX_BUFF_SIZE, "%s[%d]", path_buffer, i);
        ret = get_cdb_ipv_nexthop(rsock, inst_path, afi, curr_nh_list);
        if (CONFD_OK != ret) {
            error("Failed filing nexhtop data (#%d)!", i);
            goto term;
            // TODO - some cleanup?
        }

        if (ipv4 == afi) {
            if (NULL == last_nh_list) {
                output->ipv4.list = curr_nh_list;
            } else {
                ((struct ipv4_nexthop_list *)last_nh_list)->next = curr_nh_list;
            }
            ((struct ipv4_nexthop_list *)curr_nh_list)->next = NULL;
        } else {
            if (NULL == last_nh_list) {
                output->ipv6.list = curr_nh_list;
            } else {
                ((struct ipv6_nexthop_list *)last_nh_list)->next = curr_nh_list;
            }
            ((struct ipv6_nexthop_list *)curr_nh_list)->next = NULL;
        }
        last_nh_list = curr_nh_list;
    }

term:
    TRACE_EXIT("(%d)", ret);
    return ret;
}

// read data of specific route from CDB
static int get_route_data(int rsock, struct key_entry * ke, struct route * rt)
{
    TRACE_ENTRY("");
    int ret = CONFD_OK;

    memset(rt, 0x00, sizeof(struct route));

    route_key_from_val(&ke->dest_prefix, &rt->key);

    confd_value_t v_nh_type;
    CONFD_SET_NOEXISTS(&v_nh_type);

    char * ip_str = get_ipv_str(ipv4 == rt->key.afi);

    char p_buffer[MAX_BUFF_SIZE];
    memset(p_buffer, 0x00, MAX_BUFF_SIZE);

    confd_format_keypath(p_buffer, MAX_BUFF_SIZE, ROUTE_PATH,
                            &ke->prot_type, &ke->prot_name, ip_str,
                            &ke->dest_prefix);

    ret = cdb_get_case(rsock, "next-hop-options", &v_nh_type, p_buffer);

    int hash;

    if (CONFD_OK != ret && CONFD_ERR_NOEXISTS == confd_errno) {
        error("Missing \"next-hop-options\" choice!");
        ret = CONFD_ERR;
        return ret;
    } else {
        hash = CONFD_GET_XMLTAG(&v_nh_type);
    }

    rt->nexthop_type = nexthop_type_hash_to_system(hash);

    switch (hash) {
        case rt_simple_next_hop:
        {
            enum route_afi afi = (C_IPV4PREFIX == ke->dest_prefix.type) ? ipv4
                                                                        : ipv6;
            void * target = (ipv4 == afi) ? (void *) &rt->nexthop.ipv4.val
                                          : (void *) &rt->nexthop.ipv6.val;
            ret = get_cdb_ipv_nexthop(rsock, p_buffer, afi, target);
            if (CONFD_OK != ret) {
                error("Failed getting the simple-next-hop data!");
            }
        }
            break;

        case rt_special_next_hop:
        {
            confd_value_t v;
            CONFD_SET_NOEXISTS(&v);

            ret = get_cdb_elem(rsock, &v, "%s/special-next-hop", p_buffer);

            enum special_nexthop spec_nh;
            if (CONFD_OK != ret && C_NOEXISTS == v.type) {
                spec_nh = none;
            } else {
                spec_nh = spec_nexthop_hash_to_system(CONFD_GET_ENUM_VALUE(&v));
            }
            rt->nexthop.special = spec_nh;
        }
            break;

        case rt_next_hop_list:
        {
            ret = get_cdb_nexthop_list(rsock, ke, &rt->nexthop);
            if (CONFD_OK != ret) {
                error("Failed getting the next-hop-list!");
            }
        }
            break;

        default:
            error("Unexpected next-hop type!");
            ret = CONFD_ERR;
    }

// TODO - some cleanup?
    TRACE_EXIT("");
    return ret;
}

// clean the IPv4/IPv6 nexthop structure
static void clean_nexthop_val(enum route_afi afi, void * val_ptr)
{
    if (NULL == val_ptr) {
        return;
    }

    // remove the interface string if present
    void * to_del_ptr = NULL;
    if (ipv4 == afi) {
        to_del_ptr = (void*)(((struct ipv4_nexthop *)val_ptr)->iface);
    } else {
        to_del_ptr = (void*)(((struct ipv6_nexthop *)val_ptr)->iface);
    }
    if (NULL != to_del_ptr) {
        free(to_del_ptr);
    }
    memset(val_ptr, 0x00, (ipv4 == afi) ? nh4_size : nh6_size);
}

// clean all the dynamic memory from the route record (if applicable)
static void clean_route_data(struct route * rt)
{
    if (NULL == rt) {
        return;
    }

    enum route_afi afi = rt->key.afi;

    switch (rt->nexthop_type) {
        case simple:
            if (ipv4 == afi) {
                clean_nexthop_val(afi, &rt->nexthop.ipv4.val);
            } else {
                clean_nexthop_val(afi, &rt->nexthop.ipv6.val);
            }
            break;

        case special:
            // no data to clean...
            break;

        case list:
        {
            void * ptr_to_clean = NULL;
            void * next_list = NULL;
            void * curr_list = (ipv4 == afi) ? (void *) rt->nexthop.ipv4.list
                                       : (void *) rt->nexthop.ipv6.list;

            while (NULL != curr_list) {
                if (ipv4 == afi) {
                    struct ipv4_nexthop_list * ptr4 =
                                    (struct ipv4_nexthop_list *)curr_list;
                    next_list = (void*)(ptr4->next);
                    ptr_to_clean = &ptr4->val;
                } else {
                    struct ipv6_nexthop_list * ptr6 =
                                    (struct ipv6_nexthop_list *)curr_list;
                    next_list = (void*)(ptr6->next);
                    ptr_to_clean = &ptr6->val;
                }
                clean_nexthop_val(afi, ptr_to_clean);
                free(curr_list);
                curr_list = next_list;
            }
        }
            break;

        default:
            error("Unexpected nexthop type! (%d)", rt->nexthop_type);
            ;
    }
}

// main CDB subscriber execution of the route creation/updates
static int process_updates(struct key_entry * entries, int rsock)
{
    TRACE_ENTRY("");
    int ret = CONFD_OK;

    if (NULL == entries) {
        LOG("nothing to be done...");
        goto term;
    }

    struct key_entry * ptr = entries;
    while (NULL != ptr) {
        trace_key_entry("processing UPDATE of entry", ptr);

        struct route rt;
        if (CONFD_OK != get_route_data(rsock, ptr, &rt)) {
            error("Failed to retrieve data for created/modified route!");
            ret = CONFD_ERR;
            break;
        }

        enum routing_ret_val sys_ret = create_or_update_route(&rt);
        if (ok != sys_ret) {
            error("Failed to create/update route!");
            ret = CONFD_ERR;
            break;
        }

        clean_route_data(&rt);
        ptr = ptr->next;
    }

term:
    TRACE_EXIT("(%d)", ret);
    return ret;
}

/* This function is called on every update to the subscription. */
static void handle_update(int rsock, int ssock, int spoint)
{
    LOG("rsock: %d, spoint: %d.", rsock, spoint);
    //routing_system_dummy_ip_cmd_test();

    struct iter_data data;
    memset(&data, 0x00, sizeof(data));

    // iterate all the changes done in this transaction
    int ret = CONFD_OK;
    ret = cdb_diff_iterate(ssock, spoint, routing_iterator, 0, &data);
    if (CONFD_OK != ret) {
        LOG("Something went wrong in CDB ITER!");
    }

    trace_key_entries(data.deletes, "iterated DELETES");
    trace_key_entries(data.updates, "iterated UPDATES");

    ret = process_deletes(data.deletes);
    if (CONFD_OK != ret) {
        error("Failed with DELETES!");
        // TODO - error?
        // not much we can do in non-two phase subscriber
    }

    ret = process_updates(data.updates, rsock);
    if (CONFD_OK != ret) {
        error("Failed with UPDATES!");
        // TODO - error?
        // not much we can do in non-two phase subscriber
    }

    iter_data_clean(&data);
}

// LINUXCFG component setup() procedure registering CDB subscriber
void routing_subs_setup(struct confd_daemon_ctx *dctx, int rsock)
{
    int spoint = 0;
    spoint = subscribe(NORMAL_PRIO + 1, handle_update,
                            "/routing/control-plane-protocols"
            );
    LOG("ietf_routing started, spoint: %d.", spoint);
}
