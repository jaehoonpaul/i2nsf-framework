/*
 * Copyright 2014 Tail-F Systems AB Tail-F customers are permitted to
 * redistribute in binary form, with or without modification, for use
 * in customer products.
 */

#ifndef _IFLIST_H
#define _IFLIST_H 1

#define NOT_SET 0xFF

struct keyval {
    struct keyval *next;
    char *key;
    char *val;
};

struct ifstate {
    struct ifstate *next;

    /* Common parameters coming from ietf-interface. */
    char  *ifname;
    int    state;

    /* ipv6 specific settings. */
    int              ipv6enabled;
    int              ipv6forwarding;
    int              ipv6mtu;
    int              ipv6dad_transmits;
    int              ipv6create_global_addresses;
    int              ipv6use_tempaddr;
    int              ipv6tmp_valid_life;
    int              ipv6tmp_pref_life;
    struct keyval  *ipv6addrlist;
    struct keyval  *ipv6neighlist;

    /* ipv4 specific settings. */
    int              ipv4enabled;
    int              ipv4forwarding;
    int              ipv4mtu;
    struct keyval  *ipv4addrlist;
    struct keyval  *ipv4neighlist;
};

struct ifstate *get_ifstate(char *ifname);
void clear_ifstate(char *ifname);
struct keyval *find_list(struct keyval *list, char *key);

/* This declares simple handlers for updates to the ifstate
   list. It will produce a set_X function that calls an apply_X
   function if appropriate. */
#define DECLARE_SIMPLE_HANDLER(name, flag)           \
    static void set_##name(char *ifname, int val) {  \
          struct ifstate *ifs = get_ifstate(ifname); \
                                                     \
          if ((val != ifs->name) && (ifs->flag)) { \
            apply_##name(ifname, val);               \
          }                                          \
                                                     \
          ifs->name = val;                         \
          }

/* Update a list of addresses, this might be a new record
 * or the  address might have changed on a previous one.
 *
 * The list name and the delete/add functions are derived from
 * the name parameter and must exist.
 */
#define DECLARE_LIST_HANDLER(name, flag)                                \
    static void update_##name(char *ifname,                             \
                              char *key,                                \
                              char *val) {                              \
    struct ifstate *ifs = get_ifstate(ifname);                          \
    struct keyval *p    = find_list(ifs->name##list, key);              \
                                                                        \
    if (p != NULL) {                                                    \
        if (strcmp(p->val, val) != 0) {                                 \
            if (ifs->flag) {                                            \
                delete_##name(ifname, key, p->val);                     \
            }                                                           \
            free(p->val);                                               \
            p->val = strdup(val);                                       \
        } else {                                                        \
            /* No change */                                             \
            return;                                                     \
        }                                                               \
    } else {                                                            \
        p = xmalloc(sizeof(struct keyval));                             \
        p->key  = strdup(key);                                          \
        p->val  = strdup(val);                                          \
        p->next = ifs->name##list;                                      \
        ifs->name##list = p;                                            \
    }                                                                   \
                                                                        \
    if (ifs->flag)                                                      \
        add_##name(ifname, key, val);                                   \
    }                                                                   \
                                                                        \
    static void list_delete_##name(char *ifname, char *key) {           \
        struct ifstate  *ifs = get_ifstate(ifname);                     \
        struct keyval *p, *prev;                                        \
                                                                        \
        for(p = ifs->name##list, prev = NULL;                           \
            p != NULL; prev = p, p = p->next) {                         \
            if (strcmp(p->key, key) == 0) {                             \
                if (prev == NULL) {                                     \
                    ifs->name##list = p->next;                          \
                } else {                                                \
                    prev->next = p->next;                               \
                }                                                       \
                                                                        \
                delete_##name(ifname, key, p->val);                     \
                                                                        \
                XFREE(p->key);                                          \
                XFREE(p->val);                                          \
                XFREE(p);                                               \
                return;                                                 \
            }                                                           \
        }                                                               \
                                                                        \
        warn("Could not find ipv4 neighbour %s for interface %s",       \
             key, key);                                                 \
    }


#define NO_FLAG ipv4enabled || 1

#endif
