/*
 * Copyright 2014 Tail-F Systems AB Tail-F customers are permitted to
 * redistribute in binary form, with or without modification, for use
 * in customer products.
 */

#ifndef _IETF_INTERFACES_API_H_
#define _IETF_INTERFACES_API_H_

typedef enum cdb_iter_ret (*iter_handler)(confd_hkeypath_t *kp,
                                          enum cdb_iter_op op,
                                          confd_value_t *oldv,
                                          confd_value_t *newv,
                                          void *state);

/*
 * Registers a data handler for a specific tag under
 * /interfaces/
 *
 * The handler will be called like a regular iteration handler for
 * very change to the tag.
 */
void if_register_sub_handler(int tag, iter_handler handler);


/*
 * A handler for state update.
 *
 * - admin_state indicates the admin_state in CDB (invalid if not in cdb)
 * - cdb_state is a flag indicating if the interface exists in cdb.
 * - if_state indicates of the interface exists in the operating system or not.
 */
typedef void (*if_state_handler)(char *ifname,
                                 int admin_state,
                                 int cdb_state,
                                 int if_state);

/* Registers a handler for interface state updates */
void if_register_if_handler(if_state_handler handler);

#endif
