/*
 * Copyright 2005-2011 Tail-F Systems AB
 */

#ifndef _CONFD_HA_H
#define _CONFD_HA_H 1

#ifdef __cplusplus
extern "C" {
#endif

/* a node can be in either of four states */
enum confd_ha_status_state {
    CONFD_HA_STATE_NONE = 1,
    CONFD_HA_STATE_SLAVE = 2,
    CONFD_HA_STATE_MASTER = 3,
    CONFD_HA_STATE_SLAVE_RELAY = 4
};

struct confd_ha_status {
    enum confd_ha_status_state state;
    /* if state is MASTER, we also have a list of slaves */
    /* if state is SLAVE, then nodes[0] contains the master */
    /* if state is RELAY_SLAVE, then nodes[0] contains the master,
       and following entries contain the "sub-slaves" */
    /* if state is NONE, we have no nodes at all */
    struct confd_ha_node nodes[255];
    int num_nodes;
};

extern int confd_ha_connect(int sock, const struct sockaddr* srv,
                            int srv_sz, const char *token);

extern int confd_ha_bemaster(int sock, confd_value_t *mynodeid);

extern int confd_ha_beslave(int sock, confd_value_t *mynodeid,
                            struct confd_ha_node *master, int waitreply);

extern int confd_ha_berelay(int sock);

extern int confd_ha_benone(int sock);

extern int confd_ha_get_status(int sock, struct confd_ha_status *stat);

extern int confd_ha_slave_dead(int sock, confd_value_t *nodeid);

/* backward compatibility */
#define confd_ha_status(sock, stat) confd_ha_get_status((sock), (stat))

#ifdef __cplusplus
}
#endif
#endif
