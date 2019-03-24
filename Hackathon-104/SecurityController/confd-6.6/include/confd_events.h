/*
 * Copyright 2005-2011 Tail-F Systems AB
 */

#ifndef _CONFD_EVENTS_H
#define _CONFD_EVENTS_H 1

#ifdef __cplusplus
extern "C" {
#endif

/* when we receive confd_ha_notification structs, */
/* the type field is either */
/* of theese values - indicating what happened */
enum confd_ha_info_type {
    CONFD_HA_INFO_NOMASTER          = 1,   /* we have no master */
    CONFD_HA_INFO_SLAVE_DIED        = 2,   /* a slave disappeared */
    CONFD_HA_INFO_SLAVE_ARRIVED     = 3,   /* a slave arrived to us */
    CONFD_HA_INFO_SLAVE_INITIALIZED = 4,   /* CDB is initialized */
    CONFD_HA_INFO_IS_MASTER         = 5,   /* we are now master */
    CONFD_HA_INFO_IS_NONE           = 6,   /* we are now none */
    CONFD_HA_INFO_BESLAVE_RESULT    = 7    /* result of async beslave() */
};

/* event delivered from the CONFD_NOTIF_HA_INFO flag */
struct confd_ha_notification {
    enum confd_ha_info_type type;
    /* additional info for various info types */
    union {
        int nomaster;                      /* CONFD_HA_INFO_NOMASTER  */
        struct confd_ha_node slave_died;   /* CONFD_HA_INFO_SLAVE_DIED */
        struct confd_ha_node slave_arrived;/* CONFD_HA_INFO_SLAVE_ARRIVED*/
        int cdb_initialized_by_copy;       /* CONFD_HA_INFO_SLAVE_INITIALIZED */
        int beslave_result;                /* CONFD_HA_INFO_BESLAVE_RESULT */
    } data;
};

/* when we receive confd_subagent_notification structs, */
/* the type field is either */
/* of theese values - indicating what happened */
enum confd_subagent_info_type {
    CONFD_SUBAGENT_INFO_UP          = 1,
    CONFD_SUBAGENT_INFO_DOWN        = 2
};


#define MAXAGENTNAMELEN 32
#define MAXTARGETNAMELEN 32

/* event delivered from the CONFD_NOTIF_SUBAGENT_INFO flag */
struct confd_subagent_notification {
    enum confd_subagent_info_type type;
    char name[MAXAGENTNAMELEN];
};


/* when we receive confd_forward_notification structs, */
/* the type field is either */
/* of theese values - indicating what happened */
enum confd_forward_info_type {
    CONFD_FORWARD_INFO_UP          = 1,
    CONFD_FORWARD_INFO_DOWN        = 2,
    CONFD_FORWARD_INFO_FAILED      = 3
};


/* event delivered from the CONFD_NOTIF_FORWARD_INFO flag */
struct confd_forward_notification {
    enum confd_forward_info_type type; /* type of forward event     */
    char target[MAXTARGETNAMELEN];     /* target name in confd.conf */
    struct confd_user_info uinfo;      /* on behalf of which user   */
};

/* event delivered from the CONFD_NOTIF_DAEMON, the CONFD_NOTIF_NETCONF, */
/* and the CONFD_NOTIF_DEVEL flags                                       */
struct confd_syslog_notification {
    int prio;   /* from syslog.h */
    int logno;  /* number from confd_logsyms.h */
    char msg[BUFSIZ];
};


/* event delivered from the CONFD_NOTIF_AUDIT flag */
struct confd_audit_notification {
    int logno;   /* number from confd_logsyms.h */
    char user[MAXUSERNAMELEN];
    char msg[BUFSIZ];
    int usid;   /* session id (0 means - not applicable ) */
};



struct confd_netconf_failed_commit {
    struct confd_ip ip;
    u_int16_t port;
};


/* When commit fails (which is almost fatal) this enum */
/* describes which dataprovider was responsible        */
enum confd_data_provider {
    CONFD_DP_CDB        = 1,
    CONFD_DP_NETCONF    = 2,    /* netconf gateway */
    CONFD_DP_EXTERNAL   = 3,    /* external c/java dataproviders */
    CONFD_DP_SNMPGW     = 4,    /* when snmpgw can eventually write */
    CONFD_DP_JAVASCRIPT = 5     /* not used */
};

/* event delivered from the CONFD_NOTIF_COMMIT_FAILED flag */
struct confd_commit_failed_notification {
    enum confd_data_provider provider;
    enum confd_dbname dbname;
    union {
        struct confd_netconf_failed_commit nc;
        char daemon_name[MAX_DAEMON_NAME_LEN];
    } v;
};

/* flags for confd_commit_notification and confd_commit_diff_notification */
#define CONFD_NOTIF_COMMIT_FLAG_CONFIRMED           (1 << 0)
#define CONFD_NOTIF_COMMIT_FLAG_CONFIRMED_EXTENDED  (1 << 1)

/* event delivered from the CONFD_NOTIF_COMMIT_SIMPLE flag */
struct confd_commit_notification {
    enum confd_dbname database;
    int diff_available;
    struct confd_user_info uinfo;
    int flags;
};


#define MAX_COMMENT_LEN 256
#define MAX_LABEL_LEN   256

/* event delivered from the CONFD_NOTIF_COMMIT_DIFF flag */
struct confd_commit_diff_notification {
    enum confd_dbname database;
    struct confd_user_info uinfo;
    struct confd_trans_ctx  *tctx;
    int flags;
    char comment[MAX_COMMENT_LEN];
    char label[MAX_LABEL_LEN];
};

enum confd_confirmed_commit_type {
    CONFD_CONFIRMED_COMMIT = 1,
    CONFD_CONFIRMING_COMMIT = 2,
    CONFD_ABORT_COMMIT = 3
};

/* event delivered from the CONFD_NOTIF_CONFIRMED_COMMIT flag */
struct confd_confirmed_commit_notification {
    enum confd_confirmed_commit_type type;
    unsigned int timeout; /* in seconds
                             timeout is > 0 when type is CONFD_CONFIRMED_COMMIT,
                             otherwise it is 0 */
    struct confd_user_info uinfo;
};

enum confd_upgrade_event_type {
    CONFD_UPGRADE_INIT_STARTED = 1,
    CONFD_UPGRADE_INIT_SUCCEEDED = 2,
    CONFD_UPGRADE_PERFORMED = 3,
    CONFD_UPGRADE_COMMITED = 4,
    CONFD_UPGRADE_ABORTED = 5
};

/* event delivered from the CONFD_NOTIF_UPGRADE_EVENT flag */
struct confd_upgrade_notification {
    enum confd_upgrade_event_type event;
};

/* event delivered from the CONFD_NOTIF_COMMIT_PROGRESS flag */
struct confd_progress_notification {
    enum confd_dbname database;
    int usid;                   /* user session id */
    int thandle;                /* transaction handle */
    char msg[BUFSIZ];
};

enum confd_user_sess_type {
    CONFD_USER_SESS_START = 1,       /* a user session is started */
    CONFD_USER_SESS_STOP = 2,
    CONFD_USER_SESS_LOCK = 3,        /* a database is locked */
    CONFD_USER_SESS_UNLOCK = 4,
    CONFD_USER_SESS_START_TRANS = 5, /* a database transaction is started */
    CONFD_USER_SESS_STOP_TRANS = 6
};

/* event delivered from the CONFD_NOTIF_USER_SESSION flag */
struct confd_user_sess_notification {
    enum confd_user_sess_type type;
    struct confd_user_info uinfo;
    enum confd_dbname database;
};


enum confd_snmp_pdu_type {
    CONFD_SNMPA_PDU_V1TRAP=           1,
    CONFD_SNMPA_PDU_V2TRAP=           2,
    CONFD_SNMPA_PDU_INFORM=           3,
    CONFD_SNMPA_PDU_GET_RESPONSE=     4,
    CONFD_SNMPA_PDU_GET_REQUEST=      5,
    CONFD_SNMPA_PDU_GET_NEXT_REQUEST= 6,
    CONFD_SNMPA_PDU_REPORT=           7,
    CONFD_SNMPA_PDU_GET_BULK_REQUEST= 8,
    CONFD_SNMPA_PDU_SET_REQUEST=      9
};


struct confd_v1_trap_info {
    struct confd_snmp_oid enterprise;
    int generic_trap;
    int specific_trap;
    int time_stamp;
};

/* event delivered from the CONFD_NOTIF_SNMPA flag */
struct confd_snmpa_notification {
    enum confd_snmp_pdu_type pdu_type;
    int request_id;
    struct confd_ip ip;
    unsigned short port;
    int error_status;
    int error_index;
    int num_variables;                  /* size of vbinds     */
    struct confd_snmp_varbind *vb;      /* lib malloced array */
    struct confd_v1_trap_info *v1_trap; /* v1 traps pdus only */
};

/* If the pdu_type is V1TRAP, we have additional info in the            */
/* v1_trap field which otherwise is NULL                                */


enum confd_stream_notif_type {
    CONFD_STREAM_NOTIFICATION_EVENT = 1,    /* actual event notification */
    CONFD_STREAM_NOTIFICATION_COMPLETE = 2, /* <notificationComplete> */
    CONFD_STREAM_REPLAY_COMPLETE = 3,       /* <replayComplete> */
    CONFD_STREAM_REPLAY_FAILED = 4          /* replay failed per replay_error */
};

/* event delivered from the CONFD_NOTIF_STREAM_EVENT flag */
struct confd_stream_notification {
    enum confd_stream_notif_type type;
    struct confd_datetime event_time;
    confd_tag_value_t *values;
    int nvalues;
    char *replay_error;
};


enum ncs_cq_progress_notif_type {
    NCS_CQ_ITEM_WAITING           = 1,
    NCS_CQ_ITEM_EXECUTING         = 2,
    NCS_CQ_ITEM_LOCKED            = 3,
    NCS_CQ_ITEM_COMPLETED         = 4,
    NCS_CQ_ITEM_FAILED            = 5,
    NCS_CQ_ITEM_DELETED           = 6
};

/* event delivered from the NCS_NOTIF_CQ_PROGRESS flag */
struct ncs_cq_progress_notification {
    enum ncs_cq_progress_notif_type type;
    struct confd_datetime timestamp;
    char* cq_tag;
    u_int64_t cq_id;
    char **completed_devices;
    int  ncompleted_devices;
    char **transient_devices;
    int  ntransient_devices;
    char **failed_devices;
    char **failed_reasons;
    int  nfailed_devices;
    char **completed_services;
    confd_value_t **completed_services_completed_devices;
    int  ncompleted_services;
    char **failed_services;
    confd_value_t **failed_services_completed_devices;
    confd_value_t **failed_services_failed_devices;
    int  nfailed_services;
};




/* This is a bitmask */
enum confd_notification_type {
    CONFD_NOTIF_AUDIT                  = (1 << 0),
    CONFD_NOTIF_DAEMON                 = (1 << 1),
    CONFD_NOTIF_TAKEOVER_SYSLOG        = (1 << 2),
    CONFD_NOTIF_COMMIT_SIMPLE          = (1 << 3),
    CONFD_NOTIF_COMMIT_DIFF            = (1 << 4),
    CONFD_NOTIF_USER_SESSION           = (1 << 5),
    CONFD_NOTIF_HA_INFO                = (1 << 6),
    CONFD_NOTIF_SUBAGENT_INFO          = (1 << 7),
    CONFD_NOTIF_COMMIT_FAILED          = (1 << 8),
    CONFD_NOTIF_SNMPA                  = (1 << 9),
    CONFD_NOTIF_FORWARD_INFO           = (1 << 10),
    CONFD_NOTIF_NETCONF                = (1 << 11),
    CONFD_NOTIF_DEVEL                  = (1 << 12),
    CONFD_NOTIF_HEARTBEAT              = (1 << 13),
    CONFD_NOTIF_CONFIRMED_COMMIT       = (1 << 14),
    CONFD_NOTIF_UPGRADE_EVENT          = (1 << 15),
    CONFD_NOTIF_COMMIT_PROGRESS        = (1 << 16),
    CONFD_NOTIF_AUDIT_SYNC             = (1 << 17),
    CONFD_NOTIF_HEALTH_CHECK           = (1 << 18),
    CONFD_NOTIF_STREAM_EVENT           = (1 << 19),
    CONFD_NOTIF_HA_INFO_SYNC           = (1 << 20),
    NCS_NOTIF_PACKAGE_RELOAD           = (1 << 21),
    NCS_NOTIF_CQ_PROGRESS              = (1 << 22),
    CONFD_NOTIF_REOPEN_LOGS            = (1 << 23)
};



/* additional argument for the confd_notifications_connect2() */
/* function.                                                  */

struct confd_notifications_data {
    int heartbeat_interval;     /* required if we wish to generate */
                                /* CONFD_NOTIF_HEARTBEAT events    */
                                /* the time is milli seconds       */
    int health_check_interval;  /* required if we wish to generate */
                                /* CONFD_NOTIF_HEALTH_CHECK events */
                                /* the time is milli seconds       */
    /* The following five are used for CONFD_NOTIF_STREAM_EVENT    */
    char *stream_name;          /* stream name (required)          */
    confd_value_t start_time;   /* type = C_NOEXISTS or C_DATETIME */
    confd_value_t stop_time;    /* type = C_NOEXISTS or C_DATETIME */
                                /* when start_time is C_DATETIME   */
    char *xpath_filter;         /* optional XPath filter for the   */
                                /* stream -  NULL for no filter    */
    int usid;                   /* optional user session id for    */
                                /* AAA  restriction - 0 for no AAA */
};

/* Badly named and kept for backwards compatibility - don't use */
#define CONFD_NOTIF_SYSLOG    CONFD_NOTIF_DAEMON
#define CONFD_NOTIF_SYSLOG_TAKEOVER   (CONFD_NOTIF_DAEMON | \
                                       CONFD_NOTIF_TAKEOVER_SYSLOG)




/* data structure received over the notifications socket */
struct confd_notification {
    enum confd_notification_type type;
    union {
        struct confd_audit_notification audit;
        struct confd_syslog_notification syslog;
        struct confd_commit_notification commit;
        struct confd_commit_diff_notification commit_diff;
        struct confd_user_sess_notification user_sess;
        struct confd_ha_notification hnot;
        struct confd_subagent_notification subagent;
        struct confd_forward_notification forward;
        struct confd_commit_failed_notification cfail;
        struct confd_snmpa_notification snmpa;
        struct confd_confirmed_commit_notification confirm;
        struct confd_upgrade_notification upgrade;
        struct confd_progress_notification progress;
        struct confd_stream_notification stream;
        struct ncs_cq_progress_notification cq_progress;
    } n;
};


extern int confd_notifications_connect(int sock, const struct sockaddr* srv,
                                       int srv_sz,int mask);

extern int confd_notifications_connect2(
    int sock, const struct sockaddr* srv,
    int srv_sz,int mask,
    struct confd_notifications_data *data);

extern int confd_read_notification(int sock,struct confd_notification *n);

extern void confd_free_notification(struct confd_notification *n);

extern int confd_diff_notification_done(int sock,
                                        struct confd_trans_ctx  *tctx);

extern int confd_sync_audit_notification(int sock, int usid);

extern int confd_sync_ha_notification(int sock);

#ifdef __cplusplus
}
#endif
#endif
