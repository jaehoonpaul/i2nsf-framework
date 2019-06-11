/*
 * Copyright 2005-2011 Tail-F Systems AB
 */

#ifndef _CONFD_DP_H
#define _CONFD_DP_H 1

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_CALLPOINT_LEN 256 /* strlen of callpoint/valpoint len */

struct confd_daemon_ctx;
struct confd_db_ctx;

/* Sockets that are connected through confd_connect() */
/* need to have a type */

enum confd_sock_type {
    CONTROL_SOCKET,
    WORKER_SOCKET
};

/* data structure returned by show path rewrite function */
struct confd_rewrite_select {
    char **tokens;
    int n;
};

struct confd_next_object {
    confd_value_t *v;
    int n;
    long next;
};

struct confd_tag_next_object {
    confd_tag_value_t *tv;
    int n;
    long next;
};

struct confd_usess_cbs {
    void (*start)(struct confd_daemon_ctx *dx,
                  struct confd_user_info *uinfo);
    void (*stop)(struct confd_daemon_ctx *dx,
                 struct confd_user_info *uinfo);
};

struct confd_trans_cbs {
    int (*init)(struct confd_trans_ctx *tctx);
    int (*trans_lock)(struct confd_trans_ctx *sctx);
    int (*trans_unlock)(struct confd_trans_ctx *sctx);
    int (*write_start)(struct confd_trans_ctx *sctx);
    int (*prepare)(struct confd_trans_ctx *tctx);
    int (*abort)(struct confd_trans_ctx *tctx);
    int (*commit)(struct confd_trans_ctx *tctx);
    int (*finish)(struct confd_trans_ctx *tctx);
    void (*interrupt)(struct confd_trans_ctx *tctx);
};

struct confd_db_cbs {
    int (*candidate_commit)(struct confd_db_ctx *dbx, int timeout);
    int (*candidate_confirming_commit)(struct confd_db_ctx *dbx);
    int (*candidate_reset)(struct confd_db_ctx *dbx);
    int (*candidate_chk_not_modified)(struct confd_db_ctx *dbx);
    int (*candidate_rollback_running)(struct confd_db_ctx *dbx);
    int (*candidate_validate)(struct confd_db_ctx *dbx);
    int (*add_checkpoint_running)(struct confd_db_ctx *dbx);
    int (*del_checkpoint_running)(struct confd_db_ctx *dbx);
    int (*activate_checkpoint_running)(struct confd_db_ctx *dbx);
    int (*copy_running_to_startup)(struct confd_db_ctx *dbx);
    int (*running_chk_not_modified)(struct confd_db_ctx *dbx);
    int (*lock)(struct confd_db_ctx *dbx, enum confd_dbname dbname);
    int (*unlock)(struct confd_db_ctx *dbx, enum confd_dbname dbname);
    int (*lock_partial)(struct confd_db_ctx *dbx,
                        enum confd_dbname dbname, int lockid,
                        confd_hkeypath_t paths[], int npaths);
    int (*unlock_partial)(struct confd_db_ctx *dbx,
                          enum confd_dbname dbname, int lockid);
    int (*delete_config)(struct confd_db_ctx *dbx,
                         enum confd_dbname dbname);
};

struct confd_data_cbs {
    char callpoint[MAX_CALLPOINT_LEN];
    /* where in the XML tree do we */
    /* want this struct */

    /* Only necessary to have this cb if our data model has */
    /* typeless optional nodes or oper data lists w/o keys */
    int (*exists_optional)(struct confd_trans_ctx *tctx,
                           confd_hkeypath_t *kp);
    int (*get_elem)(struct confd_trans_ctx *tctx,
                    confd_hkeypath_t *kp);
    int (*get_next)(struct confd_trans_ctx *tctx,
                    confd_hkeypath_t *kp, long next);
    int (*set_elem)(struct confd_trans_ctx *tctx,
                    confd_hkeypath_t *kp,
                    confd_value_t *newval);
    int (*create)(struct confd_trans_ctx *tctx,
                  confd_hkeypath_t *kp);
    int (*remove)(struct confd_trans_ctx *tctx,
                  confd_hkeypath_t *kp);
    /* optional (find list entry by key/index values) */
    int (*find_next)(struct confd_trans_ctx *tctx,
                     confd_hkeypath_t *kp,
                     enum confd_find_next_type type,
                     confd_value_t *keys, int nkeys);
    /* optional optimizations */
    int (*num_instances)(struct confd_trans_ctx *tctx,
                         confd_hkeypath_t *kp);
    int (*get_object)(struct confd_trans_ctx *tctx,
                      confd_hkeypath_t *kp);
    int (*get_next_object)(struct confd_trans_ctx *tctx,
                           confd_hkeypath_t *kp, long next);
    int (*find_next_object)(struct confd_trans_ctx *tctx,
                            confd_hkeypath_t *kp,
                            enum confd_find_next_type type,
                            confd_value_t *keys, int nkeys);
    /* next two are only necessary if 'choice' is used */
    int (*get_case)(struct confd_trans_ctx *tctx,
                    confd_hkeypath_t *kp, confd_value_t *choice);
    int (*set_case)(struct confd_trans_ctx *tctx,
                    confd_hkeypath_t *kp, confd_value_t *choice,
                    confd_value_t *caseval);
    /* next two are only necessary for config data providers,
       and only if /confdConfig/enableAttributes is 'true' */
    int (*get_attrs)(struct confd_trans_ctx *tctx,
                     confd_hkeypath_t *kp,
                     u_int32_t *attrs, int num_attrs);
    int (*set_attr)(struct confd_trans_ctx *tctx,
                    confd_hkeypath_t *kp,
                    u_int32_t attr, confd_value_t *v);
    /* only necessary if "ordered-by user" is used */
    int (*move_after)(struct confd_trans_ctx *tctx,
                      confd_hkeypath_t *kp, confd_value_t *prevkeys);
    /* only for per-transaction-invoked transaction hook */
    int (*write_all)(struct confd_trans_ctx *tctx,
                     confd_hkeypath_t *kp);
    void *cb_opaque; /* private user data    */
};

/* callbacks for NCS services */

enum ncs_service_operation {
    NCS_SERVICE_CREATE = 0,
    NCS_SERVICE_UPDATE = 1,
    NCS_SERVICE_DELETE = 2
};

struct ncs_service_cbs {
    char servicepoint[MAX_CALLPOINT_LEN];

    int (*pre_modification)(struct confd_trans_ctx *tctx,
                            enum ncs_service_operation op,
                            confd_hkeypath_t *kp,
                            struct ncs_name_value *proplist,
                            int num_props);
    int (*pre_lock_create)(struct confd_trans_ctx *tctx,
                           confd_hkeypath_t *kp,
                           struct ncs_name_value *proplist,
                           int num_props, int fastmap_thandle);
    int (*create)(struct confd_trans_ctx *tctx, confd_hkeypath_t *kp,
                  struct ncs_name_value *proplist, int num_props,
                  int fastmap_thandle);
    int (*post_modification)(struct confd_trans_ctx *tctx,
                             enum ncs_service_operation op,
                             confd_hkeypath_t *kp,
                             struct ncs_name_value *proplist,
                             int num_props);
    void *cb_opaque; /* private user data    */
};

struct ncs_nano_service_cbs {
    char servicepoint[MAX_CALLPOINT_LEN];

    int (*nano_create)(struct confd_trans_ctx *tctx, confd_hkeypath_t *kp,
                       const confd_value_t *component,
                       const confd_value_t *state,
                       struct ncs_name_value *proplist, int num_props,
                       int fastmap_thandle);

    int (*nano_delete)(struct confd_trans_ctx *tctx, confd_hkeypath_t *kp,
                       const confd_value_t *component,
                       const confd_value_t *state,
                       struct ncs_name_value *proplist, int num_props,
                       int fastmap_thandle);

    void *cb_opaque; /* private user data    */
};


struct confd_trans_validate_cbs {
    int (*init)(struct confd_trans_ctx *tctx);
    int (*stop)(struct confd_trans_ctx *tctx);
};

struct confd_valpoint_cb {
    char valpoint[MAX_CALLPOINT_LEN];
    int (*validate)(struct confd_trans_ctx *tctx,
                    confd_hkeypath_t *kp,
                    confd_value_t *newval);
    void *cb_opaque;        /* private user data */
};

struct confd_action_cbs {
    char actionpoint[MAX_CALLPOINT_LEN];
    int (*init)(struct confd_user_info *uinfo);
    int (*abort)(struct confd_user_info *uinfo);
    int (*action)(struct confd_user_info *uinfo,
                  struct xml_tag *name,
                  confd_hkeypath_t *kp,
                  confd_tag_value_t *params,
                  int nparams);
    int (*command)(struct confd_user_info *uinfo,
                   char *path, int argc, char **argv);
    int (*completion)(struct confd_user_info *uinfo,
                      int cli_style, char *token, int completion_char,
                      confd_hkeypath_t *kp,
                      char *cmdpath, char *cmdparam_id,
                      struct confd_qname *simpleType, char *extra);
    void *cb_opaque;        /* private user data */
};


struct confd_notification_ctx {
    char *name;             /* stream name or snmp notify_name */
    char *ctx_name;         /* for snmp only */
    int fd;                 /* notification (worker) socket */
    struct confd_daemon_ctx *dx; /* our daemon ctx */
    struct confd_error error; /* user settable via */
                              /* confd_notification_seterr*() */
    void *cb_opaque;        /* private user data from registration */

    /* ConfD internal fields */
    int live_fd;
    u_int32_t subid;
    int flags;
    struct confd_ip src_addr;
    int seen_reply;
    int query_ref;
};

#define MAX_STREAMNAME_LEN 256

struct confd_notification_stream_cbs {
    char streamname[MAX_STREAMNAME_LEN];
    int fd;
    int (*get_log_times)(
        struct confd_notification_ctx *nctx);
    int (*replay)(struct confd_notification_ctx *nctx,
                  struct confd_datetime *start,
                  struct confd_datetime *stop);
    void *cb_opaque;        /* private user data */
};

struct confd_snmp_target {
    confd_value_t address;
    u_int16_t port;
};

struct confd_notification_snmp_inform_cbs {
    char cb_id[MAX_CALLPOINT_LEN];
    void (*targets)(struct confd_notification_ctx *nctx,
                    int ref, struct confd_snmp_target *targets,
                    int num_targets);
    void (*result)(struct confd_notification_ctx *nctx,
                   int ref, struct confd_snmp_target *target,
                   int got_response);
    void *cb_opaque;        /* private user data */
};

struct confd_notification_sub_snmp_cb {
    char sub_id[MAX_CALLPOINT_LEN];
    int (*recv)(struct confd_notification_ctx *nctx,
                char *notification,
                struct confd_snmp_varbind *varbinds, int num_vars,
                confd_value_t *src_addr, u_int16_t src_port);
    void *cb_opaque;        /* private user data */
};

/* Authentication callback */

struct confd_auth_ctx {
    struct confd_user_info *uinfo;
    char *method;
    int success;
    union {
        struct {         /* if success */
            int ngroups;
            char **groups;
        } succ;
        struct {         /* if !success */
            int logno;   /* number from confd_logsyms.h */
            char *reason;
        } fail;
    } ainfo;
    /* ConfD internal fields */
    char *errstr;
};

struct confd_auth_cb {
    int (*auth)(struct confd_auth_ctx *actx);
};


/* Authorization callbacks */

struct confd_authorization_ctx {
    struct confd_user_info *uinfo;
    int ngroups;
    char **groups;
    struct confd_daemon_ctx *dx;
    /* ConfD internal fields */
    int result;
    int query_ref;
};

struct confd_authorization_cbs {
    int cmd_filter;
    int data_filter;
    int (*chk_cmd_access)(struct confd_authorization_ctx *actx,
                          char **cmdtokens, int ntokens, int cmdop);
    int (*chk_data_access)(struct confd_authorization_ctx *actx,
                           u_int32_t hashed_ns, confd_hkeypath_t *hkp,
                           int dataop, int how);
};

#define CONFD_ACCESS_OP_READ              (1 << 0)
#define CONFD_ACCESS_OP_EXECUTE           (1 << 1)
#define CONFD_ACCESS_OP_CREATE            (1 << 2)
#define CONFD_ACCESS_OP_UPDATE            (1 << 3)
#define CONFD_ACCESS_OP_DELETE            (1 << 4)
#define CONFD_ACCESS_OP_WRITE             (1 << 5)

#define CONFD_ACCESS_CHK_INTERMEDIATE     (1 << 8)
#define CONFD_ACCESS_CHK_FINAL            (1 << 9)

#define CONFD_ACCESS_RESULT_ACCEPT        0
#define CONFD_ACCESS_RESULT_REJECT        1
#define CONFD_ACCESS_RESULT_CONTINUE      2
#define CONFD_ACCESS_RESULT_DEFAULT       3


/* Custom error formatting */

struct confd_error_cb {
    int error_types;
    void (*format_error)(struct confd_user_info *uinfo,
                         struct confd_errinfo *errinfo,
                         char *default_msg);
};


/* Set this flag via confd_set_daemon_flags() to make */
/* the callbacks only read and write string values -  */
/* i.e. all confd_value_t's are always of type C_BUF  */
#define CONFD_DAEMON_FLAG_STRINGSONLY  (1 << 1)
/* Set this flag to turn off set_elem()/set_case() callbacks
   when a leaf/choice gets its default value due to being unset */
#define CONFD_DAEMON_FLAG_NO_DEFAULTS  (1 << 2)
/* Set this flag to request that the daemon should be disconnected if
   any registrations are replaced by registrations from another daemon */
#define CONFD_DAEMON_FLAG_REG_REPLACE_DISCONNECT (1 << 4)
/* This flag is deprecated, and only present for temporary backward
   compatibility - it will be removed in a future release. */
#define CONFD_DAEMON_FLAG_LEAF_LIST_AS_LEAF (1 << 5)
/* Set this flag to request that get_object() rather
   than get_elem() should be used whenever possible. */
#define CONFD_DAEMON_FLAG_PREFER_BULK_GET (1 << 6)
/* Set this flag to enable get_object() callback for
   parent container when a leaf has no ancestor list node. */
#define CONFD_DAEMON_FLAG_BULK_GET_CONTAINER (1 << 7)

/* Daemon flags for internal library use */
#define CONFD_DAEMON_FLAG_SEND_IKP     (1 << 0)
#define CONFD_DAEMON_FLAG_REG_DONE     (1 << 16)

/* This struct is populated by confd_init_daemon() */
struct confd_daemon_ctx {
    void *d_opaque;          /* User data for the daemon */

    /* ConfD internal fields */
    struct confd_usess_cbs usess_cb;
    struct confd_trans_cbs trans_cb;
    struct confd_trans_validate_cbs trans_validate_cb;
    struct confd_db_cbs db_cb;
    struct confd_auth_cb auth_cb;
    struct confd_authorization_cbs authorization_cbs;
    struct confd_error_cb error_cb;
    struct confd_data_cbs *data_cbs;
    int data_cbs_len, num_data_cbs;
    struct ncs_service_cbs *service_cbs;
    int service_cbs_len, num_service_cbs;
    struct ncs_nano_service_cbs *nano_service_cbs;
    int nano_service_cbs_len, num_nano_service_cbs;
    struct confd_valpoint_cb *valp_cbs;
    int valp_cbs_len, num_validation_cbs;
    struct confd_action_cbs *action_cbs;
    int action_cbs_len, num_action_cbs;
    struct confd_notification_stream_cbs *notif_cbs;
    int notif_cbs_len, num_notif_cbs;
    struct confd_notification_snmp_inform_cbs *notif_snmp_inform_cbs;
    int notif_snmp_inform_cbs_len, num_notif_snmp_inform_cbs;
    struct confd_notification_sub_snmp_cb *notif_sub_snmp_cbs;
    int notif_sub_snmp_cbs_len, num_notif_sub_snmp_cbs;
    struct confd_notification_ctx **notif_ctxs;
    int notif_ctxs_len, num_notif_ctxs;
    int ctl_sock;                     /* the control socket */
    char name[MAX_DAEMON_NAME_LEN];   /* name of this daemon */
    unsigned int daemon_id;           /* daemon id */
    int index_pos;           /* cp in **transactions */
    struct confd_trans_ctx **transactions;
    int trans_len;
    void *user_sessions;
    void *slock;
    int flags;               /* CONFD_DAEMON_FLAG...    */
};


struct confd_db_ctx {
    struct confd_daemon_ctx *dx;
    int lastop;
    int did;
    int qref;
    struct confd_user_info *uinfo;
    struct confd_error error;  /* user settable via confd_db_seterr*() */
};


/* the set_elem, create, remove, set_case, set_attr, and move_after */
/* callbacks may return CONFD_ACCUMULATE. If they do, the write ops */
/* will be queued up in the linked list tctx->accumulated           */

enum confd_tr_op {
    C_SET_ELEM = 1,
    C_CREATE= 2,
    C_REMOVE = 3,
    C_SET_CASE = 4,
    C_SET_ATTR = 5,
    C_MOVE_AFTER = 6
};

struct confd_tr_item {
    char *callpoint;
    enum confd_tr_op op;
    confd_hkeypath_t *hkp;
    confd_value_t *val;
    confd_value_t *choice;  /* only for set_case */
    u_int32_t attr;         /* only for set_attr */
    struct confd_tr_item *next;
};


/* For confd_cli confd_action_reply_completion() */

enum confd_completion_type {
    CONFD_COMPLETION,
    CONFD_COMPLETION_INFO,
    CONFD_COMPLETION_DESC,
    CONFD_COMPLETION_DEFAULT
};

struct confd_completion_value {
    enum confd_completion_type type;
    char *value;
    char *extra;
};

#ifdef __GNUC__
#define PRINTF(F,A) __attribute__ ((format (printf, F, A)))
#else
#define PRINTF(F,A)
#endif


extern struct confd_daemon_ctx *confd_init_daemon(const char *name);

extern int confd_set_daemon_flags(struct confd_daemon_ctx *dx, int flags);

extern void confd_release_daemon(struct confd_daemon_ctx *dx);

extern int confd_connect(struct confd_daemon_ctx *dx, int sock,
                         enum confd_sock_type type,
                         const struct sockaddr *srv, int addrsz);

extern int confd_register_trans_cb(struct confd_daemon_ctx *dx,
                                   const struct confd_trans_cbs *trans);

extern int confd_register_db_cb(struct confd_daemon_ctx *dx,
                                const struct confd_db_cbs *dbcbs);

extern int  confd_register_range_data_cb(struct confd_daemon_ctx *dx,
                                         const struct confd_data_cbs *data,
                                         const confd_value_t *lower,
                                         const confd_value_t *upper,
                                         int numkeys,
                                         const char *fmt, ...);

extern int confd_register_data_cb(struct confd_daemon_ctx *dx,
                                  const struct confd_data_cbs *data);

extern int confd_register_usess_cb(struct confd_daemon_ctx *dx,
                                   const struct confd_usess_cbs *ucb);

extern int ncs_register_service_cb(struct confd_daemon_ctx *dx,
                                   const struct ncs_service_cbs *scb);

extern int ncs_register_nano_service_cb(struct confd_daemon_ctx *dx,
                                        const char *component_type,
                                        const char *state,
                                        const struct ncs_nano_service_cbs *scb);

extern int confd_register_done(struct confd_daemon_ctx *dx);

extern int confd_fd_ready(struct confd_daemon_ctx *dx, int fd);

extern void confd_trans_set_fd(struct confd_trans_ctx *tctx, int sock);

extern int confd_data_reply_value(struct confd_trans_ctx *tctx,
                                  const confd_value_t *v);

extern int confd_data_reply_value_array(struct confd_trans_ctx *tctx,
                                        const confd_value_t *vs, int n);

extern int confd_data_reply_tag_value_array(struct confd_trans_ctx *tctx,
                                            const confd_tag_value_t *tvs,
                                            int n);

extern int confd_data_reply_next_key(struct confd_trans_ctx *tctx,
                                     const confd_value_t *v,
                                     int num_vals_in_key, long next);

extern int confd_data_reply_not_found(struct confd_trans_ctx *tctx);

extern int confd_data_reply_found(struct confd_trans_ctx *tctx);

extern int confd_data_reply_next_object_array(struct confd_trans_ctx *tctx,
                                              const confd_value_t *v, int n,
                                              long next);

extern int confd_data_reply_next_object_tag_value_array(
    struct confd_trans_ctx *tctx,
    const confd_tag_value_t *tv, int n,
    long next);

extern int confd_data_reply_next_object_arrays(
    struct confd_trans_ctx *tctx,
    const struct confd_next_object *obj,
    int nobj, int timeout_millisecs);

extern int confd_data_reply_next_object_tag_value_arrays(
    struct confd_trans_ctx *tctx,
    const struct confd_tag_next_object *tobj,
    int nobj, int timeout_millisecs);

extern int confd_data_reply_attrs(struct confd_trans_ctx *tctx,
                                  const confd_attr_value_t *attrs,
                                  int num_attrs);

extern int ncs_service_reply_proplist(struct confd_trans_ctx *tctx,
                                      const struct ncs_name_value *proplist,
                                      int num_props);

extern int ncs_nano_service_reply_proplist(struct confd_trans_ctx *tctx,
                                          const struct ncs_name_value *proplist,
                                          int num_props);

extern int confd_delayed_reply_ok(struct confd_trans_ctx *tctx);

extern int confd_delayed_reply_error(struct confd_trans_ctx *tctx,
                                     const char *errstr);

extern int confd_data_set_timeout(struct confd_trans_ctx *tctx,
                                  int timeout_secs);

extern void confd_trans_seterr(struct confd_trans_ctx *tctx,
                               const char *fmt, ...) PRINTF(2,3);

extern void confd_trans_seterr_extended(struct confd_trans_ctx *tctx,
                                        enum confd_errcode code,
                                        u_int32_t apptag_ns,
                                        u_int32_t apptag_tag,
                                        const char *fmt, ...) PRINTF(5,6);

extern int confd_trans_seterr_extended_info(
    struct confd_trans_ctx *tctx,
    enum confd_errcode code,
    u_int32_t apptag_ns,
    u_int32_t apptag_tag,
    confd_tag_value_t *error_info, int n,
    const char *fmt, ...) PRINTF(7,8);

extern void confd_db_seterr(struct confd_db_ctx *dbx,
                            const char *fmt, ...) PRINTF(2,3);

extern void confd_db_seterr_extended(struct confd_db_ctx *dbx,
                                     enum confd_errcode code,
                                     u_int32_t apptag_ns,
                                     u_int32_t apptag_tag,
                                     const char *fmt, ...) PRINTF(5,6);

extern int confd_db_seterr_extended_info(struct confd_db_ctx *dbx,
                                         enum confd_errcode code,
                                         u_int32_t apptag_ns,
                                         u_int32_t apptag_tag,
                                         confd_tag_value_t *error_info, int n,
                                         const char *fmt, ...) PRINTF(7,8);

extern int confd_db_set_timeout(struct confd_db_ctx *dbx, int timeout_secs);

extern int confd_aaa_reload(const struct confd_trans_ctx *tctx);

/* crypto support */
extern int confd_install_crypto_keys(struct confd_daemon_ctx* dtx);


/* ConfD Validation */
extern void confd_register_trans_validate_cb(
    struct confd_daemon_ctx *dx,
    const struct confd_trans_validate_cbs *vcbs);

extern int confd_register_valpoint_cb(struct confd_daemon_ctx *dx,
                                      const struct confd_valpoint_cb *vcb);

extern int confd_register_range_valpoint_cb(struct confd_daemon_ctx *dx,
                                            struct confd_valpoint_cb *vcb,
                                            const confd_value_t *lower,
                                            const confd_value_t *upper,
                                            int numkeys,
                                            const char *fmt, ...);

extern int confd_delayed_reply_validation_warn(struct confd_trans_ctx *tctx);

/* ConfD Actions */
extern int confd_register_action_cbs(struct confd_daemon_ctx *dx,
                                     const struct confd_action_cbs *acb);

extern int confd_register_range_action_cbs(struct confd_daemon_ctx *dx,
                                           const struct confd_action_cbs *acb,
                                           const confd_value_t *lower,
                                           const confd_value_t *upper,
                                           int numkeys,
                                           const char *fmt, ...);

extern void confd_action_set_fd(struct confd_user_info *uinfo, int sock);

extern void confd_action_seterr(struct confd_user_info *uinfo,
                                const char *fmt, ...) PRINTF(2, 3);

extern void confd_action_seterr_extended(struct confd_user_info *uinfo,
                                         enum confd_errcode code,
                                         u_int32_t apptag_ns,
                                         u_int32_t apptag_tag,
                                         const char *fmt, ...) PRINTF(5,6);

extern int confd_action_seterr_extended_info(
    struct confd_user_info *uinfo,
    enum confd_errcode code,
    u_int32_t apptag_ns,
    u_int32_t apptag_tag,
    confd_tag_value_t *error_info, int n,
    const char *fmt, ...) PRINTF(7,8);

extern int confd_action_reply_values(struct confd_user_info *uinfo,
                                     confd_tag_value_t *values, int nvalues);

extern int confd_action_reply_command(struct confd_user_info *uinfo,
                                      char **values, int nvalues);

extern int confd_action_reply_rewrite(struct confd_user_info *uinfo,
                                      char **values, int nvalues,
                                      char **unhides, int nunhides);

extern int confd_action_reply_rewrite2(struct confd_user_info *uinfo,
                                       char **values, int nvalues,
                                       char **unhides, int nunhides,
                                       struct confd_rewrite_select **selects,
                                       int nselects);

extern int confd_action_reply_completion(
    struct confd_user_info *uinfo,
    struct confd_completion_value *values, int nvalues);

extern int confd_action_reply_range_enum(
    struct confd_user_info *uinfo,
    char **values, int keysize, int nkeys);

extern int confd_action_delayed_reply_ok(struct confd_user_info *uinfo);

extern int confd_action_delayed_reply_error(struct confd_user_info *uinfo,
                                            const char *errstr);

extern int confd_action_set_timeout(struct confd_user_info *uinfo,
                                    int timeout_secs);


/* Notification streams */
extern int confd_register_notification_stream(
    struct confd_daemon_ctx *dx,
    const struct confd_notification_stream_cbs *ncbs,
    struct confd_notification_ctx **nctx);

extern int confd_notification_send(
    struct confd_notification_ctx *nctx,
    struct confd_datetime *time,
    confd_tag_value_t *values, int nvalues);

extern int confd_notification_replay_complete(
    struct confd_notification_ctx *nctx);

extern int confd_notification_replay_failed(
    struct confd_notification_ctx *nctx);

extern int confd_notification_reply_log_times(
    struct confd_notification_ctx *nctx,
    struct confd_datetime *creation,
    struct confd_datetime *aged);

extern void confd_notification_set_fd(
    struct confd_notification_ctx *nctx, int fd);

extern void confd_notification_set_snmp_src_addr(
    struct confd_notification_ctx *nctx,
    const struct confd_ip *src_addr);

extern int confd_notification_set_snmp_notify_name(
    struct confd_notification_ctx *nctx,
    const char *notify_name);

extern void confd_notification_seterr(
    struct confd_notification_ctx *nctx,
    const char *fmt, ...) PRINTF(2,3);

extern void confd_notification_seterr_extended(
    struct confd_notification_ctx *nctx,
    enum confd_errcode code,
    u_int32_t apptag_ns, u_int32_t apptag_tag,
    const char *fmt, ...) PRINTF(5,6);

extern int confd_notification_seterr_extended_info(
    struct confd_notification_ctx *nctx,
    enum confd_errcode code,
    u_int32_t apptag_ns,
    u_int32_t apptag_tag,
    confd_tag_value_t *error_info, int n,
    const char *fmt, ...) PRINTF(7,8);

extern int confd_register_snmp_notification(
    struct confd_daemon_ctx *dx, int fd,
    const char *notify_name, const char *ctx_name,
    struct confd_notification_ctx **nctx);

extern int confd_notification_send_snmp(
    struct confd_notification_ctx *nctx, const char *notification,
    struct confd_snmp_varbind *varbinds, int num_vars);

extern int confd_register_notification_snmp_inform_cb(
    struct confd_daemon_ctx *dx,
    const struct confd_notification_snmp_inform_cbs *cb);

extern int confd_notification_send_snmp_inform(
    struct confd_notification_ctx *nctx, const char *notification,
    struct confd_snmp_varbind *varbinds, int num_vars,
    const char *cb_id, int ref);

extern int confd_register_notification_sub_snmp_cb(
    struct confd_daemon_ctx *dx,
    const struct confd_notification_sub_snmp_cb *cb);

extern int confd_notification_flush(struct confd_notification_ctx *nctx);

/* authentication callback */
extern int confd_register_auth_cb(struct confd_daemon_ctx *dx,
                                  const struct confd_auth_cb *acb);

extern void confd_auth_seterr(struct confd_auth_ctx *actx,
                              const char *fmt, ...);


/* authorization callbacks */
extern int confd_register_authorization_cb(
    struct confd_daemon_ctx *dx,
    const struct confd_authorization_cbs *acb);

extern int confd_access_reply_result(struct confd_authorization_ctx *actx,
                                     int result);

extern int confd_authorization_set_timeout(struct confd_authorization_ctx *actx,
                                           int timeout_secs);

/* error formatting callback */
extern int confd_register_error_cb(struct confd_daemon_ctx *dx,
                                   const struct confd_error_cb *ecb);

extern void confd_error_seterr(struct confd_user_info *uinfo,
                               const char *fmt, ...);

#undef PRINTF

#ifdef __cplusplus
}
#endif
#endif
