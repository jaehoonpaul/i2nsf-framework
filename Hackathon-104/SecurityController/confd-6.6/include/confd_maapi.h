/*
 * Copyright 2005-2008 Tail-F Systems AB
 */

#ifndef _CONFD_MAAPI_H
#define _CONFD_MAAPI_H 1

#ifdef __cplusplus
extern "C" {
#endif

#define maapi_iter_op  confd_iter_op
#define maapi_iter_ret confd_iter_ret

#define CONFD_ECHO   1
#define CONFD_NOECHO 0

/* Flags for maapi_start_trans_flags() and maapi_set_flags() */
#define MAAPI_FLAG_HINT_BULK     (1 << 0)
#define MAAPI_FLAG_NO_DEFAULTS   (1 << 1)
#define MAAPI_FLAG_CONFIG_ONLY   (1 << 2)
#define MAAPI_FLAG_HIDE_INACTIVE (1 << 3) /* maapi_start_trans_flags() only */
#define MAAPI_FLAG_DELAYED_WHEN  (1 << 6) /* maapi_start_trans_flags() only */
#define MAAPI_FLAG_NO_CONFIG_CACHE   (1 << 4) // maapi_start_trans_flags() only
#define MAAPI_FLAG_CONFIG_CACHE_ONLY (1 << 5) // maapi_start_trans_flags() only

/* Flags for:
     maapi_apply_trans_flags()
     maapi_prepare_trans_flags()
     maapi_apply_trans_with_result()
*/
#define MAAPI_COMMIT_NCS_NO_REVISION_DROP           (1 << 2)
#define MAAPI_COMMIT_NCS_NO_DEPLOY                  (1 << 3)
#define MAAPI_COMMIT_NCS_NO_NETWORKING              (1 << 4)
#define MAAPI_COMMIT_NCS_NO_OUT_OF_SYNC_CHECK       (1 << 5)
/* The following three flags can only be set together with
   maapi_apply_trans_with_result() */
#define MAAPI_COMMIT_NCS_COMMIT_QUEUE_BYPASS        (1 << 6)
#define MAAPI_COMMIT_NCS_COMMIT_QUEUE_ASYNC         (1 << 8)
#define MAAPI_COMMIT_NCS_COMMIT_QUEUE_SYNC          (1 << 9)
#define MAAPI_COMMIT_NCS_NO_OVERWRITE               (1 << 10)
#define MAAPI_COMMIT_NCS_COMMIT_QUEUE_LOCK          (1 << 11)
#define MAAPI_COMMIT_NCS_COMMIT_QUEUE_BLOCK_OTHERS  (1 << 12)
#define MAAPI_COMMIT_NCS_COMMIT_QUEUE_ATOMIC        (1 << 13)
#define MAAPI_COMMIT_NCS_COMMIT_QUEUE_NONATOMIC     (1 << 14)
/* The following four flags can only be set together with
   maapi_apply_trans_with_result() */
#define MAAPI_COMMIT_NCS_DRY_RUN_XML                (1 << 15)
#define MAAPI_COMMIT_NCS_DRY_RUN_CLI                (1 << 16)
#define MAAPI_COMMIT_NCS_DRY_RUN_NATIVE             (1 << 17)
#define MAAPI_COMMIT_NCS_DRY_RUN_REVERSE            (1 << 18)
#define MAAPI_COMMIT_NCS_USE_LSA                    (1 << 19)
#define MAAPI_COMMIT_NCS_NO_LSA                     (1 << 20)

/* Deprecated flags - use MAAPI_COMMIT_NCS_COMMIT_QUEUE_* flags instead */
#define MAAPI_COMMIT_NCS_NO_FASTMAP                 (1 << 3)
#define MAAPI_COMMIT_NCS_BYPASS_COMMIT_QUEUE        (1 << 6)
#define MAAPI_COMMIT_NCS_THROUGH_COMMIT_QUEUE       (1 << 7)
#define MAAPI_COMMIT_NCS_ASYNC_COMMIT_QUEUE         (1 << 8)
#define MAAPI_COMMIT_NCS_SYNC_COMMIT_QUEUE          (1 << 9)

struct maapi_cursor {
    confd_value_t keys[MAXKEYLEN];
    int n;
    int hint_bulk;   // deprecated; use maapi_set_flags() instead
    char *secondary_index;

    /* internal fields */
    void *prevterm;
    void *path;
    int isrel;
    int sock;
    int thandle;
    int cursor_id;
};


struct maapi_rollback {
    int nr;
    char creator[MAXUSERNAMELEN];
    char datestr[255];
    char via[255];
    int fixed_nr;
    char label[255];
    char comment[255];
};

enum maapi_delete_how {
    MAAPI_DEL_SAFE     = 1,
    MAAPI_DEL_ALL      = 2,
    MAAPI_DEL_EXPORTED = 3
};

enum maapi_move_where {
    MAAPI_MOVE_FIRST  = 1,
    MAAPI_MOVE_BEFORE = 2,
    MAAPI_MOVE_AFTER  = 3,
    MAAPI_MOVE_LAST   = 4
};

enum ncs_commit_queue_status {
    NCS_COMMIT_QUEUE_NONE      = 0,
    NCS_COMMIT_QUEUE_ASYNC     = 1,
    NCS_COMMIT_QUEUE_COMPLETED = 2,
    NCS_COMMIT_QUEUE_TIMEOUT   = 3,
    NCS_COMMIT_QUEUE_DELETED   = 4,
    NCS_COMMIT_QUEUE_FAILED    = 5
};

struct ncs_commit_queue_result {
    int64_t      queue_id;
    enum ncs_commit_queue_status status;
};

/* Deprecated definitions for deprecated maapi_snmp_send_notification()
   - use corresponding confd_snmp_* definitions instead */
enum maapi_snmp_var_type {
    C_SNMP_VARIABLE =1,
    C_SNMP_OID      =2,
    C_SNMP_COL_ROW  =3};

struct snmp_oid {
    int oid[128];
    int len;
};

struct maapi_snmp_var {
    char varname[255];
    confd_value_t val;
};

struct maapi_snmp_oid {
    struct snmp_oid oid;
    confd_value_t val;
};

struct maapi_snmp_col_row {
    char column[255];
    struct snmp_oid rowindex;
    confd_value_t val;
};

struct maapi_snmp_varbind {
    enum maapi_snmp_var_type type;
    union {
        struct maapi_snmp_var var;
        struct maapi_snmp_oid oid;
        struct maapi_snmp_col_row cr;
    } binding;
};



#ifdef __GNUC__
#define PRINTF(F,A) __attribute__ ((format (printf, F, A)))
#else
#define PRINTF(F,A)
#endif

/* flags for maapi_save_config()  and maapi_load_config() */
#define MAAPI_CONFIG_XML               (1 << 0)
#define MAAPI_CONFIG_J                 (1 << 1)
#define MAAPI_CONFIG_C                 (1 << 2)
#define MAAPI_CONFIG_WITH_DEFAULTS     (1 << 3)
#define MAAPI_CONFIG_SHOW_DEFAULTS     (1 << 4)
#define MAAPI_CONFIG_C_IOS             (1 << 5)
#define MAAPI_CONFIG_MERGE             (1 << 6)
#define MAAPI_CONFIG_WITH_OPER         (1 << 7)
#define MAAPI_CONFIG_XPATH             (1 << 8)
#define MAAPI_CONFIG_XML_PRETTY        (1 << 9)
#define MAAPI_CONFIG_REPLACE           (1 << 10)
#define MAAPI_CONFIG_HIDE_ALL          (1 << 11)
#define MAAPI_CONFIG_UNHIDE_ALL        (1 << 12)
#define MAAPI_CONFIG_AUTOCOMMIT        (1 << 13)
#define MAAPI_CONFIG_CONTINUE_ON_ERROR (1 << 14)
#define MAAPI_CONFIG_SUPPRESS_ERRORS   (1 << 15)
#define MAAPI_CONFIG_XML_LOAD_LAX      (1 << 16)
#define MAAPI_CONFIG_JSON              (1 << 17)
#define MAAPI_CONFIG_WITH_SERVICE_META (1 << 18)
#define MAAPI_CONFIG_NO_PARENTS        (1 << 19)
#define MAAPI_CONFIG_OPER_ONLY         (1 << 20)

/* flags for maapi_cli_cmd2() */

#define MAAPI_CMD_NO_FULLPATH          (1 << 0)
#define MAAPI_CMD_NO_HIDDEN            (1 << 1)
#define MAAPI_CMD_NO_AAA               (1 << 2)

/* flags for maapi_cli_path_cmd() */
#define MAAPI_FLAG_EMIT_PARENTS       (1 << 0)
#define MAAPI_FLAG_DELETE             (1 << 1)
#define MAAPI_FLAG_NON_RECURSIVE      (1 << 2)

/* flags for maapi_init_upgrade() */
#define MAAPI_UPGRADE_KILL_ON_TIMEOUT  (1 << 0)

/* flags for maapi_shared_xxx() */
#define MAAPI_SHARED_NO_BACKPOINTER  (1 <<0)

/* Obscure (but valid) way of converting an integer to a string */
#define __MAAPI_SYM__(x) #x
#define __MAAPI_STR__(x) __MAAPI_SYM__(x)
/* For use in maapi_start_user_session* and maapi_start_trans* */
#define __MAAPI_CLIENT_ID__ __FILE__ ":" __MAAPI_STR__(__LINE__)

extern int maapi_connect(int sock,
                         const struct sockaddr* srv, int srv_sz);

extern int maapi_load_schemas(int sock);

extern int maapi_load_schemas_list(int sock, int flags,
                                   const u_int32_t *nshash,
                                   const int *nsflags, int num_ns);

extern int maapi_get_schema_file_path(int sock, char **buf);

extern int maapi_close(int sock);

/* user session api */
#define maapi_start_user_session(s, user, ctx, groups, n, addr, prot) \
    maapi_start_user_session3(s, user, ctx, groups, n, addr, 0, prot, \
                              NULL, NULL, NULL, __MAAPI_CLIENT_ID__)

#define maapi_start_user_session2(s, user, ctx, groups, n, addr, port, prot) \
    maapi_start_user_session3(s, user, ctx, groups, n, addr, port, prot, \
                              NULL, NULL, NULL, __MAAPI_CLIENT_ID__)

extern int maapi_start_user_session3(int sock,
                                     const char *username,
                                     const char *context,
                                     const char **groups, int numgroups,
                                     const struct confd_ip *src_addr,
                                     int src_port,
                                     enum confd_proto prot,
                                     const char *vendor,
                                     const char *product,
                                     const char *version,
                                     const char *client_id);

extern int maapi_end_user_session(int sock);

extern int maapi_kill_user_session(int sock, int usessid);

extern int maapi_get_user_sessions(int sock, int res[], int n);

extern int maapi_get_user_session(int sock, int usessid,
                                  struct confd_user_info *us);

extern int maapi_get_my_user_session_id(int sock);

extern int maapi_set_user_session(int sock, int usessid);

extern int maapi_get_user_session_identification(
    int sock, int usessid,
    struct confd_user_identification *uident);

extern int maapi_get_user_session_opaque(int sock, int usessid, char **opaque);

extern int maapi_get_authorization_info(int sock, int usessid,
                                       struct confd_authorization_info **ainfo);

extern int maapi_set_next_user_session_id(int sock, int usessid);

/* database api */
extern int maapi_lock(int sock, enum confd_dbname name);

extern int maapi_unlock(int sock, enum confd_dbname name);

extern int maapi_is_lock_set(int sock,  enum confd_dbname name);

extern int maapi_lock_partial(int sock, enum confd_dbname name,
                              char *xpaths[], int nxpaths, int *lockid);

extern int maapi_unlock_partial(int sock, int lockid);

extern int maapi_candidate_validate(int sock);

extern int maapi_delete_config(int sock,  enum confd_dbname name);

extern int maapi_candidate_commit(int sock);

extern int maapi_candidate_commit_persistent(int sock, const char *persist_id);

extern int maapi_candidate_commit_info(int sock,
                                       const char *persist_id,
                                       const char *label,
                                       const char *comment);

extern int maapi_candidate_confirmed_commit(int sock, int timeoutsecs);

extern int maapi_candidate_confirmed_commit_persistent(
    int sock, int timeoutsecs, const char *persist, const char *persist_id);

extern int maapi_candidate_confirmed_commit_info(
    int sock, int timeoutsecs, const char *persist, const char *persist_id,
    const char *label, const char *comment);

extern int maapi_candidate_abort_commit(int sock);

extern int maapi_candidate_abort_commit_persistent(int sock,
                                                   const char *persist_id);

extern int maapi_candidate_reset(int sock);

extern int maapi_confirmed_commit_in_progress(int sock);

extern int maapi_copy_running_to_startup(int sock);

extern int maapi_is_running_modified(int sock);

extern int maapi_is_candidate_modified(int sock);

/* transaction api */
#define maapi_start_trans(s, name, rw) \
    maapi_start_trans_flags2(s, name, rw, 0, 0, NULL, NULL, NULL, \
                             __MAAPI_CLIENT_ID__)

#define maapi_start_trans2(s, name, rw, usid) \
    maapi_start_trans_flags2(s, name, rw, usid, 0, NULL, NULL, NULL, \
                             __MAAPI_CLIENT_ID__)

#define maapi_start_trans_flags(s, name, rw, usid, flags) \
    maapi_start_trans_flags2(s, name, rw, usid, flags, NULL, NULL, NULL, \
                             __MAAPI_CLIENT_ID__)

extern int maapi_start_trans_flags2(int sock, enum confd_dbname dbname,
                                    enum confd_trans_mode readwrite,
                                    int usid, int flags,
                                    const char *vendor,
                                    const char *product,
                                    const char *version,
                                    const char *client_id);

extern int maapi_start_trans_in_trans(int sock, enum confd_trans_mode readwrite,
                                      int usid, int thandle);

extern int maapi_finish_trans(int sock, int thandle);

extern int maapi_validate_trans(int sock, int thandle, int unlock,
                                int forcevalidation);

extern int maapi_prepare_trans(int sock, int thandle);

extern int maapi_prepare_trans_flags(int sock, int thandle, int flags);

extern int maapi_commit_trans(int sock, int thandle);

extern int maapi_abort_trans(int sock, int thandle);

extern int maapi_apply_trans(int sock, int thandle, int keepopen);

extern int maapi_apply_trans_flags(int sock, int thandle,
                                   int keepopen, int flags);

extern int maapi_apply_trans_with_result(int sock, int thandle, int keepopen,
                                         int flags, const char *tag,
                                         int timeoutsecs,
                                         confd_tag_value_t **values,
                                         int *nvalues);

/* Deprecated function - use maapi_apply_trans_with_result() instead */
extern int maapi_commit_queue_result(int sock, int thandle, int timeoutsecs,
                                     struct ncs_commit_queue_result *result);

/* read/write api towards a transaction */
extern int maapi_set_namespace(int sock, int thandle, int hashed_ns);

extern int maapi_cd(int sock, int thandle, const char *fmt, ...);

extern int maapi_pushd(int sock, int thandle, const char *fmt, ...);

extern int maapi_popd(int sock, int thandle);

extern int maapi_getcwd(int sock, int thandle, size_t strsz, char *curdir);

extern int maapi_getcwd_kpath(int sock, int thandle, confd_hkeypath_t **kp);

extern int maapi_exists(int sock, int thandle, const char *fmt, ...);

extern int maapi_num_instances(int sock, int thandle, const char *fmt, ...);

extern int maapi_get_elem(int sock, int thandle, confd_value_t *v,
                          const char *fmt, ...);

extern int maapi_get_int8_elem(int sock, int thandle, int8_t *rval,
                               const char *fmt, ...);

extern int maapi_get_int16_elem(int sock, int thandle, int16_t *rval,
                                const char *fmt, ...);

extern int maapi_get_int32_elem(int sock, int thandle, int32_t *rval,
                                const char *fmt, ...);

extern int maapi_get_int64_elem(int sock, int thandle, int64_t *rval,
                                const char *fmt, ...);

extern int maapi_get_u_int8_elem(int sock, int thandle, u_int8_t *rval,
                                 const char *fmt, ...);

extern int maapi_get_u_int16_elem(int sock, int thandle, u_int16_t *rval,
                                  const char *fmt, ...);

extern int maapi_get_u_int32_elem(int sock, int thandle, u_int32_t *rval,
                                  const char *fmt, ...);

extern int maapi_get_u_int64_elem(int sock, int thandle, u_int64_t *rval,
                                  const char *fmt, ...);

extern int maapi_get_ipv4_elem(int sock, int thandle, struct in_addr *rval,
                               const char *fmt, ...);

extern int maapi_get_ipv6_elem(int sock, int thandle, struct in6_addr *rval,
                               const char *fmt, ...);

extern int maapi_get_double_elem(int sock, int thandle, double *rval,
                                 const char *fmt, ...);

extern int maapi_get_bool_elem(int sock, int thandle, int *rval,
                               const char *fmt, ...);

extern int maapi_get_datetime_elem(int sock, int thandle,
                                   struct confd_datetime *rval,
                                   const char *fmt, ...);

extern int maapi_get_date_elem(int sock, int thandle, struct confd_date *rval,
                               const char *fmt, ...);

extern int maapi_get_time_elem(int sock, int thandle,
                               struct confd_time *rval, const char *fmt, ...);

extern int maapi_get_duration_elem(int sock, int thandle,
                                   struct confd_duration *rval,
                                   const char *fmt, ...);

extern int maapi_get_enum_value_elem(int sock, int thandle, int32_t *rval,
                                     const char *fmt, ...);

extern int maapi_get_bit32_elem(int sock, int thandle, u_int32_t *rval,
                                const char *fmt, ...);

extern int maapi_get_bit64_elem(int sock, int thandle, u_int64_t *rval,
                                const char *fmt, ...);

extern int maapi_get_bitbig_elem(int sock, int thandle,
                                 unsigned char **rval, int *bufsiz,
                                 const char *fmt, ...);

extern int maapi_get_objectref_elem(int sock, int thandle,
                                    confd_hkeypath_t **rval,
                                    const char *fmt, ...);

extern int maapi_get_oid_elem(int sock, int thandle,
                              struct confd_snmp_oid **rval,
                              const char *fmt, ...);

extern int maapi_get_buf_elem(int sock, int thandle,
                              unsigned char **rval, int *bufsiz,
                              const char *fmt, ...);

extern int maapi_get_str_elem(int sock, int thandle, char *buf, int n,
                              const char *fmt, ...);

extern int maapi_get_binary_elem(int sock, int thandle,
                                 unsigned char **rval, int *bufsiz,
                                 const char *fmt, ...);

extern int maapi_get_hexstr_elem(int sock, int thandle,
                                 unsigned char **rval, int *bufsiz,
                                 const char *fmt, ...);

extern int maapi_get_qname_elem(int sock, int thandle,
                                unsigned char **prefix, int *prefixsz,
                                unsigned char **name, int *namesz,
                                const char *fmt, ...);

extern int maapi_get_list_elem(int sock, int thandle, confd_value_t **values,
                               int *n, const char *fmt, ...);

extern int maapi_get_ipv4prefix_elem(int sock, int thandle,
                                     struct confd_ipv4_prefix *rval,
                                     const char *fmt, ...);

extern int maapi_get_ipv6prefix_elem(int sock, int thandle,
                                     struct confd_ipv6_prefix *rval,
                                     const char *fmt, ...);

extern int maapi_get_decimal64_elem(int sock, int thandle,
                                    struct confd_decimal64 *rval,
                                    const char *fmt, ...);

extern int maapi_get_identityref_elem(int sock, int thandle,
                                      struct confd_identityref *rval,
                                      const char *fmt, ...);

extern int maapi_get_ipv4_and_plen_elem(int sock, int thandle,
                                        struct confd_ipv4_prefix *rval,
                                        const char *fmt, ...);

extern int maapi_get_ipv6_and_plen_elem(int sock, int thandle,
                                        struct confd_ipv6_prefix *rval,
                                        const char *fmt, ...);

extern int maapi_get_dquad_elem(int sock, int thandle,
                                struct confd_dotted_quad *rval,
                                const char *fmt, ...);


extern int maapi_vget_elem(int sock, int thandle, confd_value_t *v,
                           const char *fmt, va_list args);

extern int maapi_init_cursor(int sock, int thandle, struct maapi_cursor *mc,
                             const char *fmt, ...);

extern int maapi_get_next(struct maapi_cursor *mc);

extern int maapi_find_next(struct maapi_cursor *mc,
                           enum confd_find_next_type type,
                           confd_value_t *inkeys, int n_inkeys);

extern void maapi_destroy_cursor(struct maapi_cursor *mc);

extern int maapi_set_elem(int sock, int thandle,  confd_value_t *v,
                          const char *fmt, ...);

extern int maapi_set_elem2(int sock, int thandle, const char *strval,
                           const char *fmt, ...);

extern int maapi_vset_elem(int sock, int thandle, confd_value_t *v,
                           const char *fmt, va_list args);

extern int maapi_create(int sock, int thandle, const char *fmt, ...);

extern int maapi_delete(int sock, int thandle, const char *fmt, ...);

extern int maapi_get_object(int sock, int thandle, confd_value_t *values,
                            int n, const char *fmt, ...);

extern int maapi_get_objects(struct maapi_cursor *mc, confd_value_t *values,
                             int n, int *nobj);

extern int maapi_get_values(int sock, int thandle, confd_tag_value_t *values,
                            int n, const char *fmt, ...);

extern int maapi_set_object(int sock, int thandle, const confd_value_t *values,
                            int n, const char *fmt, ...);

extern int maapi_set_values(int sock, int thandle,
                            const confd_tag_value_t *values,
                            int n, const char *fmt, ...);

extern int maapi_get_case(int sock, int thandle, const char *choice,
                          confd_value_t *rcase, const char *fmt, ...);

extern int maapi_get_attrs(int sock, int thandle,
                           u_int32_t *attrs, int num_attrs,
                           confd_attr_value_t **attr_vals, int *num_vals,
                           const char *fmt, ...);

extern int maapi_set_attr(int sock, int thandle,
                          u_int32_t attr, confd_value_t *v,
                          const char *fmt, ...);

extern int maapi_delete_all(int sock, int thandle,
                            enum maapi_delete_how how);

extern int maapi_revert(int sock, int thandle);

extern int maapi_set_flags(int sock, int thandle, int flags);

extern int maapi_set_delayed_when(int sock, int thandle, int on);

extern int maapi_set_label(int sock, int thandle, const char *label);

extern int maapi_set_comment(int sock, int thandle, const char *comment);

/* miscellaneous */
extern int maapi_copy(int sock, int from_thandle, int to_thandle);

extern int maapi_copy_path(int sock, int from_thandle, int to_thandle,
                           const char *fmt, ...);

extern int maapi_copy_tree(int sock, int thandle,
                           const char *from, const char *tofmt, ...);

extern int maapi_insert(int sock, int thandle, const char *fmt, ...);

extern int maapi_move(int sock, int thandle, confd_value_t* tokey, int n,
                      const char *fmt, ...);

extern int maapi_move_ordered(int sock, int thandle,
                              enum maapi_move_where where,
                              confd_value_t* tokey, int n,
                              const char *fmt, ...);

extern int maapi_shared_create(int sock, int thandle, int flags,
                               const char *fmt, ...);

extern int maapi_shared_set_elem(int sock, int thandle, confd_value_t *v,
                                 int flags, const char *fmt, ...);

extern int maapi_shared_set_elem2(int sock, int thandle, const char *strval,
                                  int flags, const char *fmt, ...);

extern int maapi_shared_set_values(int sock, int thandle,
                                   const confd_tag_value_t *values, int n,
                                   int flags, const char *fmt, ...);

extern int maapi_shared_insert(int sock, int thandle, int flags,
                               const char *fmt, ...);

extern int maapi_shared_copy_tree(int sock, int thandle, int flags,
                                  const char *from, const char *tofmt, ...);

extern int maapi_ncs_apply_template(int sock, int thandle, char *template_name,
                                    const struct ncs_name_value *variables,
                                    int num_variables, int flags,
                                    const char *rootfmt, ...);

extern int maapi_shared_ncs_apply_template(
    int sock, int thandle, char *template_name,
    const struct ncs_name_value *variables,
    int num_variables, int flags,
    const char *rootfmt, ...);

extern int maapi_ncs_get_templates(int sock, char ***templates,
                                   int *num_templates);

extern int maapi_ncs_write_service_log_entry(int sock, const char *msg,
                                             confd_value_t *type,
                                             confd_value_t *level,
                                             const char *fmt, ...);

extern int maapi_authenticate(int sock, const char *user, const char *pass,
                              char *groups[], int n);

extern int maapi_authenticate2(int sock, const char *user, const char *pass,
                               const struct confd_ip *src_addr, int src_port,
                               const char *context, enum confd_proto prot,
                               char *groups[], int n);

extern int maapi_validate_token(int sock, const char *token,
                                const struct confd_ip *src_addr, int src_port,
                                const char *context, enum confd_proto prot,
                                char *groups[], int n);

extern int maapi_attach(int sock, int hashed_ns, struct confd_trans_ctx *ctx);

extern int maapi_attach2(int sock, int hashed_ns, int usid, int thandle);

extern int maapi_attach_init(int sock, int *thandle);

extern int maapi_detach(int sock, struct confd_trans_ctx *ctx);

extern int maapi_detach2(int sock, int thandle);

extern int maapi_diff_iterate(
    int sock, int thandle,
    enum maapi_iter_ret (*iter)(confd_hkeypath_t *kp,
                                enum maapi_iter_op op,
                                confd_value_t *oldv,
                                confd_value_t *newv,
                                void *state),
    int flags,
    void *initstate);

extern int maapi_keypath_diff_iterate(
    int sock, int thandle,
    enum maapi_iter_ret (*iter)(confd_hkeypath_t *kp,
                                enum maapi_iter_op op,
                                confd_value_t *oldv,
                                confd_value_t *newv,
                                void *state),
    int flags,
    void *initstate,
    const char *fmtpath, ...);

extern int maapi_diff_iterate_resume(
    int sock, enum maapi_iter_ret reply,
    enum maapi_iter_ret (*iter)(confd_hkeypath_t *kp,
                                enum maapi_iter_op op,
                                confd_value_t *oldv,
                                confd_value_t *newv,
                                void *state),
    void *resumestate);

extern int maapi_iterate(
    int sock, int thandle,
    enum maapi_iter_ret (*iter)(confd_hkeypath_t *kp,
                                confd_value_t *v,
                                confd_attr_value_t *attr_vals,
                                int num_attr_vals,
                                void *state),
    int flags,
    void *initstate,
    const char *fmtpath, ...);

extern int maapi_iterate_resume(
    int sock, enum maapi_iter_ret reply,
    enum maapi_iter_ret (*iter)(confd_hkeypath_t *kp,
                                confd_value_t *v,
                                confd_attr_value_t *attr_vals,
                                int num_attr_vals,
                                void *state),
    void *resumestate);

extern int maapi_get_running_db_status(int sock);

extern int maapi_set_running_db_status(int sock, int status);

extern int maapi_list_rollbacks(int sock, struct maapi_rollback *rp,
                                int *rp_size);

extern int maapi_load_rollback(int sock, int thandle, int rollback_num);

extern int maapi_request_action(int sock,
                                confd_tag_value_t *params, int nparams,
                                confd_tag_value_t **values, int *nvalues,
                                int hashed_ns, const char *fmt, ...);

extern int maapi_request_action_th(int sock, int thandle,
                                   confd_tag_value_t *params, int nparams,
                                   confd_tag_value_t **values, int *nvalues,
                                   const char *fmt, ...);

extern int maapi_request_action_str_th(int sock, int thandle,
                                       char **output,
                                       const char *cmd_fmt,
                                       const char *path_fmt,
                                       ...);

extern int maapi_xpath2kpath(int sock, const char *xpath,
                             confd_hkeypath_t **hkp);

extern int maapi_user_message(int sock, const char *to,
                              const char *message, const char *sender);

extern int maapi_sys_message(int sock, const char *to, const char *message);

extern int maapi_prio_message(int sock, const char *to, const char *message);

extern int maapi_cli_diff_cmd(int sock, int thandle, int thandle_old,
                              char *res, int size,
                              int flags, const char *fmt, ...);

extern int maapi_cli_accounting(int sock, const char *user, const int usid,
                                const char *cmdstr);

extern int maapi_cli_path_cmd(int sock, int thandle, char *res, int size,
                              int flags,
                              const char *fmt, ...);

extern int maapi_cli_cmd_to_path(int sock, const char *line,
                                 char *ns, int nsize,
                                 char *path, int psize);

extern int maapi_cli_cmd_to_path2(int sock, int thandle, const char *line,
                                  char *ns, int nsize,
                                  char *path, int psize);

extern int maapi_cli_prompt(int sock, int usess, const char *prompt,
                            int echo, char *res, int size);

extern int maapi_cli_prompt2(int sock, int usess, const char *prompt,
                             int echo, int timeout, char *res, int size);

extern int maapi_cli_prompt_oneof(int sock, int usess, const char *prompt,
                                  char **choice, int count,
                                  char *res, int size);
extern int maapi_cli_prompt_oneof2(int sock, int usess, const char *prompt,
                                   char **choice, int count, int timeout,
                                   char *res, int size);

extern int maapi_cli_read_eof(int sock, int usess, int echo, char *res,
                              int size);

extern int maapi_cli_read_eof2(int sock, int usess, int echo, int timeout,
                               char *res, int size);

extern int maapi_cli_write(int sock, int usess, const char *buf, int size);

extern int maapi_cli_cmd(int sock, int usess, const char *buf, int size);

extern int maapi_cli_cmd2(int sock, int usess, const char *buf, int size,
                          int flags);

extern int maapi_cli_cmd3(int sock, int usess, const char *buf, int size,
                          int flags, const char *unhide, int usize);

extern int maapi_cli_cmd4(int sock, int usess, const char *buf, int size,
                          int flags, char **unhide, int usize);

extern int maapi_cli_cmd_io(int sock, int usess, const char *buf,
                            int size, int flags, const char *unhide,
                            int usize);

extern int maapi_cli_cmd_io2(int sock, int usess, const char *buf,
                             int size, int flags, char **unhide,
                             int usize);

extern int maapi_cli_cmd_io_result(int sock, int id);

extern int maapi_cli_printf(int sock, int usess,
                            const char *fmt, ...) PRINTF(3,4);

extern int maapi_cli_vprintf(int sock, int usess, const char *fmt,
                             va_list args);

extern int maapi_cli_set(int sock, int usess, const char *opt,
                         const char *value);

extern int maapi_cli_get(int sock, int usess, const char *opt,
                         char *res, int size);

extern int maapi_set_readonly_mode(int sock, int flag);

extern int maapi_disconnect_remote(int sock, const char *address);

extern int maapi_disconnect_sockets(int sock, int *sockets, int nsocks);

extern int maapi_save_config(int sock, int thandle,  int flags,
                             const char *fmtpath, ...);

extern int maapi_save_config_result(int sock, int id);

extern int maapi_load_config(int sock, int thandle,  int flags,
                             const char *filename);

extern int maapi_load_config_cmds(int sock, int thandle, int flags,
                                  const char *cmds, const char *fmt, ...);

extern int maapi_load_config_stream(int sock, int thandle,  int flags);

extern int maapi_load_config_stream_result(int sock, int id);

extern int maapi_roll_config(int sock, int thandle,  const char *fmtpath, ...);

extern int maapi_roll_config_result(int sock, int id);

extern int maapi_get_stream_progress(int sock, int id);

extern int maapi_xpath_eval(int sock, int thandle, const char *expr,
                            int (*result)(confd_hkeypath_t *kp,
                                          confd_value_t *v,
                                          void *state),
                            void (*trace)(char *),
                            void *initstate,
                            const char *fmtpath, ...);

extern int maapi_xpath_eval_expr(int sock, int thandle,
                                 const char *expr, char **res,
                                 void (*trace)(char *),
                                 const char *fmtpath, ...);

extern int maapi_query_start(int sock, int thandle,
                             const char *expr,
                             const char *context_node,
                             int chunk_size,
                             int initial_offset,
                             enum confd_query_result_type result_as,
                             int nselect, const char *select[],
                             int nsort,   const char *sort[]);

extern int maapi_query_startv(int sock, int thandle,
                              const char *expr,
                              const char *context_node,
                              int chunk_size,
                              int initial_offset,
                              enum confd_query_result_type result_as,
                              int select_nparams,
                              ...);

extern int maapi_query_result(int sock, int qh,
                              struct confd_query_result **qrs);

extern int maapi_query_result_count(int sock, int qh);

extern int maapi_query_free_result(struct confd_query_result *qrs);

extern int maapi_query_reset_to(int sock, int qh, int offset);

extern int maapi_query_reset(int sock, int qh);

extern int maapi_query_stop(int sock, int qh);

extern int maapi_do_display(int sock, int thandle, const char *fmtpath, ...);

extern int maapi_install_crypto_keys(int sock);

extern int maapi_init_upgrade(int sock, int timeoutsecs, int flags);

extern int maapi_perform_upgrade(int sock, const char **loadpathdirs, int n);

extern int maapi_commit_upgrade(int sock);

extern int maapi_abort_upgrade(int sock);

extern int maapi_aaa_reload(int sock, int synchronous);

extern int maapi_aaa_reload_path(int sock, int synchronous,
                                 const char *fmt, ...);

extern int maapi_snmpa_reload(int sock, int synchronous);

extern int maapi_start_phase(int sock, int phase, int synchronous);

extern int maapi_wait_start(int sock, int phase);

extern int maapi_reload_config(int sock);

extern int maapi_reopen_logs(int sock);

extern int maapi_stop(int sock, int synchronous);

extern int maapi_rebind_listener(int sock, int listener);

extern int maapi_clear_opcache(int sock, const char *fmt, ...);


/* va_list variants of varargs functions above */

extern int maapi_vload_config_cmds(int sock, int thandle, int flags,
                                   const char *cmds, const char *fmt,
                                   va_list args);
extern int maapi_vexists(int sock, int thandle,
                         const char *fmt, va_list args);
extern int maapi_vnum_instances(int sock, int thandle,
                                const char *fmt, va_list args);
extern int maapi_vcreate(int sock, int thandle,
                         const char *fmt, va_list args);
extern int maapi_vdelete(int sock, int thandle,
                         const char *fmt, va_list args);
extern int maapi_vcopy_tree(int sock, int thandle, const char *from,
                            const char *tofmt, va_list args);
extern int maapi_vcopy_path(int sock, int from_thandle, int to_thandle,
                            const char *fmt, va_list args);
extern int maapi_vinsert(int sock, int thandle,
                         const char *fmt, va_list args);
extern int maapi_vmove(int sock, int thandle, confd_value_t* tokey, int n,
                       const char *fmt, va_list args);
extern int maapi_vmove_ordered(int sock, int thandle,
                               enum maapi_move_where where,
                               confd_value_t* tokey, int n,
                               const char *fmt, va_list args);
extern int maapi_vshared_create(int sock, int thandle, int flags,
                                const char *fmt, va_list args);
extern int maapi_vshared_set_elem(int sock, int thandle, confd_value_t *v,
                                  int flags, const char *fmt, va_list args);
extern int maapi_vshared_set_elem2(int sock, int thandle, const char *strval,
                                   int flags, const char *fmt, va_list args);
extern int maapi_vshared_insert(int sock, int thandle, int flags,
                                const char *fmt, va_list args);
extern int maapi_vshared_copy_tree(int sock, int thandle, int flags,
                                   const char *from, const char *tofmt,
                                   va_list args);
extern int maapi_vncs_apply_template(int sock, int thandle, char *template_name,
                                     const struct ncs_name_value *variables,
                                     int num_variables, int flags,
                                     const char *rootfmt, va_list args);
extern int maapi_vshared_ncs_apply_template(
    int sock, int thandle, char *template_name,
    const struct ncs_name_value *variables,
    int num_variables, int flags,
    const char *rootfmt, va_list args);
extern int maapi_vncs_write_service_log_entry(int sock, const char *msg,
                                              confd_value_t *type,
                                              confd_value_t *level,
                                              const char *fmt, va_list args);
extern int maapi_vset_elem2(int sock, int thandle, const char *strval,
                            const char *fmt, va_list args);
extern int maapi_vcd(int sock, int thandle,
                     const char *fmt, va_list args);
extern int maapi_vpushd(int sock, int thandle,
                        const char *fmt, va_list args);
extern int maapi_vinit_cursor(int sock, int thandle, struct maapi_cursor *mc,
                              const char *fmt, va_list args);
extern int maapi_vget_int8_elem(int sock, int thandle, int8_t *rval,
                                const char *fmt, va_list args);
extern int maapi_vget_int16_elem(int sock, int thandle, int16_t *rval,
                                 const char *fmt, va_list args);
extern int maapi_vget_int32_elem(int sock, int thandle, int32_t *rval,
                                 const char *fmt, va_list args);
extern int maapi_vget_int64_elem(int sock, int thandle, int64_t *rval,
                                 const char *fmt, va_list args);
extern int maapi_vget_u_int8_elem(int sock,int thandle, u_int8_t *rval,
                                  const char *fmt, va_list args);
extern int maapi_vget_u_int16_elem(int sock,int thandle,u_int16_t *rval,
                                   const char *fmt, va_list args);
extern int maapi_vget_u_int32_elem(int sock,int thandle,u_int32_t *rval,
                                   const char *fmt, va_list args);
extern int maapi_vget_u_int64_elem(int sock,int thandle,u_int64_t *rval,
                                   const char *fmt, va_list args);
extern int maapi_vget_ipv4_elem(int sock,int thandle,struct in_addr *rval,
                                const char *fmt, va_list args);
extern int maapi_vget_ipv6_elem(int sock,int thandle,struct in6_addr *rval,
                                const char *fmt, va_list args);
extern int maapi_vget_double_elem(int sock,int thandle,double *rval,
                                  const char *fmt, va_list args);
extern int maapi_vget_bool_elem(int sock, int thandle, int *rval,
                                const char *fmt, va_list args);
extern int maapi_vget_datetime_elem(int sock, int thandle,
                                    struct confd_datetime *rval,
                                    const char *fmt, va_list args);
extern int maapi_vget_date_elem(int sock, int thandle, struct confd_date *rval,
                                const char *fmt, va_list args);
extern int maapi_vget_time_elem(int sock, int thandle, struct confd_time *rval,
                                const char *fmt, va_list args);
extern int maapi_vget_duration_elem(int sock, int thandle,
                                    struct confd_duration *rval,
                                    const char *fmt, va_list args);
extern int maapi_vget_enum_value_elem(int sock, int thandle, int32_t *rval,
                                      const char *fmt, va_list args);
extern int maapi_vget_bit32_elem(int sock, int thandle, u_int32_t *rval,
                                 const char *fmt, va_list args);
extern int maapi_vget_bit64_elem(int sock, int thandle, u_int64_t *rval,
                                 const char *fmt, va_list args);
extern int maapi_vget_bitbig_elem(int sock, int thandle,
                                  unsigned char **rval, int *bufsiz,
                                  const char *fmt, va_list args);
extern int maapi_vget_objectref_elem(int sock, int thandle,
                                     confd_hkeypath_t **rval,
                                     const char *fmt, va_list args);
extern int maapi_vget_oid_elem(int sock, int thandle,
                               struct confd_snmp_oid **rval,
                               const char *fmt, va_list args);
extern int maapi_vget_ipv4prefix_elem(int sock, int thandle,
                                      struct confd_ipv4_prefix *rval,
                                      const char *fmt, va_list args);
extern int maapi_vget_ipv6prefix_elem(int sock, int thandle,
                                      struct confd_ipv6_prefix *rval,
                                      const char *fmt, va_list args);
extern int maapi_vget_decimal64_elem(int sock, int thandle,
                                     struct confd_decimal64 *rval,
                                     const char *fmt, va_list args);
extern int maapi_vget_identityref_elem(int sock, int thandle,
                                       struct confd_identityref *rval,
                                       const char *fmt, va_list args);
extern int maapi_vget_ipv4_and_plen_elem(int sock, int thandle,
                                         struct confd_ipv4_prefix *rval,
                                         const char *fmt, va_list args);
extern int maapi_vget_ipv6_and_plen_elem(int sock, int thandle,
                                         struct confd_ipv6_prefix *rval,
                                         const char *fmt, va_list args);
extern int maapi_vget_dquad_elem(int sock, int thandle,
                                 struct confd_dotted_quad *rval,
                                 const char *fmt, va_list args);
extern int maapi_vget_buf_elem(int sock, int thandle,
                               unsigned char **rval, int *bufsiz,
                               const char *fmt, va_list args);
extern int maapi_vget_binary_elem(int sock, int thandle,
                                  unsigned char **rval, int *bufsiz,
                                  const char *fmt, va_list args);
extern int maapi_vget_hexstr_elem(int sock, int thandle,
                                  unsigned char **rval, int *bufsiz,
                                  const char *fmt, va_list args);
extern int maapi_vget_list_elem(int sock, int thandle,
                                confd_value_t **values, int *n,
                                const char *fmt, va_list args);
extern int maapi_vget_str_elem(int sock, int thandle, char *buf, int n,
                               const char *fmt, va_list args);
extern int maapi_vget_qname_elem(int sock, int thandle,
                                 unsigned char **prefix, int *prefixsz,
                                 unsigned char **name, int *namesz,
                                 const char *fmt, va_list args);
extern int maapi_vget_object(int sock, int thandle, confd_value_t *values,
                             int n, const char *fmt, va_list args);
extern int maapi_vset_object(int sock, int thandle, const confd_value_t *values,
                             int n, const char *fmt, va_list args);
extern int maapi_vget_values(int sock, int thandle, confd_tag_value_t *values,
                             int n, const char *fmt, va_list args);
extern int maapi_vset_values(int sock, int thandle,
                             const confd_tag_value_t *values,
                             int n, const char *fmt, va_list args);
extern int maapi_vshared_set_values(int sock, int thandle,
                                    const confd_tag_value_t *values, int n,
                                    int flags, const char *fmt, va_list args);
extern int maapi_vget_case(int sock, int thandle, const char *choice,
                           confd_value_t *rcase, const char *fmt, va_list args);
extern int maapi_vget_attrs(int sock, int thandle,
                            u_int32_t *attrs, int num_attrs,
                            confd_attr_value_t **attr_vals, int *num_vals,
                            const char *fmt, va_list args);
extern int maapi_vset_attr(int sock, int thandle,
                           u_int32_t attr, confd_value_t *v,
                           const char *fmt, va_list args);
extern int maapi_vkeypath_diff_iterate(
    int sock, int thandle,
    enum maapi_iter_ret (*iter)(confd_hkeypath_t *kp,
                                enum maapi_iter_op op,
                                confd_value_t *oldv,
                                confd_value_t *newv,
                                void *state),
    int flags,
    void *initstate,
    const char *fmtpath, va_list args);
extern int maapi_viterate(
    int sock, int thandle,
    enum maapi_iter_ret (*iter)(confd_hkeypath_t *kp,
                                confd_value_t *v,
                                confd_attr_value_t *attr_vals,
                                int num_attr_vals,
                                void *state),
    int flags,
    void *initstate,
    const char *fmtpath, va_list args);
extern int maapi_vrequest_action(int sock,
                                 confd_tag_value_t *params, int nparams,
                                 confd_tag_value_t **values, int *nvalues,
                                 int hashed_ns, const char *fmt, va_list args);
extern int maapi_vrequest_action_th(int sock, int thandle,
                                    confd_tag_value_t *params, int nparams,
                                    confd_tag_value_t **values, int *nvalues,
                                    const char *fmt, va_list args);
extern int maapi_vrequest_action_str_th(int sock, int thandle, char **output,
                                        const char *cmd_fmt,
                                        const char *path_fmt,
                                        va_list args);
extern int maapi_vsave_config(int sock, int thandle,  int flags,
                              const char *fmtpath, va_list args);
extern int maapi_vroll_config(int sock, int thandle,
                              const char *fmtpath, va_list args);
extern int maapi_vdo_display(int sock, int thandle, const char
                             *fmtpath, va_list args);

extern int maapi_vcli_path_cmd(int sock, int thandle, char *res, int size,
                               int flags, const char *fmt, va_list args);

extern int maapi_vcli_diff_cmd(int sock, int thandle, int thandle_old,
                               char *res, int size,
                               int flags, const char *fmt, va_list args);

extern int maapi_vaaa_reload_path(int sock, int synchronous,
                                  const char *fmt, va_list args);

extern int maapi_vclear_opcache(int sock, const char *fmt, va_list args);

# if 0
/* a slew of typesafe version of  maapi_set_elem()  */
/* that are yet not implemented ....                */

extern int maapi_set_int8_elem(int sock, int thandle, int8_t *newval,
                               const char *fmt, ...);
extern int maapi_set_int16_elem(int sock, int thandle, int16_t *newval,
                                const char *fmt, ...);
extern int maapi_set_int32_elem(int sock, int thandle, int32_t *newval,
                                const char *fmt, ...);
extern int maapi_set_int64_elem(int sock, int thandle, int64_t *newval,
                                const char *fmt, ...);
extern int maapi_set_u_int8_elem(int sock, int thandle, u_int8_t *newval,
                                 const char *fmt, ...);
extern int maapi_set_u_int16_elem(int sock, int thandle, u_int16_t *newval,
                                  const char *fmt, ...);
extern int maapi_set_u_int32_elem(int sock, int thandle, u_int32_t *newval,
                                  const char *fmt, ...);
extern int maapi_set_u_int64_elem(int sock, int thandle, u_int64_t *newval,
                                  const char *fmt, ...);
extern int maapi_set_ipv4_elem(int sock, int thandle, struct in_addr *newval,
                               const char *fmt, ...);
extern int maapi_set_ipv6_elem(int sock, int thandle, struct in6_addr *newval,
                               const char *fmt, ...);
extern int maapi_set_double_elem(int sock, int thandle, double *newval,
                                 const char *fmt, ...);
extern int maapi_set_bool_elem(int sock, int thandle, int *newval,
                               const char *fmt, ...);
extern int maapi_set_datetime_elem(int sock, int thandle,
                                   struct confd_datetime *newval,
                                   const char *fmt, ...);
extern int maapi_set_date_elem(int sock, int thandle, struct confd_date *newval,
                               const char *fmt, ...);
extern int maapi_set_time_elem(int sock, int thandle,
                               struct confd_time *newval, const char *fmt, ...);
extern int maapi_set_duration_elem(int sock, int thandle,
                                   struct confd_duration *newval,
                                   const char *fmt, ...);
extern int maapi_set_enum_value_elem(int sock, int thandle, int32_t *newval,
                                     const char *fmt, ...);
extern int maapi_set_buf_elem(int sock, int thandle,
                              unsigned char **newval, int *bufsiz,
                              const char *fmt, ...);

extern int maapi_set_qname_elem(int sock, int thandle,
                                unsigned char **prefix, int *prefixsz,
                                unsigned char **name, int *namesz,
                                const char *fmt, ...);

#endif

/* for backwards compatibility */
#define maapi_get_enum_hash_elem  maapi_get_enum_value_elem
#define maapi_vget_enum_hash_elem maapi_vget_enum_value_elem



/* Deprecated function - use confd_register_snmp_notification()
   and confd_notification_send_snmp() instead */
extern int maapi_snmp_send_notification(int sock,
                           char *notification,
                           char *nofif_name,
                           char *ctx_name,
                           struct maapi_snmp_varbind *varbinds,
                           int num_vars);

/* Deprecated function - use maapi_set_flags() with MAAPI_FLAG_HINT_BULK */
extern int maapi_bulk_get_elem(int sock, int thandle, confd_value_t *v,
                               const char *fmt, ...);

/* Unsupported, for internal test */
extern int maapi_get_elem_no_defaults(int sock, int thandle, confd_value_t *v,
                                      const char *fmt, ...);


#undef PRINTF

#ifdef __cplusplus
}
#endif
#endif

