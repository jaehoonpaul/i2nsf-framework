/*
 * Copyright 2005-2008 Tail-F Systems AB
 */


#ifndef _CONFD_CDB_H
#define _CONFD_CDB_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdarg.h>

enum cdb_db_type {
    CDB_RUNNING = 1,
    CDB_STARTUP = 2,
    CDB_OPERATIONAL = 3,
    CDB_PRE_COMMIT_RUNNING = 4
};

enum cdb_sub_type {
    CDB_SUB_RUNNING = 1,
    CDB_SUB_RUNNING_TWOPHASE = 2,
    CDB_SUB_OPERATIONAL = 3
};

enum cdb_sub_notification {
    CDB_SUB_PREPARE = 1,
    CDB_SUB_COMMIT = 2,
    CDB_SUB_ABORT = 3,
    CDB_SUB_OPER = 4
};


enum cdb_sock_type {
    CDB_READ_SOCKET,
    CDB_SUBSCRIPTION_SOCKET,
    CDB_DATA_SOCKET
};

enum cdb_phase_flags {
    CDB_FLAG_INIT = 1, /* CDB has an init transaction (when phase = 0) */
    CDB_FLAG_UPGRADE = 2 /* CDB has an upgrade transaction (when phase = 0) */
};

struct cdb_phase {
    int phase;
    int flags;
};

/* Flags for cdb_start_session2() / cdb_trigger_oper_subscriptions() */
#define CDB_LOCK_WAIT     (1 << 0)
#define CDB_LOCK_SESSION  (1 << 1)
#define CDB_LOCK_REQUEST  (1 << 2)
#define CDB_LOCK_PARTIAL  (1 << 3)

/* Flags for cdb_subscribe2() */
#define CDB_SUB_WANT_ABORT_ON_ABORT   (1 << 0)

/* Flags returned by cdb_read_subscription_socket2() */
#define CDB_SUB_FLAG_IS_LAST  (1 << 0)
#define CDB_SUB_FLAG_TRIGGER  (1 << 1)
#define CDB_SUB_FLAG_REVERT   (1 << 2)
#define CDB_SUB_FLAG_HA_SYNC  (1 << 3)
#define CDB_SUB_FLAG_HA_IS_SLAVE (1 << 4)

/* Flags used in cdb_get_modifications() */
#define CDB_GET_MODS_INCLUDE_LISTS     (1 << 0)
#define CDB_GET_MODS_REVERSE           (1 << 1) /* this flag is ignored for
                                                 cdb_get_modifications_iter() */
#define CDB_GET_MODS_SUPPRESS_DEFAULTS (1 << 2)


#define cdb_iter_op    confd_iter_op
#define cdb_iter_ret   confd_iter_ret

typedef enum cdb_iter_ret
    (cli_diff_iter_function_t)(confd_hkeypath_t *kp,
                               enum cdb_iter_op op,
                               confd_value_t *oldv,
                               confd_value_t *newv,
                               char *clistr,
                               int token_count,
                               struct confd_cli_token *tokens,
                               void *state);


enum  cdb_subscription_sync_type {
    CDB_DONE_PRIORITY =1, /* regular sync, we're all done with this prio ops */
    CDB_DONE_SOCKET =2, /* for this trans, don't send any more on this sock */
    CDB_DONE_TRANSACTION =3, /* for this trans, don't send anything else */
    CDB_DONE_OPERATIONAL =4 /* we're done with operational data notification */
};


#define MAXHOSTLEN 256

struct cdb_txid {
    u_int32_t       s1;
    u_int32_t       s2;
    u_int32_t       s3;
    char master[MAXHOSTLEN];  /* NUL terminated string */
};


extern int cdb_active_subscriptions;

#ifdef __GNUC__
#define PRINTF(F,A) __attribute__ ((format (printf, F, A)))
#else
#define PRINTF(F,A)
#endif


extern int cdb_connect(int sock,
                       enum cdb_sock_type type,
                       const struct sockaddr* srv, int srv_sz);
extern int cdb_connect_name(int sock,
                            enum cdb_sock_type type,
                            const struct sockaddr* srv, int srv_sz,
                            const char *name);
extern int cdb_mandatory_subscriber(int sock, const char *name);
extern int cdb_set_namespace(int sock, int hashed_ns);
extern int cdb_end_session(int sock);
extern int cdb_start_session(int sock, enum cdb_db_type db);
extern int cdb_start_session2(int sock, enum cdb_db_type db, int flags);
extern int cdb_close(int sock);
extern int cdb_wait_start(int sock);
extern int cdb_get_phase(int sock, struct cdb_phase *phase);
extern int cdb_get_txid(int sock, struct cdb_txid *txid);
extern int cdb_initiate_journal_compaction(int sock);
extern int cdb_load_file(int sock, const char *filename, int flags);
extern int cdb_load_str(int sock, const char *xml_str, int flags);
extern int cdb_get_user_session(int sock);
extern int cdb_get_transaction_handle(int sock);
extern int cdb_set_timeout(int sock, int timeout_secs);
extern int cdb_exists(int sock, const char *fmt, ...);
extern int cdb_cd(int sock, const char *fmt, ...);
extern int cdb_pushd(int sock, const char *fmt, ...);
extern int cdb_popd(int sock);
extern int cdb_getcwd(int sock, size_t strsz, char *curdir);
extern int cdb_getcwd_kpath(int sock, confd_hkeypath_t **kp);
extern int cdb_num_instances(int sock, const char *fmt, ...);
extern int cdb_next_index(int sock, const char *fmt, ...);
extern int cdb_index(int sock, const char *fmt, ...);
extern int cdb_is_default(int sock, const char *fmt, ...);
extern int cdb_subscribe2(int sock, enum cdb_sub_type type,
                          int flags, int priority,
                          int *spoint, int nspace, const char *fmt, ...);
extern int cdb_subscribe(int sock, int priority, int nspace,
                         int *spoint, const char *fmt, ...);
extern int cdb_oper_subscribe(int sock, int nspace,
                              int *spoint, const char *fmt, ...);
extern int cdb_subscribe_done(int sock);
extern int cdb_trigger_subscriptions(int sock, int sub_points[], int len);
extern int cdb_trigger_oper_subscriptions(int sock, int sub_points[], int len,
                                          int flags);
extern int cdb_diff_match(int sock, int subid,
                          struct xml_tag tags[], int tagslen);
extern int cdb_read_subscription_socket(int sock, int sub_points[],
                                        int *resultlen);
extern int cdb_read_subscription_socket2(int sock,
                                         enum cdb_sub_notification *type,
                                         int *flags,
                                         int *subpoints[], int *resultlen);
extern int cdb_replay_subscriptions(int sock, struct cdb_txid *txid,
                                    int sub_points[], int len);
extern int cdb_get_replay_txids(int sock,
                                struct cdb_txid **txid, int *resultlen);
extern int cdb_diff_iterate(int sock, int subid,
                            enum cdb_iter_ret (*iter)(confd_hkeypath_t *kp,
                                                      enum cdb_iter_op op,
                                                      confd_value_t *oldv,
                                                      confd_value_t *newv,
                                                      void *state),
                            int flags,
                            void *initstate);
extern int cdb_diff_iterate_resume(int sock, enum cdb_iter_ret reply,
                                   enum cdb_iter_ret (*iter)(
                                       confd_hkeypath_t *kp,
                                       enum cdb_iter_op op,
                                       confd_value_t *oldv,
                                       confd_value_t *newv,
                                       void *state),
                                   void *resumestate);
extern int cdb_cli_diff_iterate(int sock, int subid,
                                cli_diff_iter_function_t *iter,
                                int flags,
                                void *initstate);
extern int cdb_get_modifications(int sock, int subid, int flags,
                                 confd_tag_value_t **values, int *nvalues,
                                 const char *fmt, ...);
extern int cdb_get_modifications_iter(int sock, int flags,
                                      confd_tag_value_t **values, int *nvalues);
extern int cdb_get_modifications_cli(int sock, int subid,
                                     int flags, char **str);

extern int cdb_sync_subscription_socket(int sock,
                                        enum cdb_subscription_sync_type st);
extern int cdb_sub_progress(int sock, const char *fmt, ...);
extern int cdb_sub_abort_trans(int sock, enum confd_errcode code,
                               u_int32_t apptag_ns, u_int32_t apptag_tag,
                               const char *fmt, ...) PRINTF(5,6);
extern int cdb_sub_abort_trans_info(int sock, enum confd_errcode code,
                                    u_int32_t apptag_ns, u_int32_t apptag_tag,
                                    const confd_tag_value_t *error_info, int n,
                                    const char *fmt, ...) PRINTF(7,8);
extern int cdb_get_case(int sock, const char *choice,
                        confd_value_t *rcase, const char *fmt, ...);
extern int cdb_get(int sock, confd_value_t *v, const char *fmt, ...);
extern int cdb_get_int8(int sock, int8_t *rval, const char *fmt, ...);
extern int cdb_get_int16(int sock, int16_t *rval, const char *fmt, ...);
extern int cdb_get_int32(int sock, int32_t *rval, const char *fmt, ...);
extern int cdb_get_int64(int sock, int64_t *rval, const char *fmt, ...);
extern int cdb_get_u_int8(int sock, u_int8_t *rval, const char *fmt, ...);
extern int cdb_get_u_int16(int sock, u_int16_t *rval, const char *fmt, ...);
extern int cdb_get_u_int32(int sock, u_int32_t *rval, const char *fmt, ...);
extern int cdb_get_u_int64(int sock, u_int64_t *rval, const char *fmt, ...);
extern int cdb_get_bit32(int sock, u_int32_t *rval, const char *fmt, ...);
extern int cdb_get_bit64(int sock, u_int64_t *rval, const char *fmt, ...);
extern int cdb_get_bitbig(int sock, unsigned char **rval, int *bufsiz,
                          const char *fmt, ...);
extern int cdb_get_ipv4(int sock, struct in_addr *rval, const char *fmt, ...);
extern int cdb_get_ipv6(int sock, struct in6_addr *rval, const char *fmt, ...);
extern int cdb_get_double(int sock, double *rval, const char *fmt, ...);
extern int cdb_get_bool(int sock, int *rval, const char *fmt, ...);
extern int cdb_get_datetime(int sock, struct confd_datetime *rval,
                            const char *fmt, ...);
extern int cdb_get_date(int sock, struct confd_date *rval,
                        const char *fmt, ...);
extern int cdb_get_time(int sock, struct confd_time *rval,
                        const char *fmt, ...);
extern int cdb_get_duration(int sock, struct confd_duration *rval,
                            const char *fmt, ...);
extern int cdb_get_enum_value(int sock, int32_t *rval, const char *fmt, ...);
extern int cdb_get_objectref(int sock, confd_hkeypath_t **rval,
                             const char *fmt, ...);
extern int cdb_get_oid(int sock, struct confd_snmp_oid **rval,
                       const char *fmt, ...);
extern int cdb_get_buf(int sock, unsigned char **rval, int *bufsiz,
                       const char *fmt, ...);
extern int cdb_get_buf2(int sock, unsigned char *rval, int *n,
                        const char *fmt, ...);
extern int cdb_get_str(int sock, char *rval, int n, const char *fmt, ...);
extern int cdb_get_binary(int sock, unsigned char **rval, int *bufsiz,
                          const char *fmt, ...);
extern int cdb_get_hexstr(int sock, unsigned char **rval, int *bufsiz,
                          const char *fmt, ...);
extern int cdb_get_qname(int sock,
                  unsigned char **prefix, int *prefixsz,
                  unsigned char **name, int *namesz,
                  const char *fmt, ...);
extern int cdb_get_list(int sock, confd_value_t **values, int *n,
                        const char *fmt, ...);
extern int cdb_get_ipv4prefix(int sock, struct confd_ipv4_prefix *rval,
                              const char *fmt, ...);
extern int cdb_get_ipv6prefix(int sock, struct confd_ipv6_prefix *rval,
                              const char *fmt, ...);
extern int cdb_get_decimal64(int sock, struct confd_decimal64 *rval,
                             const char *fmt, ...);
extern int cdb_get_identityref(int sock, struct confd_identityref *rval,
                               const char *fmt, ...);
extern int cdb_get_ipv4_and_plen(int sock, struct confd_ipv4_prefix *rval,
                                 const char *fmt, ...);
extern int cdb_get_ipv6_and_plen(int sock, struct confd_ipv6_prefix *rval,
                                 const char *fmt, ...);
extern int cdb_get_dquad(int sock, struct confd_dotted_quad *rval,
                         const char *fmt, ...);
extern int cdb_vget(int sock, confd_value_t *v, const char *fmt, va_list args);
extern int cdb_get_object(int sock, confd_value_t *values, int n,
                          const char *fmt, ...);
extern int cdb_get_objects(int sock, confd_value_t *values, int n,
                           int ix, int nobj, const char *fmt, ...);
extern int cdb_get_values(int sock, confd_tag_value_t *values, int n,
                          const char *fmt, ...);

extern int cdb_set_elem(int sock, confd_value_t *val, const char *fmt, ...);
extern int cdb_set_elem2(int sock, const char *strval, const char *fmt, ...);
extern int cdb_vset_elem(int sock, confd_value_t *val,
                         const char *fmt, va_list args);
extern int cdb_set_case(int sock, const char *choice,
                        const char *scase, const char *fmt, ...);
extern int cdb_create(int sock, const char *fmt, ...);
extern int cdb_delete(int sock, const char *fmt, ...);
extern int cdb_set_object(int sock, const confd_value_t *values, int n,
                          const char *fmt, ...);
extern int cdb_set_values(int sock, const confd_tag_value_t *values, int n,
                          const char *fmt, ...);


#undef PRINTF

/* va_list variants of varargs functions above */

extern int cdb_vget_case(int sock, const char *choice, confd_value_t *rcase,
                         const char *fmt, va_list args);
extern int cdb_vcd(int sock, const char *fmt, va_list args);
extern int cdb_vpushd(int sock, const char *fmt, va_list args);
extern int cdb_vexists(int sock, const char *fmt, va_list args);
extern int cdb_vnum_instances(int sock, const char *fmt, va_list args);
extern int cdb_vnext_index(int sock, const char *fmt, va_list args);
extern int cdb_vindex(int sock, const char *fmt, va_list args);
extern int cdb_vis_default(int sock, const char *fmt, va_list args);
extern int cdb_vsubscribe2(int sock, enum cdb_sub_type type, int flags,
                           int priority, int *spoint, int nspace,
                           const char *fmt, va_list args);
extern int cdb_vsubscribe(int sock, int priority, int nspace, int *spoint,
                          const char *fmt, va_list args);
extern int cdb_voper_subscribe(int sock, int nspace, int *spoint,
                               const char *fmt, va_list args);
extern int cdb_vget_list(int sock, confd_value_t **values, int *n,
                         const char *fmt, va_list args);
extern int cdb_vget_int8(int sock, int8_t *rval, const char *fmt, va_list args);
extern int cdb_vget_int16(int sock, int16_t *rval,
                          const char *fmt, va_list args);
extern int cdb_vget_int32(int sock, int32_t *rval,
                          const char *fmt, va_list args);
extern int cdb_vget_int64(int sock, int64_t *rval,
                          const char *fmt, va_list args);
extern int cdb_vget_u_int8(int sock, u_int8_t *rval,
                           const char *fmt, va_list args);
extern int cdb_vget_u_int16(int sock, u_int16_t *rval,
                            const char *fmt, va_list args);
extern int cdb_vget_u_int32(int sock, u_int32_t *rval,
                            const char *fmt, va_list args);
extern int cdb_vget_u_int64(int sock, u_int64_t *rval,
                            const char *fmt, va_list args);
extern int cdb_vget_bit32(int sock, u_int32_t *rval,
                          const char *fmt, va_list args);
extern int cdb_vget_bit64(int sock, u_int64_t *rval,
                          const char *fmt, va_list args);
extern int cdb_vget_bitbig(int sock, unsigned char **rval, int *bufsiz,
                           const char *fmt, va_list args);
extern int cdb_vget_ipv4(int sock, struct in_addr *rval,
                         const char *fmt, va_list args);
extern int cdb_vget_ipv6(int sock, struct in6_addr *rval,
                         const char *fmt, va_list args);
extern int cdb_vget_double(int sock, double *rval,
                           const char *fmt, va_list args);
extern int cdb_vget_bool(int sock, int *rval, const char *fmt, va_list args);
extern int cdb_vget_datetime(int sock, struct confd_datetime *rval,
                             const char *fmt, va_list args);
extern int cdb_vget_date(int sock, struct confd_date *rval,
                         const char *fmt, va_list args);
extern int cdb_vget_time(int sock, struct confd_time *rval,
                         const char *fmt, va_list args);
extern int cdb_vget_duration(int sock, struct confd_duration *rval,
                             const char *fmt, va_list args);
extern int cdb_vget_enum_value(int sock, int32_t *rval,
                               const char *fmt, va_list args);
extern int cdb_vget_objectref(int sock, confd_hkeypath_t **rval,
                              const char *fmt, va_list args);
extern int cdb_vget_oid(int sock, struct confd_snmp_oid **rval,
                        const char *fmt, va_list args);
extern int cdb_vget_ipv4prefix(int sock, struct confd_ipv4_prefix *rval,
                               const char *fmt, va_list args);
extern int cdb_vget_ipv6prefix(int sock, struct confd_ipv6_prefix *rval,
                               const char *fmt, va_list args);
extern int cdb_vget_decimal64(int sock, struct confd_decimal64 *rval,
                              const char *fmt, va_list args);
extern int cdb_vget_identityref(int sock, struct confd_identityref *rval,
                                const char *fmt, va_list args);
extern int cdb_vget_ipv4_and_plen(int sock, struct confd_ipv4_prefix *rval,
                                  const char *fmt, va_list args);
extern int cdb_vget_ipv6_and_plen(int sock, struct confd_ipv6_prefix *rval,
                                  const char *fmt, va_list args);
extern int cdb_vget_dquad(int sock, struct confd_dotted_quad *rval,
                          const char *fmt, va_list args);
extern int cdb_vget_buf(int sock, unsigned char **rval, int *bufsiz,
                        const char *fmt, va_list args);
extern int cdb_vget_binary(int sock, unsigned char **rval, int *bufsiz,
                           const char *fmt, va_list args);
extern int cdb_vget_hexstr(int sock, unsigned char **rval, int *bufsiz,
                           const char *fmt, va_list args);
extern int cdb_vget_buf2(int sock, unsigned char *buf, int *n,
                  const char *fmt, va_list args);
extern int cdb_vget_str(int sock, char *buf, int n,
                        const char *fmt, va_list args);
extern int cdb_vget_qname(int sock,
                          unsigned char **prefix, int *prefixsz,
                          unsigned char **name, int *namesz,
                          const char *fmt, va_list args);
extern int cdb_vget_object(int sock, confd_value_t *values, int n,
                           const char *fmt, va_list args);
extern int cdb_vget_objects(int sock, confd_value_t *values,
                            int n, int ix, int nobj,
                            const char *fmt, va_list args);
extern int cdb_vget_values(int sock, confd_tag_value_t *values, int n,
                           const char *fmt, va_list args);
extern int cdb_vset_elem2(int sock, const char *strval,
                          const char *fmt, va_list args);
extern int cdb_vset_case(int sock, const char *choice, const char *scase,
                         const char *fmt, va_list args);
extern int cdb_vcreate(int sock, const char *fmt, va_list args);
extern int cdb_vdelete(int sock, const char *fmt, va_list args);
extern int cdb_vset_object(int sock, const confd_value_t *values, int n,
                           const char *fmt, va_list args);
extern int cdb_vset_values(int sock, const confd_tag_value_t *values, int n,
                           const char *fmt, va_list args);

/* for backwards compatibility */
#define cdb_get_enum_hash  cdb_get_enum_value
#define cdb_vget_enum_hash cdb_vget_enum_value

#ifdef __cplusplus
}
#endif
#endif
