/*
 * Copyright 2005-2011 Tail-F Systems AB
 */

#ifndef _CONFD_LIB_H
#define _CONFD_LIB_H 1

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <assert.h>
#include <stdio.h>
#include <stdarg.h>
#if defined(__QNX__) || defined(__sun__)
#include <inttypes.h>
typedef uint8_t u_int8_t;
typedef uint16_t u_int16_t;
typedef uint32_t u_int32_t;
typedef uint64_t u_int64_t;
#endif

#define CONFD_PORT 4565       /* default portno where ConfD listens */
#define NCS_PORT   4569       /* default portno where NCS listens */

#ifndef MAXDEPTH
#define MAXDEPTH 20   /* max depth of model tree (max KP length + 1) */
#endif
#ifndef MAXKEYLEN
#define MAXKEYLEN 9   /* max number of key elems (max keys + 1) */
#endif

/* Defining this macro before including this file will
   replace the use of assert() in the CONFD_GET_XXX() macros */
#ifndef CONFD_ASSERT
#define CONFD_ASSERT(E) assert(E)
#endif

/* When a connection to ConfD has been made, these
   will have the actual numbers for the data model */
extern int confd_maxdepth;
extern int confd_maxkeylen;

#define CONFD_LIB_VSN           0x06060000
#define CONFD_LIB_VSN_STR       "06060000"
#define CONFD_LIB_API_VSN       0x06060000
#define CONFD_LIB_API_VSN_STR   "06060000"
#define CONFD_LIB_PROTO_VSN     66
#define CONFD_LIB_PROTO_VSN_STR "66"

#define NCS_PORT 4569

#define MAX_DAEMON_NAME_LEN 255

struct confd_trans_ctx;


struct confd_crypto_keys {
    char des3_key1[8];
    char des3_key2[8];
    char des3_key3[8];
    char des3_ivec[8];
    int des3_keys_initialized;

    char aes_key[16];
    char aes_ivec[16];
    int aes_keys_initialized;
};

extern struct  confd_crypto_keys confd_crypto_keys;


enum confd_dbname {
    CONFD_NO_DB = 0,
    CONFD_CANDIDATE = 1,
    CONFD_RUNNING = 2,
    CONFD_STARTUP = 3,
    CONFD_OPERATIONAL = 4,
    CONFD_TRANSACTION = 5,   /* trans_in_trans */
    CONFD_PRE_COMMIT_RUNNING = 6
};


/* Keep integers in sync with util/include/xsd.hrl */
/* also if change any int here, all CDB dbs die, so don't do that */
enum confd_vtype {
    C_NOEXISTS    = 1,  /* end marker                              */
    C_XMLTAG      = 2,  /* struct xml_tag                          */
    C_SYMBOL      = 3,  /* not yet used                            */
    C_STR         = 4,  /* NUL-terminated strings                  */
    C_BUF         = 5,  /* confd_buf_t (string ...)                */
    C_INT8        = 6,  /* int8_t    (int8)                        */
    C_INT16       = 7,  /* int16_t   (int16)                       */
    C_INT32       = 8,  /* int32_t   (int32)                       */
    C_INT64       = 9,  /* int64_t   (int64)                       */
    C_UINT8       = 10, /* u_int8_t  (uint8)                       */
    C_UINT16      = 11, /* u_int16_t (uint16)                      */
    C_UINT32      = 12, /* u_int32_t (uint32)                      */
    C_UINT64      = 13, /* u_int64_t (uint64)                      */
    C_DOUBLE      = 14, /* double (xs:float,xs:double)             */
    C_IPV4        = 15, /* struct in_addr in NBO                   */
                        /*  (inet:ipv4-address)                    */
    C_IPV6        = 16, /* struct in6_addr in NBO                  */
                        /*  (inet:ipv6-address)                    */
    C_BOOL        = 17, /* int       (boolean)                     */
    C_QNAME       = 18, /* struct confd_qname (xs:QName)           */
    C_DATETIME    = 19, /* struct confd_datetime                   */
                        /*  (yang:date-and-time)                   */
    C_DATE        = 20, /* struct confd_date (xs:date)             */
    C_TIME        = 23, /* struct confd_time (xs:time)             */
    C_DURATION    = 27, /* struct confd_duration (xs:duration)     */
    C_ENUM_VALUE  = 28, /* int32_t (enumeration)                   */
    C_BIT32       = 29, /* u_int32_t (bits size 32)                */
    C_BIT64       = 30, /* u_int64_t (bits size 64)                */
    C_LIST        = 31, /* confd_list (leaf-list)                  */
    C_XMLBEGIN    = 32, /* struct xml_tag, start of container or   */
                        /*  list entry                             */
    C_XMLEND      = 33, /* struct xml_tag, end of container or     */
                        /*  list entry                             */
    C_OBJECTREF   = 34, /* struct confd_hkeypath*                  */
                        /*  (instance-identifier)                  */
    C_UNION       = 35, /* (union) - not used in API functions     */
    C_PTR         = 36, /* see cdb_get_values in confd_lib_cdb(3)  */
    C_CDBBEGIN    = 37, /* as C_XMLBEGIN, with CDB instance index  */
    C_OID         = 38, /* struct confd_snmp_oid*                  */
                        /*  (yang:object-identifier)               */
    C_BINARY      = 39, /* confd_buf_t (binary ...)                */
    C_IPV4PREFIX  = 40, /* struct confd_ipv4_prefix                */
                        /*  (inet:ipv4-prefix)                     */
    C_IPV6PREFIX  = 41, /* struct confd_ipv6_prefix                */
                        /*  (inet:ipv6-prefix)                     */
    C_DEFAULT     = 42, /* default value indicator                 */
    C_DECIMAL64   = 43, /* struct confd_decimal64 (decimal64)      */
    C_IDENTITYREF = 44, /* struct confd_identityref (identityref)  */
    C_XMLBEGINDEL = 45, /* as C_XMLBEGIN, but for a deleted list   */
                        /*  entry                                  */
    C_DQUAD       = 46, /* struct confd_dotted_quad                */
                        /*  (yang:dotted-quad)                     */
    C_HEXSTR      = 47, /* confd_buf_t (yang:hex-string)           */
    C_IPV4_AND_PLEN = 48, /* struct confd_ipv4_prefix              */
                        /*  (tailf:ipv4-address-and-prefix-length) */
    C_IPV6_AND_PLEN = 49, /* struct confd_ipv6_prefix              */
                        /*  (tailf:ipv6-address-and-prefix-length) */
    C_BITBIG      = 50, /* confd_buf_t (bits size > 64)            */
    C_MAXTYPE           /* maximum marker; add new values above    */
};

/* for backwards compatibility */
#define C_ENUM_HASH C_ENUM_VALUE

/* used to represent buffers, e.g. YANG 'string' */
typedef struct confd_buf {
    unsigned int size;
    unsigned char *ptr;
} confd_buf_t;
/* alias with const */
typedef struct confd_buf_const {
    unsigned int size;
    unsigned const char *ptr;
} confd_buf_const_t;



/* This is used to represent the XML element tags */
/* in a confd_hkeypath_t                          */
struct xml_tag {
    u_int32_t tag;
    u_int32_t ns;
};

/* used to represent xs:qname */
struct confd_qname {
    confd_buf_t prefix;
    confd_buf_t name;
};

/* used to represent xs:dateTime */
#define CONFD_TIMEZONE_UNDEF -111

struct confd_datetime {
    int16_t year;
    u_int8_t month;
    u_int8_t day;
    u_int8_t hour;
    u_int8_t min;
    u_int8_t sec;
    u_int32_t micro;
    int8_t timezone;
    int8_t timezone_minutes;
};

/* used to represent xs:date */
struct confd_date {
    int16_t year;
    u_int8_t month;
    u_int8_t day;
    int8_t timezone;
    int8_t timezone_minutes;
};

/* used to represent xs:time */
struct confd_time {
    u_int8_t hour;
    u_int8_t min;
    u_int8_t sec;
    u_int32_t micro;
    int8_t timezone;
    int8_t timezone_minutes;
};

/* used to represent xs:duration */
struct confd_duration {
    u_int32_t years;
    u_int32_t months;
    u_int32_t days;
    u_int32_t hours;
    u_int32_t mins;
    u_int32_t secs;
    u_int32_t micros;
};

struct confd_value;
struct confd_hkeypath;

/* used to represent xs:list values */
struct confd_list {
    unsigned int size;
    struct confd_value *ptr;
};

/* reference to variable - only for cdb/maapi_get_values() */
struct confd_vptr {
    enum confd_vtype type;  /* as defined above */
    void *valp;
};

struct confd_snmp_oid {
    u_int32_t oid[128];
    int len;
};

struct confd_ipv4_prefix {
    struct in_addr ip;
    u_int8_t len;
};

struct confd_ipv6_prefix {
    struct in6_addr ip6;
    u_int8_t len;
};

struct confd_decimal64 {
    int64_t value;
    u_int8_t fraction_digits;
};

struct confd_identityref {
    u_int32_t ns;
    u_int32_t id;
};

struct confd_dotted_quad {
    unsigned char quad[4];
};

typedef struct confd_value {
    enum confd_vtype type;  /* as defined above */
    union {
        struct xml_tag xmltag;
        u_int32_t symbol;
        confd_buf_t buf;
        confd_buf_const_t c_buf;
        char *s;
        const char *c_s;
        int8_t i8;
        int16_t i16;
        int32_t i32;
        int64_t i64;
        u_int8_t u8;
        u_int16_t u16;
        u_int32_t u32;
        u_int64_t u64;
        double d;
        struct in_addr ip;
        struct in6_addr ip6;
        int boolean;
        struct confd_qname qname;
        struct confd_datetime datetime;
        struct confd_date date;
        struct confd_time time;
        struct confd_duration duration;
        int32_t enumvalue;
        u_int32_t b32;
        u_int64_t b64;
        struct confd_list list;
        struct confd_hkeypath *hkp;
        struct confd_vptr ptr;
        struct confd_snmp_oid *oidp;
        struct confd_ipv4_prefix ipv4prefix;
        struct confd_ipv6_prefix ipv6prefix;
        struct confd_decimal64 d64;
        struct confd_identityref idref;
        struct confd_dotted_quad dquad;
        u_int32_t enumhash;     /* backwards compat */
    } val;
} confd_value_t;


/* The array v[][] has to be last in the structure, code
   depends on this eg. confd_imm/confd_cursor */
typedef struct confd_hkeypath {
    int len;
    confd_value_t v[MAXDEPTH][MAXKEYLEN];
} confd_hkeypath_t;

typedef struct confd_tag_value {
    struct xml_tag tag;
    confd_value_t v;
} confd_tag_value_t;

struct confd_ip {
    int af;    /* AF_INET | AF_INET6 */
    union {
        struct in_addr v4;
        struct in6_addr v6;
    } ip;
};

/* CONFD_ATTR_TAGS: value is C_LIST of C_BUF/C_STR */
#define CONFD_ATTR_TAGS       0x80000000
/* CONFD_ATTR_ANNOTATION: value is C_BUF/C_STR */
#define CONFD_ATTR_ANNOTATION 0x80000001
/* CONFD_ATTR_INACTIVE: value is C_BOOL 1 (i.e. "true") */
#define CONFD_ATTR_INACTIVE   0x00000000
/* CONFD_ATTR_BACKPOINTER: value is C?LIST of C_BUF/C_STR */
#define CONFD_ATTR_BACKPOINTER 0x80000003


typedef struct confd_attr_value {
    u_int32_t attr;
    confd_value_t v;
} confd_attr_value_t;

enum confd_query_result_type {
    CONFD_QUERY_STRING = 0,
    CONFD_QUERY_HKEYPATH = 1,
    CONFD_QUERY_HKEYPATH_VALUE = 2,
    CONFD_QUERY_TAG_VALUE = 3
};

struct confd_query_result {
    enum confd_query_result_type type;
    int offset;
    int nresults;
    int nelements;
    union {
        char **str;
        confd_hkeypath_t *hkp;
        struct {
            confd_hkeypath_t hkp;
            confd_value_t    val;
        } *kv;
        confd_tag_value_t *tv;
    } *results;
    void *__internal;           /* confd_lib internal housekeeping */
};

enum confd_errcode {
    CONFD_ERRCODE_IN_USE = 0,
    CONFD_ERRCODE_RESOURCE_DENIED = 1,
    CONFD_ERRCODE_INCONSISTENT_VALUE = 2,
    CONFD_ERRCODE_ACCESS_DENIED = 3,
    CONFD_ERRCODE_APPLICATION = 4,
    CONFD_ERRCODE_APPLICATION_INTERNAL = 5,
    CONFD_ERRCODE_PROTO_USAGE = 6,       /* for internal libconfd use */
    CONFD_ERRCODE_INTERNAL = 7,          /* for internal libconfd use */
    CONFD_ERRCODE_DATA_MISSING = 8,
    CONFD_ERRCODE_INTERRUPT = 9
};

struct confd_error {
    enum confd_errcode code;
    struct xml_tag apptag;
    char *str;
    void *info;
};

struct confd_user_info;

/* Custom error formatting */

/* Error info for type CONFD_ERRTYPE_VALIDATION and CONFD_ERRTYPE_OPERATION */
struct confd_errinfo_validation {
    int code;  /* CONFD_ERR_NOTSET, CONFD_ERR_TOO_FEW_ELEMS, ... */
    union {
        struct {
            /* the element given by kp is not set */
            confd_hkeypath_t *kp;
        } notset;
        struct {
            /* kp has n instances, must be at least min */
            confd_hkeypath_t *kp;
            int n, min;
        } too_few_elems;
        struct {
            /* kp has n instances, must be at most max */
            confd_hkeypath_t *kp;
            int n, max;
        } too_many_elems;
        struct {
            /* the elements given by kps1 have the same set
               of values vals as the elements given by kps2
               (kps1, kps2, and vals point to n_elems long arrays) */
            int n_elems;
            confd_hkeypath_t *kps1;
            confd_hkeypath_t *kps2;
            confd_value_t *vals;
        } non_unique;
        struct {
            /* the element given by kp references
               the non-existing element given by ref
               Note: 'ref' may be NULL or have key elements without values
               (ref->v[n][0].type == C_NOEXISTS) if it cannot be instantiated */
            confd_hkeypath_t *kp;
            confd_hkeypath_t *ref;
        } bad_keyref;
        struct {
            /* the mandatory 'choice' statement choice in the
               container kp does not have a selected 'case' */
            confd_value_t *choice;
            confd_hkeypath_t *kp;
        } unset_choice;
        struct {
            /* the 'must' expression expr for element kp is not satisfied
               - error_message and and error_app_tag are NULL if not given
               in the 'must'; val points to the value of the element if it
               has one, otherwise it is NULL */
            char *expr;
            confd_hkeypath_t *kp;
            char *error_message;
            char *error_app_tag;
            confd_value_t *val;
        } must_failed;
        struct {
            /* the element kp has the instance-identifier value instance,
               which doesn't exist, but require-instance is 'true' */
            confd_hkeypath_t *kp;
            confd_hkeypath_t *instance;
        } missing_instance;
        struct {
            /* the element kp has the instance-identifier value instance,
               which doesn't conform to the specified path filters */
            confd_hkeypath_t *kp;
            confd_hkeypath_t *instance;
        } invalid_instance;
        struct {
            /* the expression for a configuration policy rule evaluated to
               'false' - error_message is the associated error message */
            char *error_message;
        } policy_failed;
        struct {
            /* the XPath expression expr, for the configuration policy
               rule with key name, could not be compiled due to msg */
            char *name;
            char *expr;
            char *msg;
        } policy_compilation_failed;
        struct {
            /* the expression expr, for the configuration policy rule
               with key name, failed XPath evaluation due to msg */
            char *name;
            char *expr;
            char *msg;
        } policy_evaluation_failed;
    } info;
    /* These are only provided for CONFD_ERRTYPE_VALIDATION */
    int test;            /* 1 if 'validate', 0 if 'commit' */
    struct confd_trans_ctx *tctx; /* only valid for duration of callback */
};

enum confd_errinfo_ptype {
    CONFD_ERRINFO_KEYPATH,
    CONFD_ERRINFO_STRING
};

struct confd_errinfo_param {
    enum confd_errinfo_ptype type;
    union {
        confd_hkeypath_t *kp;
        char *str;
    } val;
};

/* Error info for type CONFD_ERRTYPE_BAD_VALUE */
struct confd_errinfo_bad_value {
    int code;
    int n_params;
    struct confd_errinfo_param *params;
};

/* Error info for type CONFD_ERRTYPE_CLI */
struct confd_errinfo_cli {
    int code;
    int n_params;
    struct confd_errinfo_param *params;
};

/* Error info for type CONFD_ERRTYPE_MISC */
struct confd_errinfo_misc {
    int code;
    int n_params;
    struct confd_errinfo_param *params;
};

struct confd_errinfo {
    int type;  /* CONFD_ERRTYPE_XXX */
    union {
        struct confd_errinfo_validation validation;
        struct confd_errinfo_bad_value bad_value;
        struct confd_errinfo_cli cli;
        struct confd_errinfo_misc misc;
    } info;
};

enum confd_debug_level {
    CONFD_SILENT = 0,
    CONFD_DEBUG  = 1,
    CONFD_TRACE  = 2,      /* trace callback calls */
    CONFD_PROTO_TRACE = 3  /* tailf internal protocol trace */
};


#define MAXUSERNAMELEN 255
#define MAXCTXLEN      32

enum confd_proto {
    CONFD_PROTO_UNKNOWN = 0,
    CONFD_PROTO_TCP = 1,
    CONFD_PROTO_SSH = 2,
    CONFD_PROTO_SYSTEM = 3,  /* ConfD initiated transactions */
    CONFD_PROTO_CONSOLE = 4,
    CONFD_PROTO_SSL = 5,
    CONFD_PROTO_HTTP = 6,
    CONFD_PROTO_HTTPS = 7,
    CONFD_PROTO_UDP = 8 /* SNMP sessions */
};


enum confd_usess_lock_mode {
    CONFD_USESS_LOCK_MODE_NONE      = 0,
    CONFD_USESS_LOCK_MODE_PRIVATE   = 1,
    CONFD_USESS_LOCK_MODE_EXCLUSIVE = 2,
    CONFD_USESS_LOCK_MODE_SHARED    = 3
};

/* Set if the user session is in NETCONF or CLI forward mode. */
#define CONFD_USESS_FLAG_FORWARD            (1 << 0)
/* Set if the client has provided additional identification information,
   available through maapi_get_user_session_identification() */
#define CONFD_USESS_FLAG_HAS_IDENTIFICATION (1 << 1)
/* Set if the client has provided "opaque" information,
   available through maapi_get_user_session_opaque() */
#define CONFD_USESS_FLAG_HAS_OPAQUE         (1 << 2)

struct confd_daemon_ctx;
struct confd_action_cbs;

struct confd_action_ctx {
    int fd;                       /* action (worker) socket */
    struct confd_daemon_ctx *dx;  /* our daemon ctx        */
    void *t_opaque;               /* Private User data */
    struct confd_error error;     /* user settable via confd_action_seterr*() */
    void *cb_opaque;        /* private user data from callback registration */
    int thandle;            /* transaction handle if CLI callback (else -1) */
    char *actionpoint_opaque;   /* tailf:opaque for actionpoint in data model */
    /* ConfD internal fields                                                */
    int state;
    int last_op;
    int seen_reply;
    void *rterm;
    int query_ref;
    struct confd_action_cbs *acb;
};


struct confd_user_info {
    int af;                        /* AF_INET | AF_INET6 */
    union {
        struct in_addr v4;         /* address from where the */
        struct in6_addr v6;        /* user session originates */
    } ip;
    u_int16_t port;                /* source port */
    char username[MAXUSERNAMELEN]; /* who is the user */
    int usid;                      /* user session id */
    char context[MAXCTXLEN];       /* cli | webui | netconf | */
                                   /* noaaa | any MAAPI string */
    enum confd_proto proto;        /* which protocol */
    struct confd_action_ctx actx;  /* used during action call */
    time_t logintime;
    enum confd_usess_lock_mode lmode;  /* the lock we have (only from */
                                       /* maapi_get_user_session())   */
    char snmp_v3_ctx[255];         /* SNMP context for SNMP sessions */
                                   /* empty string ("") for non-SNMP sessions */
    char clearpass[255];           /* if have the pass, it's here */
                                   /* only if confd internal ssh is used */
    int flags;                     /* CONFD_USESS_FLAG_... */
    void *u_opaque;                /* Private User data */
    /* ConfD internal fields */
    char *errstr;                  /* for error formatting callback */
    int refc;
};


/* info from northbound agent
   - currently only SNMP request id */
union confd_request_data {
    struct {
        u_int32_t request_id;
    } snmp;
};

/* additional identification information
   via maapi_get_user_session_identification() */
struct confd_user_identification {
   char *vendor;
   char *product;
   char *version;
   char *client_identity;
};

/* authorization info via maapi_get_authorization_info */
struct confd_authorization_info {
    int ngroups;
    char **groups;
};


enum confd_trans_mode {
    CONFD_READ       = 1,
    CONFD_READ_WRITE = 2
};

#define CONFD_VALIDATION_FLAG_TEST    (1 << 0)
#define CONFD_VALIDATION_FLAG_COMMIT  (1 << 1)

struct confd_trans_ctx {
    int fd;                      /* trans (worker) socket */
    int vfd;                     /* validation worker socket */
    struct confd_daemon_ctx *dx; /* our daemon ctx */
    enum confd_trans_mode mode;
    enum confd_dbname dbname;
    struct confd_user_info *uinfo;
    void *t_opaque;              /* Private User data (transaction) */
    void *v_opaque;              /* Private User data (validation) */
    struct confd_error error;    /* user settable via */
                                 /* confd_trans_seterr*() */
    struct confd_tr_item *accumulated;
    int thandle;                 /* transaction handle */
    void *cb_opaque;             /* private user data from */
                                 /* data callback registration */
    void *vcb_opaque;            /* private user data from */
                                 /* validation callback registration */
    int secondary_index;         /* if != 0: secondary index number */
                                 /* for list traversal */
    int validation_info;         /* CONFD_VALIDATION_FLAG_XXX */
    char *callpoint_opaque;      /* tailf:opaque for callpoint
                                    in data model */
    char *validate_opaque;       /* tailf:opaque for validation point
                                    in data model */
    union confd_request_data request_data; /* info from northbound agent */
    int hide_inactive;           /* if != 0: config data with
                                    CONFD_ATTR_INACTIVE should be hidden */

    /* ConfD internal fields                            */
    int index;         /* array pos                       */
    int lastop;        /* remember what we were doing     */
    int last_proto_op; /* ditto */
    int seen_reply;    /* have we seen a reply msg        */
    int query_ref;     /* last query ref for this trans   */
    int in_num_instances;
    u_int32_t num_instances;
    long nextarg;
    struct confd_data_cbs *next_dcb;
    confd_hkeypath_t *next_kp;
    struct confd_tr_item *lastack; /* tail of acklist */
    int refc;
};



/* return values from all the the confd_ api functions */
/* rather than defining an enum, we use regular ints here */
/* looks better in the user code */

#define CONFD_DELAYED_RESPONSE 2
#define CONFD_ACCUMULATE       1
#define CONFD_OK               0
#define CONFD_ERR             -1
#define CONFD_EOF             -2
#define CONFD_VALIDATION_WARN -3
#define CONFD_ALREADY_LOCKED  -4
#define CONFD_IN_USE          -5

/* 'type' argument for find_next()/find_next_object()
   callbacks and maapi_find_next() */
enum confd_find_next_type {
    CONFD_FIND_NEXT = 0,
    CONFD_FIND_SAME_OR_NEXT = 1
};

enum confd_bool {
    CONFD_FALSE = 0,
    CONFD_TRUE = 1
};

struct confd_type_ctx {
    struct confd_nsinfo *nsmap;
    int num_ns;
    char *errstr;
};

struct confd_type {
    /* If a derived type point at the parent */
    struct confd_type *parent;

    /* not used in confspecs, but used in YANG */
    struct confd_type *defval;

    /* parse value located in str, and validate.
     * returns CONFD_TRUE if value is syntactically correct
     * and CONFD_FALSE otherwise.
     */
    int (*str_to_val)(struct confd_type *self,
                      struct confd_type_ctx *ctx,
                      const char *str, unsigned int len,
                      confd_value_t *v);

    /* print the value to str.
     * does not print more than len bytes, including trailing NUL.
     * return value as snprintf - i.e. if the value is correct for
     * the type, it returns the length of the string form regardless
     * of the len limit - otherwise it returns a negative number.
     * thus, the NUL terminated output has been completely written
     * if and only if the returned value is nonnegative and less
     * than len.
     * If strp is non-NULL and the string form is constant (i.e.
     * C_ENUM_VALUE), a pointer to the string is stored in *strp.
     */
    int (*val_to_str)(struct confd_type *self,
                      struct confd_type_ctx *ctx,
                      const confd_value_t *v,
                      char *str, unsigned int len,
                      const char **strp);

    /* returns CONFD_TRUE if value is correct, otherwise CONFD_FALSE
     */
    int (*validate)(struct confd_type *self,
                    struct confd_type_ctx *ctx,
                    const confd_value_t *v);

    /* data optionally used by the callbacks */
    void *opaque;
};

struct confd_type_cbs {
    char *typepoint;
    struct confd_type *type;
};

/* flag bits in confd_cs_node_info */
#define CS_NODE_IS_LIST          (1 << 0)
#define CS_NODE_IS_WRITE         (1 << 1)
#define CS_NODE_IS_CDB           (1 << 2)
#define CS_NODE_IS_ACTION        (1 << 3)
#define CS_NODE_IS_PARAM         (1 << 4)
#define CS_NODE_IS_RESULT        (1 << 5)
#define CS_NODE_IS_NOTIF         (1 << 6)
#define CS_NODE_IS_CASE          (1 << 7)
#define CS_NODE_IS_CONTAINER     (1 << 8)
#define CS_NODE_HAS_WHEN         (1 << 9)
#define CS_NODE_HAS_DISPLAY_WHEN (1 << 10)
#define CS_NODE_HAS_META_DATA    (1 << 11)
#define CS_NODE_IS_WRITE_ALL     (1 << 12)
#define CS_NODE_IS_LEAF_LIST     (1 << 13)
#define CS_NODE_IS_LEAFREF       (1 << 14)
#define CS_NODE_IS_DYN CS_NODE_IS_LIST /* backwards compat */

/* cmp values in confd_cs_node_info */
#define CS_NODE_CMP_NORMAL        0
#define CS_NODE_CMP_SNMP          1
#define CS_NODE_CMP_SNMP_IMPLIED  2
#define CS_NODE_CMP_USER          3
#define CS_NODE_CMP_UNSORTED      4

struct confd_cs_node_info {
    u_int32_t *keys;
    int minOccurs;
    int maxOccurs;   /* -1 if unbounded */
    enum confd_vtype shallow_type;
    struct confd_type *type;
    confd_value_t *defval;
    struct confd_cs_choice *choices;
    int flags;
    u_int8_t cmp;
    struct confd_cs_meta_data *meta_data;
};

struct confd_cs_meta_data {
    char* key;
    char* value;
};

struct confd_cs_node {
    u_int32_t tag;
    u_int32_t ns;
    struct confd_cs_node_info info;
    struct confd_cs_node *parent;
    struct confd_cs_node *children;
    struct confd_cs_node *next;
    void *opaque;   /* private user data */
};

struct confd_cs_choice {
    u_int32_t tag;
    u_int32_t ns;
    int minOccurs;
    struct confd_cs_case *default_case;
    struct confd_cs_node *parent;         /* NULL if parent is case */
    struct confd_cs_case *cases;
    struct confd_cs_choice *next;
    struct confd_cs_case *case_parent;    /* NULL if parent is node */
};

struct confd_cs_case {
    u_int32_t tag;
    u_int32_t ns;
    struct confd_cs_node *first;
    struct confd_cs_node *last;
    struct confd_cs_choice *parent;
    struct confd_cs_case *next;
    struct confd_cs_choice *choices;
};

struct confd_nsinfo {
    const char *uri;
    const char *prefix;
    u_int32_t hash;
    const char *revision;
    const char *module;
};

/* NCS service properties and template variables */
struct ncs_name_value {
    char *name;
    char *value;
};

/* Flags for (maapi|confd)_load_schemas_list() */
#define CONFD_LOAD_SCHEMA_NODES        (1 << 0)
#define CONFD_LOAD_SCHEMA_TYPES        (1 << 1)
#define CONFD_LOAD_SCHEMA_HASH        (1 << 16)

/* 'listener' values for maapi_rebind_listener() */
#define CONFD_LISTENER_IPC     (1 << 0)
#define CONFD_LISTENER_NETCONF (1 << 1)
#define CONFD_LISTENER_SNMP    (1 << 2)
#define CONFD_LISTENER_CLI     (1 << 3)
#define CONFD_LISTENER_WEBUI   (1 << 4)


/* Definitions for snmp notifications */
enum confd_snmp_var_type {
    CONFD_SNMP_VARIABLE = 1,
    CONFD_SNMP_OID      = 2,
    CONFD_SNMP_COL_ROW  = 3
};

struct confd_snmp_col_row {
    char column[256];
    struct confd_snmp_oid rowindex;
};

enum confd_snmp_type {
    CONFD_SNMP_NULL = 0,
    CONFD_SNMP_INTEGER = 1,
    CONFD_SNMP_Interger32 = 2,
    CONFD_SNMP_OCTET_STRING = 3,
    CONFD_SNMP_OBJECT_IDENTIFIER = 4,
    CONFD_SNMP_IpAddress = 5,
    CONFD_SNMP_Counter32 = 6,
    CONFD_SNMP_TimeTicks = 7,
    CONFD_SNMP_Opaque = 8,
    CONFD_SNMP_Counter64 = 9,
    CONFD_SNMP_Unsigned32 = 10
};

struct confd_snmp_varbind {
    enum confd_snmp_var_type type;
    union {
        char name[256];
        struct confd_snmp_oid oid;
        struct confd_snmp_col_row cr;
    } var;
    confd_value_t val;
    enum confd_snmp_type vartype; /* ignored when we send traps from the lib */
};


/* Types used by maapi_diff_iterate() and cdb_diff_iterate() */
enum confd_iter_op {
    MOP_CREATED = 1,
    MOP_DELETED = 2,
    MOP_MODIFIED = 3,
    MOP_VALUE_SET = 4,
    MOP_MOVED_AFTER = 5,
    MOP_ATTR_SET = 6       /* maapi_diff_iterate() only */
};

enum confd_iter_ret {
    ITER_STOP = 1,
    ITER_RECURSE = 2,
    ITER_CONTINUE = 3,
    ITER_SUSPEND = 4,
    ITER_UP = 5
};

enum confd_iter_flags {     /* bitmask */
    ITER_WANT_PREV = (1<<0),            /* cdb_diff_iterate() only */
    ITER_WANT_ANCESTOR_DELETE = (1<<1), /* cdb_diff_iterate() only */
    ITER_WANT_ATTR = (1<<2),            /* maapi_diff_iterate() only */
    ITER_WANT_CLI_STR = (1<<3),         /* internal use */
    ITER_WANT_SCHEMA_ORDER = (1<<4),    /* cdb_diff_iterate() only*/
    ITER_WANT_LEAF_FIRST_ORDER = (1<<5),/* cdb_diff_iterate() only*/
    ITER_WANT_LEAF_LAST_ORDER = (1<<6), /* cdb_diff_iterate() only*/
    ITER_WANT_REVERSE = (1<<7),         /* cdb_diff_iterate() only*/
    ITER_WANT_P_CONTAINER = (1<<8),     /* maapi_diff_iterate() only */
    ITER_WANT_LEAF_LIST_AS_LEAF = (1<<9)/* DEPRECATED */
};

struct confd_cli_token {
    confd_value_t val;
    char *string;
};

/* return values from confd_hkp_tagmatch() */
#define CONFD_HKP_MATCH_NONE 0
#define CONFD_HKP_MATCH_TAGS (1 << 0)
#define CONFD_HKP_MATCH_HKP  (1 << 1)
#define CONFD_HKP_MATCH_FULL (CONFD_HKP_MATCH_TAGS|CONFD_HKP_MATCH_HKP)

/* Flags for confd_mmap_schemas_setup() */
#define CONFD_MMAP_SCHEMAS_KEEP_SIZE    (1 << 0)
#define CONFD_MMAP_SCHEMAS_FIXED_ADDR   (2 << 0) /* MAP_FIXED - dangerous! */

enum confd_serializable_type {
    CONFD_SERIAL_NONE      = 0,
    CONFD_SERIAL_VALUE_T   = 1,
    CONFD_SERIAL_HKEYPATH  = 2,
    CONFD_SERIAL_TAG_VALUE = 3
};

struct confd_deserializable {
    enum confd_serializable_type type;
    union {
        confd_value_t value;
        confd_hkeypath_t hkp;
        confd_tag_value_t tval;
    } u;
    void *internal;  // internal structure containing memory
                     // for the above datatypes to point _into_
                     // freed by a call to confd_deserialize_free()
};

struct confd_serializable {
    enum confd_serializable_type type;
    union {
        confd_value_t *value;
        confd_hkeypath_t *hkp;
        confd_tag_value_t *tval;
    } u;
};


#ifdef __GNUC__
#define PRINTF(F,A) __attribute__ ((format (printf, F, A)))
#else
#define PRINTF(F,A)
#endif

extern void confd_init_vsn_sz(const char *name, FILE *estream,
                              const enum confd_debug_level debug,
                              int vsn, int maxdepth, int maxkeylen);

#define confd_init(name, estream, debug) \
    confd_init_vsn_sz((name), (estream), (debug), \
                      CONFD_LIB_API_VSN, MAXDEPTH, MAXKEYLEN)

extern int* confd_errno_location(void);
#define confd_errno (*confd_errno_location ())

extern int confd_set_debug(enum confd_debug_level debug, FILE *estream);

extern void confd_fatal(const char *fmt, ...) PRINTF(1,2);

extern int confd_load_schemas(const struct sockaddr* srv, int srv_sz);

extern int confd_load_schemas_list(const struct sockaddr* srv, int srv_sz,
                                   int flags,
                                   const u_int32_t *nshash, const int *nsflags,
                                   int num_ns);

extern int confd_mmap_schemas_setup(void *addr, size_t size,
                                    const char *filename, int flags);

extern int confd_mmap_schemas(const char *filename);

extern void confd_free_schemas(void);

extern int confd_svcmp(const char *s, const confd_value_t *v);

extern int confd_pp_value(char *buf, int bufsiz, const confd_value_t *v);

extern int confd_ns_pp_value(char *buf, int bufsiz,
                             const confd_value_t *v, int ns);

extern int confd_pp_kpath(char *buf, int bufsiz,
                          const confd_hkeypath_t *hkeypath);

extern int confd_pp_kpath_len(char *buf, int bufsiz,
                              const confd_hkeypath_t *hkeypath, int len);

extern char *confd_xmltag2str(u_int32_t ns, u_int32_t xmltag);

extern int confd_xpath_pp_kpath(char *buf, int bufsiz, u_int32_t ns,
                                const confd_hkeypath_t *hkeypath);

extern int confd_format_keypath(char *buf, int bufsiz, const char *fmt, ...);

extern int confd_vformat_keypath(char *buf, int bufsiz, const char *fmt,
                                 va_list ap);

extern int confd_get_nslist(struct confd_nsinfo **listp);

extern char *confd_ns2prefix(u_int32_t ns);

extern char *confd_hash2str(u_int32_t hash);

extern u_int32_t confd_str2hash(const char *str);

extern struct confd_cs_node *confd_find_cs_root(int ns);

extern struct confd_cs_node *confd_find_cs_node(
    const confd_hkeypath_t *hkeypath, int len);

extern struct confd_cs_node *confd_find_cs_node_child(
    const struct confd_cs_node *parent, struct xml_tag xmltag);

extern struct confd_cs_node *confd_cs_node_cd(
    const struct confd_cs_node *start, const char *fmt, ...);

extern int confd_max_object_size(struct confd_cs_node *object);

extern struct confd_cs_node *confd_next_object_node(
    struct confd_cs_node *object, struct confd_cs_node *cur,
    confd_value_t *value);

extern struct confd_type *confd_find_ns_type(u_int32_t nshash,
                                             const char *name);

extern struct confd_type *confd_get_leaf_list_type(struct confd_cs_node *node);

extern int confd_val2str(struct confd_type *type, const confd_value_t *val,
                         char *buf, int bufsiz);

extern int confd_str2val(struct confd_type *type, const char *str,
                         confd_value_t *val);

extern char *confd_val2str_ptr(struct confd_type *type,
                               const confd_value_t *val);

extern int confd_get_decimal64_fraction_digits(struct confd_type *type);

extern int confd_get_bitbig_size(struct confd_type *type);

extern int confd_hkp_tagmatch(struct xml_tag tags[], int tagslen,
                              confd_hkeypath_t *hkp);

extern int confd_hkp_prefix_tagmatch(struct xml_tag tags[], int tagslen,
                                     confd_hkeypath_t *hkp);

extern int confd_val_eq(const confd_value_t *v1, const confd_value_t *v2);

extern void confd_free_value(confd_value_t *v);

extern confd_value_t *confd_value_dup_to(const confd_value_t *v,
                                         confd_value_t *newv);

extern void confd_free_dup_to_value(confd_value_t *v);

extern confd_value_t *confd_value_dup(const confd_value_t *v);

extern void confd_free_dup_value(confd_value_t *v);

extern confd_hkeypath_t *confd_hkeypath_dup(const confd_hkeypath_t *src);

extern confd_hkeypath_t *confd_hkeypath_dup_len(const confd_hkeypath_t *src,
                                                int len);

extern void confd_free_hkeypath(confd_hkeypath_t *hkp);

extern void confd_free_authorization_info(
    struct confd_authorization_info *ainfo);

extern char *confd_lasterr(void);

extern char *confd_strerror(int code);

extern struct xml_tag *confd_last_error_apptag(void);


/* User-defined types */
extern int confd_register_ns_type(u_int32_t nshash, const char *name,
                                  struct confd_type *type);

extern int confd_register_node_type(struct confd_cs_node *node,
                                    struct confd_type *type);

extern int confd_type_cb_init(struct confd_type_cbs **cbs);


/* crypto support */
extern int confd_decrypt(const char *ciphertext, int len, char *output);


/* stream socket */
extern int confd_stream_connect(int sock, const struct sockaddr* srv,
                                int srv_sz, int id, int flags);

/* marshall */
extern int confd_deserialize(struct confd_deserializable *s,
                             unsigned char *buf);

extern int confd_serialize(struct confd_serializable *s,
                           unsigned char *buf, int bufsz,int *bytes_written,
                           unsigned char **allocated);

extern void confd_deserialized_free(struct confd_deserializable *s);

/* internal connect function but also used from NCS PyVM (packages) */
extern int confd_do_connect(int sock, const struct sockaddr *srv, int srv_sz,
                            int id);

#undef PRINTF

/* A set of macros to set/get the value of a confd_value_t struct */

#define CONFD_SET_STR(_v, _str)                                 \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type = C_STR; __vp->val.c_s = _str;} while(0)
#define CONFD_SET_BUF(_v, _bufp, _len)                    \
    do {confd_value_t *__vp = (_v);                       \
        __vp->type = C_BUF; __vp->val.c_buf.ptr = _bufp;  \
        __vp->val.c_buf.size = _len;} while(0)
/* a version that works with signed char* */
#define CONFD_SET_CBUF(_v, _bufp, _len)                                 \
    do {confd_value_t *__vp = (_v);                                     \
        __vp->type = C_BUF; __vp->val.c_buf.ptr = (unsigned const char*)_bufp; \
        __vp->val.buf.size = _len;} while(0)
#define CONFD_SET_BINARY(_v, _bufp, _len)                       \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type = C_BINARY; __vp->val.c_buf.ptr = _bufp;     \
        __vp->val.c_buf.size = _len;} while(0)
#define CONFD_SET_IPV4(_v, _ipval)                              \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type = C_IPV4; __vp->val.ip = _ipval;} while(0)
#define CONFD_SET_IPV6(_v, _ipval)                              \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type = C_IPV6; __vp->val.ip6 = _ipval;} while(0)
#define CONFD_SET_INT8(_v, _i)                                  \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type =  C_INT8;  __vp->val.i8 = _i;} while(0)
#define CONFD_SET_INT16(_v, _i)                                 \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type = C_INT16; __vp->val.i16 = _i;} while(0)
#define CONFD_SET_INT32(_v, _i)                                 \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type = C_INT32; __vp->val.i32 = _i;} while(0)
#define CONFD_SET_INT64(_v, _i)                                 \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type = C_INT64; __vp->val.i64 = _i;} while(0)
#define CONFD_SET_BOOL(_v, _b)                                  \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type = C_BOOL; __vp->val.boolean = _b;} while(0)
#define CONFD_SET_UINT8(_v, _ui)                                \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type = C_UINT8;  __vp->val.u8 = _ui;} while(0)
#define CONFD_SET_UINT16(_v, _ui)                               \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type = C_UINT16; __vp->val.u16 = _ui;} while(0)
#define CONFD_SET_UINT32(_v, _ui)                               \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type = C_UINT32; __vp->val.u32 = _ui;} while(0)
#define CONFD_SET_UINT64(_v, _ui)                               \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type = C_UINT64; __vp->val.u64 = _ui;} while(0)

#define CONFD_SET_DOUBLE(_v, _dval)                             \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type =  C_DOUBLE;  __vp->val.d = _dval;} while(0)

#define CONFD_SET_QNAME(_v, _pre, _presz, _n, _nsz)             \
    do {confd_value_t *__vp = (_v);                             \
        CONFD_ASSERT((_nsz) > 0 && (_n) != NULL);               \
        __vp->type = C_QNAME;                                   \
        __vp->val.qname.prefix.ptr = (_pre);                    \
        __vp->val.qname.prefix.size = (_presz);                 \
        __vp->val.qname.name.ptr = (_n);                        \
        __vp->val.qname.name.size = (_nsz);} while(0)

#define CONFD_SET_DATETIME(_v, _dval)                                   \
    do {confd_value_t *__vp = (_v);                                     \
        __vp->type =  C_DATETIME;  __vp->val.datetime = _dval;} while(0)
#define CONFD_SET_DATE(_v, _dval)                                       \
    do {confd_value_t *__vp = (_v);                                     \
        __vp->type =  C_DATE;  __vp->val.date = _dval;} while(0)
#define CONFD_SET_TIME(_v, _dval)                                       \
    do {confd_value_t *__vp = (_v);                                     \
        __vp->type =  C_TIME;  __vp->val.time = _dval;} while(0)
#define CONFD_SET_DURATION(_v, _dval)                                   \
    do {confd_value_t *__vp = (_v);                                     \
        __vp->type =  C_DURATION;  __vp->val.duration = _dval;} while(0)
#define CONFD_SET_ENUM_VALUE(_v, _dval)                                 \
    do {confd_value_t *__vp = (_v);                                     \
        __vp->type =  C_ENUM_VALUE;  __vp->val.enumvalue = _dval;} while(0)
/* for backwards compatibility */
#define CONFD_SET_ENUM_HASH(_v, _dval)                                  \
    do {confd_value_t *__vp = (_v);                                     \
        __vp->type =  C_ENUM_HASH;  __vp->val.enumhash = _dval;} while(0)
#define CONFD_SET_BIT32(_v, _ui)                                \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type = C_BIT32; __vp->val.b32 = _ui;} while(0)
#define CONFD_SET_BIT64(_v, _ui)                                \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type = C_BIT64; __vp->val.b64 = _ui;} while(0)
#define CONFD_SET_BITBIG(_v, _bufp, _len)                       \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type = C_BITBIG; __vp->val.c_buf.ptr = _bufp;     \
        __vp->val.c_buf.size = _len;} while(0)
#define CONFD_SET_LIST(_v, _ptr, _size)         \
    do {confd_value_t *__vp = (_v);             \
        __vp->type = C_LIST;                    \
        __vp->val.list.ptr = _ptr;              \
        __vp->val.list.size = _size; } while(0)
#define CONFD_SET_OBJECTREF(_v, _hkp)                           \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type = C_OBJECTREF; __vp->val.hkp = _hkp;} while(0)
#define CONFD_SET_OID(_v, _oidp)                                \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type = C_OID; __vp->val.oidp = _oidp;} while(0)
#define CONFD_SET_IPV4PREFIX(_v, _dval)                                 \
    do {confd_value_t *__vp = (_v);                                     \
        __vp->type =  C_IPV4PREFIX;  __vp->val.ipv4prefix = _dval;} while(0)
#define CONFD_SET_IPV6PREFIX(_v, _dval)                                 \
    do {confd_value_t *__vp = (_v);                                     \
        __vp->type =  C_IPV6PREFIX;  __vp->val.ipv6prefix = _dval;} while(0)
#define CONFD_SET_DECIMAL64(_v, _dval)                                  \
    do {confd_value_t *__vp = (_v);                                     \
        __vp->type = C_DECIMAL64; __vp->val.d64 = _dval; } while(0)
#define CONFD_SET_IDENTITYREF(_v, _dval)                                \
    do {confd_value_t *__vp = (_v);                                     \
        __vp->type = C_IDENTITYREF; __vp->val.idref = _dval; } while(0)
#define CONFD_SET_DQUAD(_v, _dval)                                 \
    do {confd_value_t *__vp = (_v);                                     \
        __vp->type =  C_DQUAD;  __vp->val.dquad = _dval;} while(0)
#define CONFD_SET_HEXSTR(_v, _bufp, _len)                       \
    do {confd_value_t *__vp = (_v);                             \
        __vp->type = C_HEXSTR; __vp->val.c_buf.ptr = _bufp;     \
        __vp->val.c_buf.size = _len;} while(0)
#define CONFD_SET_IPV4_AND_PLEN(_v, _dval)                                 \
    do {confd_value_t *__vp = (_v);                                     \
        __vp->type =  C_IPV4_AND_PLEN;  __vp->val.ipv4prefix = _dval;} while(0)
#define CONFD_SET_IPV6_AND_PLEN(_v, _dval)                                 \
    do {confd_value_t *__vp = (_v);                                     \
        __vp->type =  C_IPV6_AND_PLEN;  __vp->val.ipv6prefix = _dval;} while(0)
#define CONFD_SET_XMLTAG(_v, _tag, _ns)         \
    do {confd_value_t *__vp = (_v);             \
        __vp->type = C_XMLTAG;                  \
        __vp->val.xmltag.tag = _tag;            \
        __vp->val.xmltag.ns = _ns; } while(0)
#define CONFD_SET_XMLBEGIN(_v, _tag, _ns)       \
    do {confd_value_t *__vp = (_v);             \
        __vp->type = C_XMLBEGIN;                \
        __vp->val.xmltag.tag = _tag;            \
        __vp->val.xmltag.ns = _ns; } while(0)
#define CONFD_SET_XMLEND(_v, _tag, _ns)         \
    do {confd_value_t *__vp = (_v);             \
        __vp->type = C_XMLEND;                  \
        __vp->val.xmltag.tag = _tag;            \
        __vp->val.xmltag.ns = _ns; } while(0)
#define CONFD_SET_XMLBEGINDEL(_v, _tag, _ns)    \
    do {confd_value_t *__vp = (_v);             \
        __vp->type = C_XMLBEGINDEL;             \
        __vp->val.xmltag.tag = _tag;            \
        __vp->val.xmltag.ns = _ns; } while(0)
#define CONFD_SET_NOEXISTS(_v)                  \
    do {confd_value_t *__vp = (_v);             \
        __vp->type = C_NOEXISTS;} while(0)
#define CONFD_SET_CDBBEGIN(_v, _index)          \
    do {confd_value_t *__vp = (_v);             \
        __vp->type = C_CDBBEGIN;                \
        __vp->val.i32 = _index; } while(0)
#define CONFD_SET_DEFAULT(_v)                   \
    do {confd_value_t *__vp = (_v);             \
        __vp->type = C_DEFAULT;} while(0)

#define CONFD_SET_PTR(_v, _type, _valp)                                 \
    do {confd_value_t *__vp = (_v);                                     \
        __vp->type = C_PTR;                                             \
        __vp->val.ptr.type = _type; __vp->val.ptr.valp = _valp;} while(0)

/* Helper macros for setting/testing bits in the "little-endian" byte
   array of a C_BITBIG confd_value_t. Call with CONFD_GET_BITBIG_PTR(vp)
   for '_buf_ptr' when used for the actual confd_value_t.
   NOTE: The size of the byte array must be big enough! (i.e. > _pos / 8) */
#define CONFD_BITBIG_SET_BIT(_buf_ptr, _pos)                    \
    do {unsigned int __pos = (_pos);                            \
        (_buf_ptr)[__pos / 8] |= 1 << (__pos % 8);              \
    } while (0)
#define CONFD_BITBIG_CLR_BIT(_buf_ptr, _pos)                    \
    do {unsigned int __pos = (_pos);                            \
        (_buf_ptr)[__pos / 8] &= ~(1 << (__pos % 8));           \
    } while (0)
#define CONFD_BITBIG_BIT_IS_SET(_buf_ptr, _pos)                 \
    (((_buf_ptr)[(_pos) / 8] & (1 << ((_pos) % 8))) != 0)

#define CONFD_GET_BUFPTR(_v) \
    (CONFD_ASSERT((_v)->type == C_BUF), (_v)->val.buf.ptr)
#define CONFD_GET_CBUFPTR(_v) \
    (CONFD_ASSERT((_v)->type == C_BUF), ((char*)(_v)->val.buf.ptr))
#define CONFD_GET_BUFSIZE(_v) \
    (CONFD_ASSERT((_v)->type == C_BUF), (_v)->val.buf.size)
#define CONFD_GET_BINARY_PTR(_v) \
    (CONFD_ASSERT((_v)->type == C_BINARY), (_v)->val.buf.ptr)
#define CONFD_GET_BINARY_SIZE(_v) \
    (CONFD_ASSERT((_v)->type == C_BINARY), (_v)->val.buf.size)
#define CONFD_GET_LIST(_v) \
    (CONFD_ASSERT((_v)->type == C_LIST), (_v)->val.list.ptr)
#define CONFD_GET_LISTSIZE(_v) \
    (CONFD_ASSERT((_v)->type == C_LIST), (_v)->val.list.size)
#define CONFD_GET_XMLTAG(_v) \
    (CONFD_ASSERT((_v)->type == C_XMLTAG), (_v)->val.xmltag.tag)
#define CONFD_GET_XMLTAG_NS(_v) \
    (CONFD_ASSERT((_v)->type == C_XMLTAG), (_v)->val.xmltag.ns)
#define CONFD_GET_XMLBEGIN(_v) \
    (CONFD_ASSERT((_v)->type == C_XMLBEGIN), (_v)->val.xmltag.tag)
#define CONFD_GET_XMLBEGIN_NS(_v) \
    (CONFD_ASSERT((_v)->type == C_XMLBEGIN), (_v)->val.xmltag.ns)
#define CONFD_GET_XMLEND(_v) \
    (CONFD_ASSERT((_v)->type == C_XMLEND), (_v)->val.xmltag.tag)
#define CONFD_GET_XMLEND_NS(_v) \
    (CONFD_ASSERT((_v)->type == C_XMLEND), (_v)->val.xmltag.ns)
#define CONFD_GET_XML(_v) \
    (CONFD_ASSERT((_v)->type == C_XMLTAG || (_v)->type == C_XMLBEGIN || \
                  (_v)->type == C_XMLBEGINDEL || (_v)->type == C_XMLEND), \
     (_v)->val.xmltag.tag)
#define CONFD_GET_XML_NS(_v) \
    (CONFD_ASSERT((_v)->type == C_XMLTAG || (_v)->type == C_XMLBEGIN || \
                  (_v)->type == C_XMLBEGINDEL || (_v)->type == C_XMLEND), \
     (_v)->val.xmltag.ns)
#define CONFD_GET_CDBBEGIN(_v) \
    (CONFD_ASSERT((_v)->type == C_CDBBEGIN), (_v)->val.i32)
#define CONFD_GET_INT8(_v) \
    (CONFD_ASSERT((_v)->type == C_INT8), (_v)->val.i8)
#define CONFD_GET_INT16(_v) \
    (CONFD_ASSERT((_v)->type == C_INT16), (_v)->val.i16)
#define CONFD_GET_INT32(_v) \
    (CONFD_ASSERT((_v)->type == C_INT32), (_v)->val.i32)
#define CONFD_GET_INT64(_v) \
    (CONFD_ASSERT((_v)->type == C_INT64), (_v)->val.i64)
#define CONFD_GET_UINT8(_v) \
    (CONFD_ASSERT((_v)->type == C_UINT8), (_v)->val.u8)
#define CONFD_GET_UINT16(_v) \
    (CONFD_ASSERT((_v)->type == C_UINT16), (_v)->val.u16)
#define CONFD_GET_UINT32(_v) \
    (CONFD_ASSERT((_v)->type == C_UINT32), (_v)->val.u32)
#define CONFD_GET_UINT64(_v) \
    (CONFD_ASSERT((_v)->type == C_UINT64), (_v)->val.u64)
#define CONFD_GET_DOUBLE(_v) \
    (CONFD_ASSERT((_v)->type == C_DOUBLE), (_v)->val.d)
#define CONFD_GET_IPV4(_v) \
    (CONFD_ASSERT((_v)->type == C_IPV4), (_v)->val.ip)
#define CONFD_GET_IPV6(_v) \
    (CONFD_ASSERT((_v)->type == C_IPV6), (_v)->val.ip6)
#define CONFD_GET_BOOL(_v) \
    (CONFD_ASSERT((_v)->type == C_BOOL), (_v)->val.boolean)
#define CONFD_GET_QNAME_PREFIX_PTR(_v)  \
    (CONFD_ASSERT((_v)->type == C_QNAME), (_v)->val.qname.prefix.ptr)
#define CONFD_GET_QNAME_NAME_PTR(_v)  \
    (CONFD_ASSERT((_v)->type == C_QNAME), (_v)->val.qname.name.ptr)
#define CONFD_GET_QNAME_PREFIX_SIZE(_v)  \
    (CONFD_ASSERT((_v)->type == C_QNAME), (_v)->val.qname.prefix.size)
#define CONFD_GET_QNAME_NAME_SIZE(_v)  \
    (CONFD_ASSERT((_v)->type == C_QNAME), (_v)->val.qname.name.size)
#define CONFD_GET_DATETIME(_v) \
    (CONFD_ASSERT((_v)->type == C_DATETIME), (_v)->val.datetime)
#define CONFD_GET_DATE(_v) \
    (CONFD_ASSERT((_v)->type == C_DATE), (_v)->val.date)
#define CONFD_GET_TIME(_v) \
    (CONFD_ASSERT((_v)->type == C_TIME), (_v)->val.time)
#define CONFD_GET_DURATION(_v) \
    (CONFD_ASSERT((_v)->type == C_DURATION), (_v)->val.duration)
#define CONFD_GET_ENUM_VALUE(_v) \
    (CONFD_ASSERT((_v)->type == C_ENUM_VALUE), (int32_t)((_v)->val.enumvalue))
/* for backwards compatibility */
#define CONFD_GET_ENUM_HASH(_v) \
    (CONFD_ASSERT((_v)->type == C_ENUM_HASH), (_v)->val.enumhash)
#define CONFD_GET_BIT32(_v) \
    (CONFD_ASSERT((_v)->type == C_BIT32), (_v)->val.b32)
#define CONFD_GET_BIT64(_v) \
    (CONFD_ASSERT((_v)->type == C_BIT64), (_v)->val.b64)
#define CONFD_GET_BITBIG_PTR(_v) \
    (CONFD_ASSERT((_v)->type == C_BITBIG), (_v)->val.buf.ptr)
#define CONFD_GET_BITBIG_SIZE(_v) \
    (CONFD_ASSERT((_v)->type == C_BITBIG), (_v)->val.buf.size)
#define CONFD_GET_OBJECTREF(_v) \
    (CONFD_ASSERT((_v)->type == C_OBJECTREF), (_v)->val.hkp)
/* deprecated for type == OBJECTREF */
#define CONFD_GET_HKPPTR(_v) (CONFD_GET_OBJECTREF(_v))
#define CONFD_GET_OID(_v) \
    (CONFD_ASSERT((_v)->type == C_OID), (_v)->val.oidp)
#define CONFD_GET_IPV4PREFIX(_v) \
    (CONFD_ASSERT((_v)->type == C_IPV4PREFIX), (_v)->val.ipv4prefix)
#define CONFD_GET_IPV6PREFIX(_v) \
    (CONFD_ASSERT((_v)->type == C_IPV6PREFIX), (_v)->val.ipv6prefix)
#define CONFD_GET_DECIMAL64(_v) \
    (CONFD_ASSERT((_v)->type == C_DECIMAL64), (_v)->val.d64)
#define CONFD_GET_IDENTITYREF(_v) \
    (CONFD_ASSERT((_v)->type == C_IDENTITYREF), (_v)->val.idref)
#define CONFD_GET_IPV4_AND_PLEN(_v) \
    (CONFD_ASSERT((_v)->type == C_IPV4_AND_PLEN), (_v)->val.ipv4prefix)
#define CONFD_GET_IPV6_AND_PLEN(_v) \
    (CONFD_ASSERT((_v)->type == C_IPV6_AND_PLEN), (_v)->val.ipv6prefix)
#define CONFD_GET_DQUAD(_v) \
    (CONFD_ASSERT((_v)->type == C_DQUAD), (_v)->val.dquad)
#define CONFD_GET_HEXSTR_PTR(_v) \
    (CONFD_ASSERT((_v)->type == C_HEXSTR), (_v)->val.buf.ptr)
#define CONFD_GET_HEXSTR_SIZE(_v) \
    (CONFD_ASSERT((_v)->type == C_HEXSTR), (_v)->val.buf.size)
#define CONFD_GET_PTR_TYPE(_v) \
    (CONFD_ASSERT((_v)->type == C_PTR), (_v)->val.ptr.type)
#define CONFD_GET_PTR_VALP(_v) \
    (CONFD_ASSERT((_v)->type == C_PTR), (_v)->val.ptr.valp)


/* A set of macros to set/get the value of a confd_tag_value_t struct */

#define CONFD_SET_TAG_STR(_v, _t_tag, _str)             \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_STR(&__tvp->v, (_str));} while(0)
#define CONFD_SET_TAG_BUF(_v, _t_tag, _bufp, _len)              \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);           \
        CONFD_SET_BUF(&__tvp->v, (_bufp), (_len));} while(0)
/* a version that works with signed char* */
#define CONFD_SET_TAG_CBUF(_v, _t_tag, _bufp, _len)             \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);           \
        CONFD_SET_CBUF(&__tvp->v, (_bufp), (_len));} while(0)
#define CONFD_SET_TAG_BINARY(_v, _t_tag, _bufp, _len)           \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);           \
        CONFD_SET_BINARY(&__tvp->v, (_bufp), (_len));} while(0)
#define CONFD_SET_TAG_IPV4(_v, _t_tag, _ipval)          \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_IPV4(&__tvp->v, (_ipval));} while(0)
#define CONFD_SET_TAG_IPV6(_v, _t_tag, _ipval)          \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_IPV6(&__tvp->v, (_ipval));} while(0)

#define CONFD_SET_TAG_INT8(_v, _t_tag, _i)              \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_INT8(&__tvp->v, (_i));} while(0)
#define CONFD_SET_TAG_INT16(_v, _t_tag, _i)             \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_INT16(&__tvp->v, (_i));} while(0)
#define CONFD_SET_TAG_INT32(_v, _t_tag, _i)             \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_INT32(&__tvp->v, (_i));} while(0)
#define CONFD_SET_TAG_INT64(_v, _t_tag, _i)             \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_INT64(&__tvp->v, (_i));} while(0)
#define CONFD_SET_TAG_BOOL(_v, _t_tag, _b)              \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_BOOL(&__tvp->v, (_b));} while(0)

#define CONFD_SET_TAG_UINT8(_v, _t_tag, _ui)            \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_UINT8(&__tvp->v, (_ui));} while(0)
#define CONFD_SET_TAG_UINT16(_v, _t_tag, _ui)           \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_UINT16(&__tvp->v, (_ui));} while(0)
#define CONFD_SET_TAG_UINT32(_v, _t_tag, _ui)           \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_UINT32(&__tvp->v, (_ui));} while(0)
#define CONFD_SET_TAG_UINT64(_v, _t_tag, _ui)           \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_UINT64(&__tvp->v, (_ui));} while(0)

#define CONFD_SET_TAG_DOUBLE(_v, _t_tag, _dval)         \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_DOUBLE(&__tvp->v, (_dval));} while(0)

#define CONFD_SET_TAG_QNAME(_v, _t_tag, _pre, _presz, _n, _nsz)         \
    do {confd_tag_value_t *__tvp = (_v);                                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);                   \
        CONFD_SET_QNAME(&__tvp->v, (_pre), (_presz), (_n), (_nsz));} while(0)

#define CONFD_SET_TAG_DATETIME(_v, _t_tag, _dval)               \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);           \
        CONFD_SET_DATETIME(&__tvp->v, (_dval));} while(0)
#define CONFD_SET_TAG_DATE(_v, _t_tag, _dval)           \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_DATE(&__tvp->v, (_dval));} while(0)
#define CONFD_SET_TAG_TIME(_v, _t_tag, _dval)           \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_TIME(&__tvp->v, (_dval));} while(0)
#define CONFD_SET_TAG_DURATION(_v, _t_tag, _dval)               \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);           \
        CONFD_SET_DURATION(&__tvp->v, (_dval));} while(0)
#define CONFD_SET_TAG_ENUM_VALUE(_v, _t_tag, _dval)             \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);           \
        CONFD_SET_ENUM_VALUE(&__tvp->v, (_dval));} while(0)
/* for backwards compatibility */
#define CONFD_SET_TAG_ENUM_HASH(_v, _t_tag, _dval)              \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);           \
        CONFD_SET_ENUM_HASH(&__tvp->v, (_dval));} while(0)
#define CONFD_SET_TAG_BIT32(_v, _t_tag, _ui)            \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_BIT32(&__tvp->v, (_ui));} while(0)
#define CONFD_SET_TAG_BIT64(_v, _t_tag, _ui)            \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_BIT64(&__tvp->v, (_ui));} while(0)
#define CONFD_SET_TAG_BITBIG(_v, _t_tag, _bufp, _len)           \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);           \
        CONFD_SET_BITBIG(&__tvp->v, (_bufp), (_len));} while(0)
#define CONFD_SET_TAG_LIST(_v, _t_tag, _ptr, _size)             \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);           \
        CONFD_SET_LIST(&__tvp->v, (_ptr), (_size));} while(0)
#define CONFD_SET_TAG_OBJECTREF(_v, _t_tag, _hkp)               \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_OBJECTREF(&__tvp->v, (_hkp));} while(0)
#define CONFD_SET_TAG_OID(_v, _t_tag, _oidp)            \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_OID(&__tvp->v, (_oidp));} while(0)
#define CONFD_SET_TAG_IPV4PREFIX(_v, _t_tag, _dval)     \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_IPV4PREFIX(&__tvp->v, (_dval));} while(0)
#define CONFD_SET_TAG_IPV6PREFIX(_v, _t_tag, _dval)     \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_IPV6PREFIX(&__tvp->v, (_dval));} while(0)
#define CONFD_SET_TAG_DECIMAL64(_v, _t_tag, _dval)      \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_DECIMAL64(&__tvp->v, (_dval));} while(0)
#define CONFD_SET_TAG_IDENTITYREF(_v, _t_tag, _dval)            \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);           \
        CONFD_SET_IDENTITYREF(&__tvp->v, (_dval));} while(0)
#define CONFD_SET_TAG_IPV4_AND_PLEN(_v, _t_tag, _dval)     \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_IPV4_AND_PLEN(&__tvp->v, (_dval));} while(0)
#define CONFD_SET_TAG_IPV6_AND_PLEN(_v, _t_tag, _dval)     \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_IPV6_AND_PLEN(&__tvp->v, (_dval));} while(0)
#define CONFD_SET_TAG_DQUAD(_v, _t_tag, _dval)     \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        CONFD_SET_DQUAD(&__tvp->v, (_dval));} while(0)
#define CONFD_SET_TAG_HEXSTR(_v, _t_tag, _bufp, _len)           \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);           \
        CONFD_SET_HEXSTR(&__tvp->v, (_bufp), (_len));} while(0)
#define CONFD_SET_TAG_XMLTAG(_v, _t_tag, _ns)                   \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = _ns; __tvp->tag.tag = (_t_tag);         \
        CONFD_SET_XMLTAG(&__tvp->v, (_t_tag), (_ns));} while(0)
#define CONFD_SET_TAG_XMLBEGIN(_v, _t_tag, _ns)                 \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = _ns; __tvp->tag.tag = (_t_tag);         \
        CONFD_SET_XMLBEGIN(&__tvp->v, (_t_tag), (_ns));} while(0)
#define CONFD_SET_TAG_XMLEND(_v, _t_tag, _ns)                   \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = _ns; __tvp->tag.tag = (_t_tag);         \
        CONFD_SET_XMLEND(&__tvp->v, (_t_tag), (_ns));} while(0)
#define CONFD_SET_TAG_XMLBEGINDEL(_v, _t_tag, _ns)              \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = _ns; __tvp->tag.tag = (_t_tag);         \
        CONFD_SET_XMLBEGINDEL(&__tvp->v, (_t_tag), (_ns));} while(0)
#define CONFD_SET_TAG_NOEXISTS(_v, _t_tag)                      \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);           \
        CONFD_SET_NOEXISTS(&__tvp->v);} while(0)
#define CONFD_SET_TAG_CDBBEGIN(_v, _t_tag, _ns, _index)         \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = _ns; __tvp->tag.tag = (_t_tag);         \
        CONFD_SET_CDBBEGIN(&__tvp->v, (_index));} while(0)
#define CONFD_SET_TAG_PTR(_v, _t_tag, _type, _valp)             \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);           \
        CONFD_SET_PTR(&__tvp->v, (_type), (_valp));} while(0)
#define CONFD_SET_TAG_DEFAULT(_v, _t_tag)                       \
    do {confd_tag_value_t *__tvp = (_v);                        \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);           \
        CONFD_SET_DEFAULT(&__tvp->v);} while(0)

#define CONFD_SET_TAG_VALUE(_v, _t_tag, _vv)            \
    do {confd_tag_value_t *__tvp = (_v);                \
        __tvp->tag.ns = 0; __tvp->tag.tag = (_t_tag);   \
        __tvp->v = *_vv;} while(0)

#define CONFD_SET_TAG_NS(_v, _ns)               \
    do {confd_tag_value_t *__tvp = (_v);        \
        __tvp->tag.ns = (_ns);} while(0)

#define CONFD_GET_TAG_NS(_v) ((_v)->tag.ns)
#define CONFD_GET_TAG_TAG(_v) ((_v)->tag.tag)
#define CONFD_GET_TAG_VALUE(_v) (&(_v)->v)

/* a ha node has an identifier and an address */
struct confd_ha_node {
    confd_value_t nodeid;
    int af;               /* AF_INET | AF_INET6 | AF_UNSPEC */
    union {               /* address of remote note */
        struct in_addr ip4;
        struct in6_addr ip6;
        char *str;
    } addr;
    char buf[128];        /* when confd_read_notification() and            */
                          /* confd_ha_get_status() populate these structs, */
                          /* if type of nodeid is C_BUF, the pointer       */
                          /* will be set to point into this buffer         */
    char addr_buf[128];   /* similar to the above, but for the address     */
                          /* of remote node when using external IPC        */
                          /* (from getpeeraddr() callback for slaves)      */
};


/* contains the actual symblic names of all the logsyms*/
/* indexed by logno                                  */
extern char *confd_log_symbols[];

/* contains a textual description of each log item */
/* indexed by logno                                  */
extern char *confd_log_descriptions[];

/* set confd_lib_use_syslog to 1 if we want all confd_lib output to also */
/* be syslogged */
/* it's the responsibility of the user to call openlog() first */
extern int confd_lib_use_syslog;

/* user supplied loghook, whenever confd_lib writes anything to the */
/* usersupplied (possibly NULL) FILE* given to confd_init()         */
/* this function also gets called (if non-NULL)                     */
extern void (*confd_user_log_hook)(int syslogprio, const char *fmt, va_list ap);

/* various values of confd_errno */
#define CONFD_ERR_NOEXISTS                1
#define CONFD_ERR_ALREADY_EXISTS          2
#define CONFD_ERR_ACCESS_DENIED           3
#define CONFD_ERR_NOT_WRITABLE            4
#define CONFD_ERR_BADTYPE                 5
#define CONFD_ERR_NOTCREATABLE            6
#define CONFD_ERR_NOTDELETABLE            7
#define CONFD_ERR_BADPATH                 8
#define CONFD_ERR_NOSTACK                 9
#define CONFD_ERR_LOCKED                 10
#define CONFD_ERR_INUSE                  11

/* error codes from apply and validate */
#define CONFD_ERR_NOTSET                  12
#define CONFD_ERR_NON_UNIQUE              13
#define CONFD_ERR_BAD_KEYREF              14
#define CONFD_ERR_TOO_FEW_ELEMS           15
#define CONFD_ERR_TOO_MANY_ELEMS          16
#define CONFD_ERR_BADSTATE                17

#define CONFD_ERR_INTERNAL                18
#define CONFD_ERR_EXTERNAL                19
#define CONFD_ERR_MALLOC                  20
#define CONFD_ERR_PROTOUSAGE              21
#define CONFD_ERR_NOSESSION               22
#define CONFD_ERR_TOOMANYTRANS            23
#define CONFD_ERR_OS                      24

/* ha related errors */
#define CONFD_ERR_HA_CONNECT          25
#define CONFD_ERR_HA_CLOSED           26
#define CONFD_ERR_HA_BADFXS           27
#define CONFD_ERR_HA_BADTOKEN         28
#define CONFD_ERR_HA_BADNAME          29
#define CONFD_ERR_HA_BIND             30
#define CONFD_ERR_HA_NOTICK           31

/* maapi_validate()  */
#define CONFD_ERR_VALIDATION_WARNING  32

#define CONFD_ERR_SUBAGENT_DOWN       33
#define CONFD_ERR_LIB_NOT_INITIALIZED 34
#define CONFD_ERR_TOO_MANY_SESSIONS   35
#define CONFD_ERR_BAD_CONFIG          36

/* maapi for extended errors */
#define CONFD_ERR_RESOURCE_DENIED      37
#define CONFD_ERR_INCONSISTENT_VALUE   38
#define CONFD_ERR_APPLICATION_INTERNAL 39

/* more error codes from apply and validate */
#define CONFD_ERR_UNSET_CHOICE         40
#define CONFD_ERR_MUST_FAILED          41
#define CONFD_ERR_MISSING_INSTANCE     42
#define CONFD_ERR_INVALID_INSTANCE     43

/* (e.g.) maapi_get_attrs()/maapi_set_attr() */
#define CONFD_ERR_UNAVAILABLE          44

/* used when API function returns CONFD_EOF */
#define CONFD_ERR_EOF                  45

/* maapi_move* */
#define CONFD_ERR_NOTMOVABLE           46

/* in-service upgrade */
#define CONFD_ERR_HA_WITH_UPGRADE      47
#define CONFD_ERR_TIMEOUT              48
#define CONFD_ERR_ABORTED              49

/* xpath compilation/evaluation */
#define CONFD_ERR_XPATH                50

/* not implemented CDB/MAAPI op */
#define CONFD_ERR_NOT_IMPLEMENTED      51

/* HA incompatible version */
#define CONFD_ERR_HA_BADVSN            52

/* error codes for configuration policies from apply and validate */
#define CONFD_ERR_POLICY_FAILED             53
#define CONFD_ERR_POLICY_COMPILATION_FAILED 54
#define CONFD_ERR_POLICY_EVALUATION_FAILED  55

/* NCS failed to connect to a device */
#define NCS_ERR_CONNECTION_REFUSED          56

/* error code when maapi_start_phase() fails */
#define CONFD_ERR_START_FAILED              57

/* more maapi for extended error */
#define CONFD_ERR_DATA_MISSING              58

/* error code for cli command */
#define CONFD_ERR_CLI_CMD                   59

/* error code for disallowed operations in upgrade phase */
#define CONFD_ERR_UPGRADE_IN_PROGRESS       60

/* invalid transaction handle passed to a maapi function */
#define CONFD_ERR_NOTRANS                   61

/* NCS specific errors */
#define NCS_ERR_SERVICE_CONFLICT            62
#define NCS_ERR_CONNECTION_TIMEOUT          63
#define NCS_ERR_CONNECTION_CLOSED           64
#define NCS_ERR_DEVICE                      65
#define NCS_ERR_TEMPLATE                    66

#ifdef __cplusplus
}
#endif
#endif
