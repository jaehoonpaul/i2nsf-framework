/*
 * Copyright 2014 Tail-F Systems AB Tail-F customers are permitted to
 * redistribute in binary form, with or without modification, for use
 * in customer products.
 */

#ifndef _LINUXCFG_API_H
#define _LINUXCFG_API_H 1

#include <confd.h>

#define _CONCAT(a, b, c)    a ## b ## c
#define _NSCONCAT(ns, val)  _CONCAT(ns, _, val)
#define _NSCONCAT2(ns, val) _CONCAT(ns, __, val)
#define NSPREF(val)         _NSCONCAT(NS_PREFIX, val)
#define NSPREF2(val)        _NSCONCAT2(NS_PREFIX, val)
#define NAMESPACE           NSPREF(_ns)

#define NORMAL_PRIO     10

// TODO: Decide where this goes.
#define TMPDIR          "/tmp"

extern int linuxcfg_loglevel;
#define LOG(format, ...) do {                                        \
        if (linuxcfg_loglevel != 0)                                  \
        info("%s:%d: %s() - " format,                                \
             __FILE__, __LINE__, __FUNCTION__, ##__VA_ARGS__);       \
    } while (0)

#define TRACE_ENTRY(format, ...)   do {                              \
     if (linuxcfg_loglevel != 0)                                     \
        info("%s:%d: %s %s() - " format,                             \
             __FILE__, __LINE__, "==>", __FUNCTION__, ##__VA_ARGS__);\
     } while (0)

#define TRACE_EXIT(format, ...)    do {                              \
      if (linuxcfg_loglevel != 0)                                    \
         info("%s:%d: %s %s() - " format, __FILE__,                  \
              __LINE__, "<==", __FUNCTION__, ##__VA_ARGS__);         \
      } while (0)

#define GET_EXT_CMD(ext_cmd)    (((ext_cmd).cmd != NULL) ?           \
                                 (ext_cmd).cmd :                     \
                                 get_ext_cmd(&(ext_cmd)))


/* Only setup is required, all the others can be set to NULL */
struct component {
    void (*init)(void);
    void (*setup0)(struct confd_daemon_ctx *dctx,
                   struct confd_trans_ctx *tctx, int readsock);
    void (*setup)(struct confd_daemon_ctx *dctx, int readsock);
    struct confd_valpoint_cb *valpoints;
    struct confd_data_cbs    *datapoints;
    struct confd_action_cbs  *actionpoints;
    int (*init_validation)(struct confd_trans_ctx *tctx);
    void (*stop_validation)(void);
    int (*init_data)(struct confd_trans_ctx *tctx);
    void (*finish_data)(struct confd_trans_ctx *tctx);
};

struct external_cmd {
    // Descriptive name for the log
    char *descr_name;
    // The command string linuxcfg will use.
    // Initialize it to NULL, will be set by get_ext_cmd() on the
    // first call.
    char *cmd;
    // Command parameters. Will be added to 'cmd'.
    char *args;
    // Possible commands. First match (== command exists and is
    //  executable) will be used as 'cmd':
    char *env_var;      // specified by an environment variable
    char *cc_macro;     // specified by a compiler macro
    char *binary_name;  // name of the binary, $PATH will be searched
    char *paths[];      // array of possible "/path/cmd" strings
};

extern void register_fd_handler(int fd, int events,
                                void (*handler)(int fd, int revents));

extern const char *get_system_root();

extern int subscribe(int prio, void (*cb)(int readsock,
                                           int subsock, int subpoint),
                      char *path);
extern int validate_fail(struct confd_trans_ctx *tctx,
                         char *fmt, ...);
extern int validate_warn(struct confd_trans_ctx *tctx,
                         char *fmt, ...);
extern int get_data_not_found(struct confd_trans_ctx *tctx);
extern int get_data_unknown_path(struct confd_trans_ctx *tctx,
                                 confd_hkeypath_t *kp);

extern int run(char **outbuf, int outlen, char *fmt, ...);
extern int cmp(char *file1, char *file2);
extern int read_file(char *file, char **bufp);
extern void file_kill(char *server, char *pidfile, int signal);
extern void killall(char *server, int signal);

extern int is_executable_file(char *path);
extern char *get_ext_cmd(struct external_cmd *ext_cmd);

extern char *kpath2str(const confd_hkeypath_t *hkeypath);

extern void mk4subnet(struct in_addr *addr, int prefixlen,
                      struct in_addr *net);
extern void mk6subnet(struct in6_addr *addr, int prefixlen,
                      struct in6_addr *net);

extern void info(const char *fmt, ...);
extern void warn(const char *fmt, ...);
extern void error(const char *fmt, ...);
extern void fail(const char *fmt, ...);
extern void *xmalloc(size_t size);

void trigger_subscriptions(int *subpoints);

/* Set to 1 if the current change is the result of a trigger. */
extern int linuxcfg_change_is_trigger;

// for use in CDB subscribers requiring access to "old" data state;
// returns the socket that can be used to access pre-commit CDB state;
extern int start_read_old_session();

// wrapper for all the data required to pass around for transaction context
struct opaque_data {
    // MAAPI socket pointer used by some of the components.
    // is a pointer, not an integer for legacy reasons...
    int *msock;
    // NOTE: this is dirty code - simple data placeholder for one known LINUXCFG
    // component that uses something extra, not only the MAAPI socket
    // (like the IPMIBS or SYSTEM components do)
    void *routing_data;
};

// extract the MAAPI socket from the opaque data
int get_msock_from_opaq(void *tctx_opaque_data);

#endif
