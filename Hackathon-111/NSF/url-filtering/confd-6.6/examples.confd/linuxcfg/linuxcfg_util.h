/*
 * Copyright 2014 Tail-F Systems AB Tail-F customers are permitted to
 * redistribute in binary form, with or without modification, for use
 * in customer products.
 */

#ifndef _LINUXCFG_UTIL_H
#define _LINUXCFG_UTIL_H 1

/* Helper that simplifies a pattern where idx indexes into a keypath
 * stored as kp. */
#define GET_NEXT_TAG   (idx = idx - 1,                          \
                        CONFD_GET_XMLTAG(&kp->v[idx][0]));

#define NOT_FOUND  do { confd_data_reply_not_found(tctx);        \
                        return CONFD_OK;} while (0)


#define CHECK(val, msg) do {                                        \
        if ((val) != CONFD_OK) {                                    \
            error("%s: %s (%d)", (msg), confd_lasterr(),            \
                  confd_errno);                                     \
            return CONFD_ERR;                                       \
        }                                                           \
    } while (0)

/* A void version of CHECK */
#define CHECK2(val, msg) do {                                       \
        if ((val) != CONFD_OK) {                                    \
            error("%s: %s (%d)", (msg), confd_lasterr(),            \
                  confd_errno);                                     \
            return;                                                 \
        }                                                           \
    } while (0)

#define CHECK3(val, msg, rv) do {                                   \
        if ((val) != CONFD_OK) {                                    \
            error("%s: %s (%d)", (msg), confd_lasterr(),            \
                  confd_errno);                                     \
            return rv;                                              \
        }                                                           \
    } while (0)

/* A safer strncpy() */
#define XSTRNCPY(_dest, _src, _n)   do { \
    strncpy((_dest), (_src), (_n)); \
    (_dest)[(_n) - 1] = '\0';   \
} while (0)

/* A safer free() */
#define XFREE(_p)   do { free(_p); (_p) = NULL; } while (0)


char __kpbuf[512];
#define KPSTR(kp) (confd_pp_kpath(__kpbuf, 512, kp), __kpbuf)

char __valbuf[1024];
#define VALSTR(val) ((val == NULL) ? "" : \
                     (confd_pp_value(__valbuf, 1024, val), __valbuf))
#define VALSTRNN(val) (confd_pp_value(__valbuf, 1024, val), __valbuf)
#define VALSTR2(val) ((confd_pp_value(__valbuf, 1024, &val), \
                      __valbuf))

char __cwdbuf[512];
#define CWD(sock) (cdb_getcwd(sock, 512, __cwdbuf), __cwdbuf)

/* Converts a time_t into the confd representation of time.  */
int time_to_dt(time_t t, struct confd_datetime *dt);

/* Formats a buffer of bytes in the octet string format XX:XX:XX.. */
char *physaddr2str(unsigned char *buf, int len);

/* Reads a single int32 from a file, returns -1 on failure. */
int get_int32_file_cfg(const char *file, int32_t * value);

#endif
