
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <ctype.h>

#include <confd_lib.h>
#include "types.h"

/* parsing help functions */

static void skip_space(const char **str, const char *end)
{
    const char *p = *str;

    while (p < end && isspace(*p))
        p++;
    *str = p;
}

static int get_uint(const char **str, const char *end)
{
    const char *p = *str;
    const char *start;
    int i = 0;

    skip_space(&p, end);
    start = p;
    while (p < end && *p >= '0' && *p <= '9')
        i = i * 10 + (*p++ - '0');
    if (p == start)
        return -1;
    *str = p;
    return i;
}

/* custom error message */

static int ret_false_str(struct confd_type_ctx *ctx, char *fmt, ...)
{
    va_list ap;
    char buf[BUFSIZ];

    va_start(ap, fmt);
    vsnprintf(buf, sizeof(buf), fmt, ap);
    va_end(ap);
    ctx->errstr = strdup(buf);
    return CONFD_FALSE;
}


/* A port consists of three integer elements
   "<shelf>/<slot>/<port>" - stored in a C_INT32 */

#define MAX_SHELF 3
#define MAX_SLOT 15
#define MAX_PORT 63

static int max[3] = {MAX_SHELF, MAX_SLOT, MAX_PORT};
static char *name[3] = {"shelf", "slot", "port"};

#define RET_INVALID_PORT(ctx, elem, val, max)                           \
    ret_false_str((ctx), "invalid %s %d (min 0, max %d)", (elem), (val), (max))

static int port_str_to_val(struct confd_type *self,
                          struct confd_type_ctx *ctx,
                          const char *str, unsigned int len,
                          confd_value_t *v)
{
    const char *p = str;
    const char *end = str + len;
    int elem[3];
    int i;

    /* extract the three '/'-separated integers -
       leading and internal whitespace is allowed */
    for (i = 0; i < 3; i++) {
        skip_space(&p, end);
        elem[i] = get_uint(&p, end);
        if (elem[i] < 0 || elem[i] > max[i])
            return RET_INVALID_PORT(ctx, name[i], elem[i], max[i]);
        if (i < 2) {
            skip_space(&p, end);
            if (p >= end || *p != '/')
                return CONFD_FALSE;
            p++;
        }
    }
    /* check that only whitespace remains */
    skip_space(&p, end);
    if (p < end)
        return CONFD_FALSE;

    i = (elem[0] << 16) | (elem[1] << 8) | elem[2];
    CONFD_SET_INT32(v, i);
    return CONFD_TRUE;
}

static int port_val_to_str(struct confd_type *self,
                           struct confd_type_ctx *ctx,
                           const confd_value_t *v,
                           char *str, unsigned int len,
                           const char **strp)
{
    int i, shelf, slot, port;

    if (v->type != C_INT32)
        return -1;
    i = CONFD_GET_INT32(v);
    shelf = (i >> 16) & 0xff;
    slot  = (i >> 8) & 0xff;
    port  = i & 0xff;
    return snprintf(str, len, "%d/%d/%d", shelf, slot, port);
}

static int port_validate(struct confd_type *self,
                         struct confd_type_ctx *ctx,
                         const confd_value_t *v)
{
    int i, shelf, slot, port;

    if (v->type != C_INT32)
        return CONFD_FALSE;
    i = CONFD_GET_INT32(v);
    shelf = (i >> 16) & 0xff;
    slot  = (i >> 8) & 0xff;
    port  = i & 0xff;
    if (shelf > MAX_SHELF)
        return RET_INVALID_PORT(ctx, "shelf", shelf, MAX_SHELF);
    if (slot > MAX_SLOT)
        return RET_INVALID_PORT(ctx, "slot", slot, MAX_SLOT);
    if (port > MAX_PORT)
        return RET_INVALID_PORT(ctx, "port", port, MAX_PORT);
    return CONFD_TRUE;
}


/* An ipv4AddressMask consists of 2 values
   "<IPv4-address>/<prefix-length>" - stored in a C_IPV4PREFIX */

static int ipv4_mask_str_to_val(struct confd_type *self,
                                struct confd_type_ctx *ctx,
                                const char *str, unsigned int len,
                                confd_value_t *v)
{
    const char *p = str;
    const char *ep = str + len;
    const char *s;
    unsigned int vlen;
    char src[INET_ADDRSTRLEN];
    struct confd_ipv4_prefix addr_mask;
    int plen;

    skip_space(&p, ep);
    if (p >= ep)
        return CONFD_FALSE;
    vlen = ep - p;
    s = memchr(p, '/', vlen);
    if (s == NULL) {
        return CONFD_FALSE;
    }

    /* scan the ip */
    vlen = s - p;
    if (vlen > sizeof(src)-1) {
        return CONFD_FALSE;
    }
    /* make a NUL-terminated string for inet_pton */
    strncpy(src, p, vlen);
    src[vlen] = '\0';
    if (inet_pton(AF_INET, src, &addr_mask.ip) <= 0) {
        return CONFD_FALSE;
    }

    /* skip the '/' */
    s++;
    if ((plen = get_uint(&s, ep)) < 0 || plen > 32)
        return CONFD_FALSE;
    /* check that only whitespace remains */
    skip_space(&s, ep);
    if (s < ep)
        return CONFD_FALSE;
    addr_mask.len = plen;

    CONFD_SET_IPV4PREFIX(v, addr_mask);

    return CONFD_TRUE;
}

static int ipv4_mask_val_to_str(struct confd_type *self,
                                struct confd_type_ctx *ctx,
                                const confd_value_t *v,
                                char *str, unsigned int len,
                                const char **strp)
{
    struct confd_ipv4_prefix addr_mask;
    char dst[INET_ADDRSTRLEN];

    if (v->type != C_IPV4PREFIX)
        return -1;
    addr_mask = CONFD_GET_IPV4PREFIX(v);
    if (inet_ntop(AF_INET, &addr_mask.ip, dst, sizeof(dst)) == NULL)
        return -1;
    return snprintf(str, len, "%s/%d", dst, addr_mask.len);
}

static int ipv4_mask_validate(struct confd_type *self,
                              struct confd_type_ctx *ctx,
                              const confd_value_t *v)
{
    struct confd_ipv4_prefix addr_mask;

    if (v->type != C_IPV4PREFIX)
        return CONFD_FALSE;
    addr_mask = CONFD_GET_IPV4PREFIX(v);
    if (addr_mask.len > 32)
        return CONFD_FALSE;
    return CONFD_TRUE;
}

/* An ipv6AddressMask consists of 2 values
   "<IPv6-address>/<prefix-length>" - stored in a C_IPV6PREFIX */

static int ipv6_mask_str_to_val(struct confd_type *self,
                                struct confd_type_ctx *ctx,
                                const char *str, unsigned int len,
                                confd_value_t *v)
{
    const char *p = str;
    const char *ep = str + len;
    const char *s;
    unsigned int vlen;
    char src[INET_ADDRSTRLEN];
    struct confd_ipv6_prefix addr_mask;
    int plen;

    skip_space(&p, ep);
    if (p >= ep)
        return CONFD_FALSE;
    vlen = ep - p;
    s = memchr(p, '/', vlen);
    if (s == NULL) {
        return CONFD_FALSE;
    }

    /* scan the ip */
    vlen = s - p;
    if (vlen > sizeof(src)-1 ) {
        return CONFD_FALSE;
    }
    /* make a NUL-terminated string for inet_pton */
    strncpy(src, p, vlen);
    src[vlen] = '\0';
    if (inet_pton(AF_INET6, src, &addr_mask.ip6) <= 0) {
        return CONFD_FALSE;
    }

    /* skip the '/' */
    s++;
    if ((plen = get_uint(&s, ep)) < 0 || plen > 128)
        return CONFD_FALSE;
    /* check that only whitespace remains */
    skip_space(&s, ep);
    if (s < ep)
        return CONFD_FALSE;
    addr_mask.len = plen;

    CONFD_SET_IPV6PREFIX(v, addr_mask);

    return CONFD_TRUE;
}

static int ipv6_mask_val_to_str(struct confd_type *self,
                                struct confd_type_ctx *ctx,
                                const confd_value_t *v,
                                char *str, unsigned int len,
                                const char **strp)
{
    struct confd_ipv6_prefix addr_mask;
    char dst[INET6_ADDRSTRLEN];

    if (v->type != C_IPV6PREFIX)
        return -1;
    addr_mask = CONFD_GET_IPV6PREFIX(v);
    if (inet_ntop(AF_INET6, &addr_mask.ip6, dst, sizeof(dst)) == NULL)
        return -1;
    return snprintf(str, len, "%s/%d", dst, addr_mask.len);
}

static int ipv6_mask_validate(struct confd_type *self,
                              struct confd_type_ctx *ctx,
                              const confd_value_t *v)
{
    struct confd_ipv6_prefix addr_mask;

    if (v->type != C_IPV6PREFIX)
        return CONFD_FALSE;
    addr_mask = CONFD_GET_IPV6PREFIX(v);
    if (addr_mask.len > 128)
        return CONFD_FALSE;
    return CONFD_TRUE;
}


static struct confd_type type[] = {
    { .str_to_val = port_str_to_val,
      .val_to_str = port_val_to_str,
      .validate   = port_validate },
    { .str_to_val = ipv4_mask_str_to_val,
      .val_to_str = ipv4_mask_val_to_str,
      .validate   = ipv4_mask_validate },
    { .str_to_val = ipv6_mask_str_to_val,
      .val_to_str = ipv6_mask_val_to_str,
      .validate   = ipv6_mask_validate }
};

static struct confd_type_cbs tcb[] = {
    { types__typepointid_port_type, &type[0] },
    { types__typepointid_ipv4_mask, &type[1] },
    { types__typepointid_ipv6_mask, &type[2] }
};

int confd_type_cb_init(struct confd_type_cbs **tcbp)
{
    *tcbp = &tcb[0];
    return sizeof(tcb) / sizeof(tcb[0]);
}
