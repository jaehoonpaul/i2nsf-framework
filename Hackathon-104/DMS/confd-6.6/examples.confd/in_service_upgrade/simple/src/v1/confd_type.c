
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <ctype.h>

#include <confd_lib.h>

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


static struct confd_type type[] = {
    { .str_to_val = ipv4_mask_str_to_val,
      .val_to_str = ipv4_mask_val_to_str,
      .validate   = ipv4_mask_validate }
};

static struct confd_type_cbs tcb[] = {
    { "ipv4_mask", &type[0] }
};

int confd_type_cb_init(struct confd_type_cbs **tcbp)
{
    *tcbp = &tcb[0];
    return sizeof(tcb) / sizeof(tcb[0]);
}
