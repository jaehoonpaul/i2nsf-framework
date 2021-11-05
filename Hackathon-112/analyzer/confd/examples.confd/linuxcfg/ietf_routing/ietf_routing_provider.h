#ifndef IETF_ROUTING_PROVIDER_H
#define IETF_ROUTING_PROVIDER_H

#include <confd.h>

// initialize opaque data required by other callbacks;
// caller must bind this pointer to the confd_trans_ctx t_opaque due to some
// expectation in get_next() callback...
void * routing_alloc_dp_data(void);

// cleanup the opaque data
void routing_clean_opaque(
    void * opaque_data
);

int routing_get_elem(
    struct confd_trans_ctx *tctx,
    confd_hkeypath_t *kp
);

// callback expects that there's opaque data bound in the tctx->t_opaque
// from the prior "routing_alloc_dp_data() invocation
int routing_get_next(
    struct confd_trans_ctx *tctx,
    confd_hkeypath_t *kp,
    long next
);

int routing_get_case(
    struct confd_trans_ctx *tctx,
    confd_hkeypath_t *kp,
    confd_value_t *choice
);

int routing_exists_optional(
    struct confd_trans_ctx *tctx,
    confd_hkeypath_t *kp
);

#endif // IETF_ROUTING_PROVIDER_H
