/*********************************************************************
 * ConfD Transformation callpoint example
 *
 * This is ConfD Sample Code.
 *
 * (C) 2017 Tail-f Systems
 * Permission to use this code as a starting point hereby granted
 *
 * See the README file for more information
 ********************************************************************/

#include "cb_common.h"

int cb_exists_optional(struct confd_trans_ctx *tctx, confd_hkeypath_t *kp)
{
    TRACE_ENTER("");
    print_path("EXISTS_OPTIONAL() request keypath", kp);

    int ret = CONFD_OK;

    int does_exist = 0;

    int32_t leaf_tag = CONFD_GET_XMLTAG(&kp->v[0][0]);

    confd_value_t v_user_id;
    get_user_id_by_username(tctx, &kp->v[1][0], &v_user_id);

    switch (leaf_tag) {
        case folders_auth_none:
            does_exist =
                    (storage_at_none == get_ll_auth_type(tctx, &v_user_id));
            break;

        default:
           TRACE("Unsupported leaf! (%s)",
                   confd_xmltag2str(folders__ns, leaf_tag));
    }

    TRACE("does exist == %d", does_exist);
    if (does_exist) {
        confd_data_reply_found(tctx);
    } else {
        confd_data_reply_not_found(tctx);
    }

    TRACE_EXIT("(%d)", ret);
    return ret;
}
