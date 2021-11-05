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

// returns a ConfD XML tag of the "choice" in YANG model depending on the input
// enumeration type of the "/user-storage/user{}/auth-info/auth-type"
static uint32_t get_auth_type_tag(int32_t auth_type_enum)
{
    TRACE_ENTER("auth type enum == %d", auth_type_enum);
    int ret;

    switch(auth_type_enum) {
        case storage_at_none:
            ret = folders_none;
            break;
        case storage_at_key:
            ret = folders_key;
            break;
        case storage_at_password:
            ret = folders_password;
            break;
        default:
            WARN("Unsupported auth-type! (%d)");
            ret = -1;
    }

    TRACE_EXIT("auth type tag == %d", ret);
    return ret;
}

int cb_get_case(
    struct confd_trans_ctx *tctx,
    confd_hkeypath_t *kp,
    confd_value_t *choice
) {
    TRACE_ENTER("");
    print_path("GET_CASE() request keypath", kp);
    print_val("request choice", choice);

    int ret = CONFD_OK;

    confd_value_t v_auth_type;
    CONFD_SET_NOEXISTS(&v_auth_type);

    confd_value_t *username = &(kp->v[kp->len-2][0]);

    confd_value_t v_user_id;
    get_user_id_by_username(tctx, username, &v_user_id);

    if (C_NOEXISTS == v_user_id.type) {
        confd_data_reply_not_found(tctx);
        goto term;
    }

    int32_t auth_type_enum = get_ll_auth_type(tctx, &v_user_id);
    confd_value_t val;
    CONFD_SET_XMLTAG(&val, get_auth_type_tag(auth_type_enum), folders__ns);
    confd_data_reply_value(tctx, &val);
    print_val("response", &val);

term:
    TRACE_EXIT("(%d)", ret);
    return ret;
}