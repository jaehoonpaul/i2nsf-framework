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

// XPath iteration function for "batch" deletion of ownership records
static int ownership_iter(
    confd_hkeypath_t *kp,
    confd_value_t *val, void *state
) {
    TRACE_ENTER("");
    print_path("matched keypath", kp);
    print_val("found key value", val);

    struct confd_trans_ctx *tctx = (struct confd_trans_ctx *) state;

    int sock = glob.maapi_socket;
    int th = tctx->thandle;

    confd_value_t *user_id = &kp->v[1][0];
    confd_value_t *storage_id = &kp->v[1][1];

    OK(maapi_delete(sock, th, STORAGE_PATH "{%x}", storage_id));
    OK(maapi_delete(sock, th, OWNER_PATH "{%x %x}", user_id, storage_id));

    TRACE_EXIT("");
    return ITER_CONTINUE;
}

// remove all the data related to specific user's owned folders
static int remove_users_ownerships(
    struct confd_trans_ctx *tctx,
    confd_value_t *user_id
) {
    TRACE_ENTER("");
    int ret = CONFD_OK;

    char xpath_expr[TRANSFORM_BUFF_LEN];
    snprintf(xpath_expr, TRANSFORM_BUFF_LEN,
            "%s[user-id=\"%d\"]/user-id", OWNER_PATH, CONFD_GET_INT32(user_id));
    TRACE("xpath to check: %s", xpath_expr);

    // find all the user's ownerships - that have specific user-id
    ret = maapi_xpath_eval(glob.maapi_socket, tctx->thandle, xpath_expr,
            ownership_iter, NULL, tctx, NULL);

    TRACE_EXIT("(%d)", ret);
    return ret;
}

// remove all the data related to specific user
static int remove_folder_user(
    struct confd_trans_ctx *tctx,
    confd_value_t *username
) {
    TRACE_ENTER("");
    print_val("username", username);

    int ret = CONFD_OK;

    confd_value_t v_user_id;
    get_user_id_by_username(tctx, username, &v_user_id);

    OK(remove_users_ownerships(tctx, &v_user_id));

    OK(maapi_delete(glob.maapi_socket, tctx->thandle,
                USER_PATH "{%x}", &v_user_id));

    TRACE_EXIT("");
    return ret;
}

// remove all the data related to specific managed folder
static int remove_managed_folder(
    struct confd_trans_ctx *tctx,
    confd_value_t *username,
    confd_value_t *folder_id
) {
    TRACE_ENTER("");
    print_val("username", username);
    print_val("folder_id", folder_id);

    confd_value_t v_storage_id;
    get_ll_storage_id(tctx, username, folder_id, &v_storage_id);
    print_val("storage_id", &v_storage_id);

    OK(maapi_delete(glob.maapi_socket, tctx->thandle,
                STORAGE_PATH "{%x}", &v_storage_id));

    confd_value_t v_user_id;
    get_user_id_by_username(tctx, username, &v_user_id);

    OK(maapi_delete(glob.maapi_socket, tctx->thandle,
                OWNER_PATH "{%x %x}", &v_user_id, &v_storage_id));

    TRACE_EXIT("");
    return CONFD_OK;
}

// remove the data related to specific leaf being deleted in transformed model
static int remove_specific_leaf(
    struct confd_trans_ctx *tctx,
    confd_hkeypath_t *kp
) {
    TRACE_ENTER("");
    int ret = CONFD_OK;

    int32_t leaf_tag = CONFD_GET_XMLTAG(&kp->v[0][0]);

    int sock = glob.maapi_socket;
    int th = tctx->thandle;

    confd_value_t *username = (kp->len > 2) ? &kp->v[kp->len-2][0] : NULL;

    confd_value_t v_user_id;
    get_user_id_by_username(tctx, username, &v_user_id);

    switch (leaf_tag) {
        case folders_auth_none:
            ret = CONFD_OK; // nothing needs to be done...
            break;

        case folders_auth_key:
            ret = maapi_delete(sock, th, USER_PATH "{%x}/auth-info/key",
                    &v_user_id);
            break;

        case folders_auth_password:
            ret = maapi_delete(sock, th, USER_PATH "{%x}/auth-info/password",
                    &v_user_id);
            break;

        default:
            WARN("Unsupported leaf tag! (%d)", leaf_tag);
            ret = CONFD_ERR;
    }

    TRACE_EXIT("(%d)", ret);
    return ret;
}

int cb_remove(
    struct confd_trans_ctx *tctx,
    confd_hkeypath_t *kp
) {
    TRACE_ENTER("");
    print_path("REMOVE() request keypath", kp);
    int ret = CONFD_OK;

    if (C_XMLTAG == kp->v[0][0].type) {
        ret = remove_specific_leaf(tctx, kp);
        goto term;
    }

    uint32_t list_tag = CONFD_GET_XMLTAG(&kp->v[1][0]);
    confd_value_t *username = NULL;

    switch (list_tag) {
        case folders_folder_user:
            username = &kp->v[0][0];
            ret = remove_folder_user(tctx, username);
            break;

        case folders_managed_folder:
            username = &kp->v[2][0];
            confd_value_t *folder_id = &kp->v[0][0];
            ret = remove_managed_folder(tctx, username, folder_id);
            break;

        default:
            WARN("Unimplemented list! (tag == %d ~ %s",
                    list_tag, confd_xmltag2str(folders__ns, list_tag));
            ret = CONFD_ERR;
    }

term:
    TRACE_EXIT("(%d)", ret);
    return ret;
}