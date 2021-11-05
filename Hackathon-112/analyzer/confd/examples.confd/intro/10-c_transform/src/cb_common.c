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

#include <string.h>

// string separator used when creating/parsing the low level storage-id
#define STORAGE_ID_SEPARATOR "||"

// iteration function for the maapi_xpath_eval()
// used in our username <-> user-id transformation
static int user_id_iter(confd_hkeypath_t *kp, confd_value_t *val,void *state)
{
    TRACE_ENTER("");
    print_path("matched keypath", kp);
    print_val("found key value", val);

    confd_value_dup_to(val, (confd_value_t *) state);

    TRACE_EXIT("");
    // due to how target yang model is written, there's "no way" more than one
    // record would be returned (unique "username"; in yang)
    return ITER_STOP;
}

// User is identified by "username" in our "folder" model, but that's not a key
// in the target "storage" model.
// This procedure uses XPath to get the user-id of specific "username" from
// the transaction.
int get_user_id_by_username(
    struct confd_trans_ctx *tctx,
    confd_value_t *username,
    confd_value_t *output
) {
    TRACE_ENTER("");
    print_val("username", username);

    int ret = CONFD_OK;
    CONFD_SET_NOEXISTS(output);

    char xpath_expr[TRANSFORM_BUFF_LEN];
    snprintf(xpath_expr, TRANSFORM_BUFF_LEN,
            "%s[username=\"%s\"]/user-id",
            USER_PATH, username->val.c_buf.ptr);
    TRACE("xpath to check: %s", xpath_expr);

    // find all (the only) the users that have specific username (non-key leaf)
    ret = maapi_xpath_eval(glob.maapi_socket, tctx->thandle, xpath_expr,
            user_id_iter, NULL, output, NULL);

    TRACE_EXIT("(%d)", ret);
    return ret;
}

void get_ll_storage_id(
    struct confd_trans_ctx *tctx,
    confd_value_t *username,
    confd_value_t *folder_id,
    confd_value_t *output
) {
    TRACE_ENTER("");
    print_val("input username", username);
    print_val("input folder-id", folder_id);

    CONFD_SET_NOEXISTS(output);

    if (NULL == folder_id) {
        goto term;
    }

    confd_value_t v_user_id;
    get_user_id_by_username(tctx, username, &v_user_id);
    if (C_NOEXISTS == v_user_id.type) {
        goto term;
    }

    // low level storage id is a concatenation of user-id + folder-id;
    // this allows two different users to have a storage with same name
    char buff[TRANSFORM_BUFF_LEN];
    int str_len = 0;
    str_len += confd_pp_value(buff + str_len, TRANSFORM_BUFF_LEN - str_len,
                    &v_user_id);
    str_len += snprintf(buff + str_len, TRANSFORM_BUFF_LEN - str_len,
                    STORAGE_ID_SEPARATOR);
    str_len += confd_pp_value(buff + str_len, TRANSFORM_BUFF_LEN - str_len,
                    folder_id);

    confd_value_t v_concat;
    CONFD_SET_CBUF(&v_concat, buff, str_len);
    confd_value_dup_to(&v_concat, output);

term:
    print_val("output storage-id", output);
    TRACE_EXIT("");
}

int32_t get_ll_auth_type(
    struct confd_trans_ctx *tctx,
    confd_value_t *user_id
) {
    TRACE_ENTER("");
    print_val("input user-id", user_id);

    confd_value_t val;
    CONFD_SET_NOEXISTS(&val);

    maapi_get_elem(glob.maapi_socket, tctx->thandle, &val,
            "%s{%x}/auth-info/auth-type", USER_PATH, user_id);
    print_val("user's auth-type", &val);

    int32_t auth_type = CONFD_GET_ENUM_VALUE(&val);

    TRACE_EXIT("auth type == %d", auth_type);
    return auth_type;
}

int set_ll_auth_type(
    struct confd_trans_ctx *tctx,
    confd_value_t  *user_id,
    int32_t auth_type
) {
    TRACE_ENTER("(auth type == %d", auth_type);
    confd_value_t val;
    CONFD_SET_ENUM_VALUE(&val, auth_type);

    int ret = maapi_set_elem(glob.maapi_socket, tctx->thandle, &val,
                    USER_PATH "{%x}/auth-info/auth-type", user_id);

    TRACE_EXIT("");
    return ret;
}

void extract_folder_id(confd_value_t *storage_id, confd_value_t *output)
{
    TRACE_ENTER("");
    print_val("storage-id", storage_id);

    char buff[TRANSFORM_BUFF_LEN];
    confd_pp_value(buff, TRANSFORM_BUFF_LEN, storage_id);

    char *sep_ptr = strstr(buff, STORAGE_ID_SEPARATOR);
    if (NULL == sep_ptr) {
        TRACE("Separator not found in the storage-id!");
        // the storage-id seems not be created by this transform code...
        CONFD_SET_NOEXISTS(output);
        goto term;
    }
    int sep_len = strlen(STORAGE_ID_SEPARATOR);

    int folder_id_len;
    folder_id_len = CONFD_GET_BUFSIZE(storage_id) - (sep_ptr - buff) - sep_len;
    TRACE("output str length == %d", folder_id_len);

    confd_value_t val;
    CONFD_SET_CBUF(&val, sep_ptr + sep_len, folder_id_len);
    confd_value_dup_to(&val, output);

term:
    print_val("output", output);
    TRACE_EXIT("");
}