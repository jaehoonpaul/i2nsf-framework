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

// integer used as initial "user-id" to be assigned to newly created users;
// every new user will get the last used user-id +1;
// see further/usage of macro for details...
#define INIT_USER_ID 100

// prefix for the "hidden" data model mount-points leaf value
#define MOUNTPOINT_PREFIX "/mnt/user-storages"

// return the "free" user id that is not yet in the database, and can be used
// to create a new user in the "user-storage.yang" configuration
static int get_unused_user_id(struct confd_trans_ctx *tctx)
{
    TRACE_ENTER("");
    int ret = CONFD_OK;

    int sock = glob.maapi_socket;
    int th = tctx->thandle;

    // BEWARE - following "solution" is not safe out of "example" scenarios!
    // We assume that the user id numbers will not be very big, and we create
    // only the users with larger user-id than the previous ones used...
    struct maapi_cursor mc;
    maapi_init_cursor(sock, th, &mc, USER_PATH);
    maapi_get_next(&mc);

    int last_user_id = INIT_USER_ID;
    while (CONFD_OK == ret && 0 != mc.n) {
        last_user_id = CONFD_GET_INT32(&mc.keys[0]);
        ret = maapi_get_next(&mc);
    }
    maapi_destroy_cursor(&mc);

    // return largest + 1
    TRACE_EXIT("(%d)", last_user_id + 1);
    return (last_user_id + 1);
}

// stuff that needs to be done for the creation of a "/folder-user{}" record
static int create_folder_user(
    struct confd_trans_ctx *tctx,
    confd_value_t *username
) {
    TRACE_ENTER("");
    print_val("username", username);

    int32_t new_user_id = get_unused_user_id(tctx);

    // new table record in /user-storage/user{}
    OK(maapi_create(glob.maapi_socket, tctx->thandle,
        USER_PATH "{%d}", new_user_id));

    // username in the newly created record - /user-storage/user{}/username
    OK(maapi_set_elem(glob.maapi_socket, tctx->thandle, username,
        USER_PATH "{%d}/username", new_user_id));

    TRACE_EXIT("");
    return CONFD_OK;
}

// artificial mount-point that is not show in "/folder-user" model;
// for an example here, use a compile time defined string prefix
// - /mnt/user-id/storage-id
static int set_mountpoint(
    struct confd_trans_ctx *tctx,
    confd_value_t *user_id,
    confd_value_t *folder_id,
    confd_value_t *storage_id
) {
    TRACE_ENTER("");

    char mp_buff[TRANSFORM_BUFF_LEN];
    int str_len = snprintf(mp_buff, TRANSFORM_BUFF_LEN, "%s/%d/",
            MOUNTPOINT_PREFIX, CONFD_GET_INT32(user_id));
    confd_pp_value(mp_buff + str_len, TRANSFORM_BUFF_LEN - str_len, folder_id);
    confd_value_t val;
    CONFD_SET_CBUF(&val, mp_buff, strnlen(mp_buff, TRANSFORM_BUFF_LEN));
    print_val("mountpoint", &val);

    OK(maapi_set_elem(glob.maapi_socket, tctx->thandle, &val,
            STORAGE_PATH "{%x}/mountpoint", storage_id));

    TRACE_EXIT("");
    return CONFD_OK;
}

// stuff that needs to be done for the creation
// of a "/folder-user{}/managed-folder{}" record
static int create_managed_folder(
    struct confd_trans_ctx *tctx,
    confd_value_t *username,
    confd_value_t *folder_id
) {
    TRACE_ENTER("");
    print_val("username", username);
    print_val("storage_id", folder_id);

    int sock = glob.maapi_socket;
    int th = tctx->thandle;

    confd_value_t v_user_id;
    get_user_id_by_username(tctx, username, &v_user_id);

    confd_value_t v_storage_id;
    get_ll_storage_id(tctx, username, folder_id, &v_storage_id);

    // new record in /user-storage/storage{}
    OK(maapi_create(sock, th, STORAGE_PATH "{%x}", &v_storage_id));

    // new record in /user-storage/ownership{}
    OK(maapi_create(sock, th, OWNER_PATH "{%x %x}", &v_user_id, &v_storage_id));

    // "mandatory" mountpoint that is not accessible in folder-user model
    OK(set_mountpoint(tctx, &v_user_id, folder_id, &v_storage_id));

    confd_free_value(&v_storage_id);
    TRACE_EXIT("");
    return CONFD_OK;
}

// stuff that needs to be done for the creation
// of a "/folder-user{}/auth-none" record
static int create_type_empty(
    struct confd_trans_ctx *tctx,
    confd_hkeypath_t *kp
) {
    TRACE_ENTER("");

    if (C_XMLTAG != kp->v[0][0].type) {
        return CONFD_ERR;
    }

    int ret = CONFD_OK;
    int32_t tag = kp->v[0][0].val.xmltag.tag;

    if (folders_auth_none == tag) {
        confd_value_t *username = &kp->v[1][0];
        confd_value_t v_user_id;
        get_user_id_by_username(tctx, username, &v_user_id);
        ret = set_ll_auth_type(tctx, &v_user_id, storage_at_none);
    } else {
        ret = CONFD_ERR;
    }

    TRACE_EXIT("(%d)", ret);
    return ret;
}

int cb_create(struct confd_trans_ctx *tctx, confd_hkeypath_t *kp)
{
    TRACE_ENTER("");
    print_path("CREATE() request keypath", kp);

    int ret = CONFD_OK;

    if (C_XMLTAG == kp->v[0][0].type) {
        ret = create_type_empty(tctx, kp);
        goto term;
    }

    uint32_t list_tag = CONFD_GET_XMLTAG(&kp->v[1][0]);
    confd_value_t *username = NULL;

    switch (list_tag) {
        case folders_folder_user:
            username = &kp->v[0][0];
            ret = create_folder_user(tctx, username);
            break;

        case folders_managed_folder:
            username = &kp->v[2][0];
            confd_value_t *folder_id = &kp->v[0][0];
            ret = create_managed_folder(tctx, username, folder_id);
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