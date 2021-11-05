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

#include <stdlib.h>

// get the ConfD leaf-list value from the input bitmask value
// this is a mapping between the "/user-storage/ownership{}/content-type"
// and "/folder-user{}/managed-folder{}/content-type" elements
void content_type_bitmask_to_leaflist(
    confd_value_t *content_bits,
    confd_value_t *output
) {
    TRACE_ENTER("");
    print_val("input content type bits", content_bits);

    CONFD_SET_NOEXISTS(output);
    if (NULL == content_bits || C_NOEXISTS == content_bits->type) {
        goto term;
    }

    uint32_t bits_set = CONFD_GET_BIT32(content_bits);

    unsigned int size = 0;
    confd_value_t *ptr = malloc(3 * sizeof(*ptr));

    // Yang defines three different bits only, we wont bother with "clean"
    // implementation with some "for elem in set" - not as light/short in C
    // as e.g. in python.
    // beware - don't forget the ordering of enumeration values not to cause
    // configuration diff conflicts etc.

    if (bits_set & storage__bm_t3_media) {
        CONFD_SET_ENUM_VALUE(&ptr[size], folders_media);
        size++;
    }
    if (bits_set & storage__bm_t3_document) {
        CONFD_SET_ENUM_VALUE(&ptr[size], folders_document);
        size++;
    }
    if (bits_set & storage__bm_t3_archive) {
        CONFD_SET_ENUM_VALUE(&ptr[size], folders_archive);
        size++;
    }

    CONFD_SET_LIST(output, ptr, size);
term:
    print_val("output list", output);
    TRACE_EXIT("");
}

// retrieve the content-type data from pending transaction
int get_folder_content_type(
    struct confd_trans_ctx *tctx,
    confd_value_t *user_id,
    confd_value_t *storage_id,
    confd_value_t *output
) {
    TRACE_ENTER("");
    print_val("user-id", user_id);
    print_val("storage-id", storage_id);
    int ret = CONFD_OK;

    confd_value_t content_type_bits;
    CONFD_SET_NOEXISTS(&content_type_bits);

    ret = maapi_get_elem(glob.maapi_socket, tctx->thandle, &content_type_bits,
            "%s{%x %x}/content-type", OWNER_PATH, user_id, storage_id);

    if (CONFD_OK != ret && CONFD_ERR_NOEXISTS != confd_errno) {
        WARN("Failed to get content-type!");
    } else {
        ret = CONFD_OK;
        content_type_bitmask_to_leaflist(&content_type_bits, output);
    }

    TRACE_EXIT("(%d)", ret);
    return ret;
}

int cb_get_elem(struct confd_trans_ctx *tctx, confd_hkeypath_t *kp)
{
    TRACE_ENTER("");
    print_path("GET_ELEM() request keypath", kp);

    int ret = CONFD_OK;

    int sock = glob.maapi_socket;
    int th = tctx->thandle;

    confd_value_t v_result;
    CONFD_SET_NOEXISTS(&v_result);

    uint32_t leaf_tag = CONFD_GET_XMLTAG(&kp->v[0][0]);
    uint32_t list_tag = CONFD_GET_XMLTAG(&kp->v[2][0]);

    confd_value_t *username = &kp->v[kp->len-2][0];
    confd_value_t *folder_id = (folders_managed_folder == list_tag) ?
                                    &kp->v[1][0] : NULL;

    confd_value_t v_user_id;
    get_user_id_by_username(tctx, username, &v_user_id);

    confd_value_t v_storage_id;
    get_ll_storage_id(tctx, username, folder_id, &v_storage_id);

    switch (leaf_tag) {
        case folders_username:
            if (C_NOEXISTS != v_user_id.type) {
                // user with specific username actually does exists - as we got
                // his user_id in the previous step via storage-id retrieval
                confd_value_dup_to(username, &v_result);
            }
            break;

        case folders_auth_password:
            if (storage_at_password == get_ll_auth_type(tctx, &v_user_id)) {
                ret = maapi_get_elem(sock, th, &v_result,
                        USER_PATH "{%x}/auth-info/password", &v_user_id);
            }
            break;

        case folders_auth_key:
            if (storage_at_key == get_ll_auth_type(tctx, &v_user_id)) {
                ret = maapi_get_elem(sock, th, &v_result,
                        USER_PATH "{%x}/auth-info/auth-key", &v_user_id);
            }
            break;

        case folders_folder_id:
            if (C_NOEXISTS != v_storage_id.type
                    && maapi_exists(sock, th, STORAGE_PATH "{%x}",
                            &v_storage_id)
            ) {
                // existence verified in low level, return the val from keypath
                // - it saves the need to extract storage-id substring again...
                confd_value_dup_to(folder_id, &v_result);
            }
            break;

        case folders_content_type:
            ret = get_folder_content_type(tctx, &v_user_id, &v_storage_id,
                    &v_result);
            break;

        default:
            TRACE("Unsupported tag! (%s)",
                    confd_xmltag2str(storage__ns, leaf_tag));
            ret = CONFD_ERR;
    }

    if (CONFD_OK == ret) {
        // if we got a value in one of previous steps, forward it to ConfD
        if (C_NOEXISTS != v_result.type) {
            confd_data_reply_value(tctx, &v_result);
        } else {
            // else respond "not-exists"
            confd_data_reply_not_found(tctx);
        }
    }

    confd_free_value(&v_storage_id);
    confd_free_value(&v_result);

    TRACE_EXIT("(%d)", ret);
    return ret;
}