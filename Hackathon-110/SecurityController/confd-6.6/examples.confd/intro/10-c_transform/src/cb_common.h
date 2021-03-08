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

// common includes used across all the callback implementations
// are put here to save some copy&paste including...
#include <confd_lib.h>
#include <confd_dp.h>
#include <confd_maapi.h>

#include <traceh.h>

#include "common.h"

#include "user-storage.h"
#include "user-folders.h"

#define USER_PATH "/user-storage/user"
#define STORAGE_PATH "/user-storage/storage"
#define OWNER_PATH "/user-storage/ownership"

// retrieve value of "/user-storage/user{}/user-id" that has the specific
// unique username set as a non-key element
int get_user_id_by_username(
    struct confd_trans_ctx *tctx,
    confd_value_t *username,
    confd_value_t *output
);

// retrieve the key value of "/user-storage/storage{}" record
// that corresponds to a specific user's managed-folder
void get_ll_storage_id(
    struct confd_trans_ctx *tctx,
    confd_value_t *username,
    confd_value_t *folder_id,
    confd_value_t *output
);

// retrieve specific user's "/user-storage/user{}/authentication-type"
int32_t get_ll_auth_type(
    struct confd_trans_ctx *tctx,
    confd_value_t *user_id
);

// set the "/user-storage/user{}/authentication-type" to a specific value
int set_ll_auth_type(
    struct confd_trans_ctx *tctx,
    confd_value_t  *user_id,
    int32_t auth_type
);

// extract the value to be used as "/folder-user{}/managed-folder{}/folder-id"
// from the input value originating in the "/user-storage/storage{}/storage-id"
// that has been created by this transformation mapping/framework
void extract_folder_id(confd_value_t *storage_id, confd_value_t *output);