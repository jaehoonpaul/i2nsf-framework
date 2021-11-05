%%%-------------------------------------------------------------------
%%% @copyright 2006 Tail-F Systems AB
%%% @version {$Id$}
%%% @doc A blocking Erlang interface equivalent to the CDB C-API
%%%-------------------------------------------------------------------

-ifndef(PROTO_HRL).
-define(PROTO_HRL, true).

%%- COMMON START - these definitions are common to erlang and c

-define(CONFD_PROTO_ID,                          0).
-define(CONFD_PROTO_DAEMON,                      1).
-define(CONFD_PROTO_WORKER,                      2).
-define(CONFD_PROTO_REGISTER,                    3).
-define(CONFD_PROTO_REGISTER_RANGE,              4).
-define(CONFD_PROTO_DEBUG,                       5).
-define(CONFD_PROTO_ERROR,                       6).
-define(CONFD_PROTO_WARNING,                     7).
-define(CONFD_GET_CRYPTO_KEYS,                   8).
-define(CONFD_TRANS_CB_REGISTER,                 9).
-define(CONFD_DB_CB_REGISTER,                   10).
-define(CONFD_PROTO_REGISTER_DONE,              11).
-define(CONFD_PROTO_OLD_USESS,                  12).

-define(CONFD_PROTO_REQUEST,                    15).
-define(CONFD_PROTO_REGISTER_NANO,              16).


%%- trans callback
-define(CONFD_PROTO_NEW_TRANS,                  20).
-define(CONFD_PROTO_TRANS_LOCK,                 21).
-define(CONFD_PROTO_TRANS_UNLOCK,               22).
-define(CONFD_PROTO_WRITE_START,                23).
-define(CONFD_PROTO_PREPARE,                    24).
-define(CONFD_PROTO_ABORT,                      25).
-define(CONFD_PROTO_COMMIT,                     26).
-define(CONFD_PROTO_CLOSE_TRANS,                27).
-define(CONFD_PROTO_CALLBACK,                   28).
-define(CONFD_PROTO_CALLBACK_TIMEOUT,           29).
-define(CONFD_PROTO_INTERRUPT,                  30).

%%- flags for trans callbacks
-define(MASK_TR_INIT,         (1 bsl 0)).
-define(MASK_TR_TRANS_LOCK,   (1 bsl 1)).
-define(MASK_TR_TRANS_UNLOCK, (1 bsl 2)).
-define(MASK_TR_WRITE_START,  (1 bsl 3)).
-define(MASK_TR_PREPARE,      (1 bsl 4)).
-define(MASK_TR_ABORT,        (1 bsl 5)).
-define(MASK_TR_COMMIT,       (1 bsl 6)).
-define(MASK_TR_FINISH,       (1 bsl 7)).
-define(MASK_TR_INTERRUPT,    (1 bsl 8)).


%%- user session
-define(CONFD_PROTO_NEW_USESS,                  40).
-define(CONFD_PROTO_CLOSE_USESS,                41).


%%- db callbacks
-define(CONFD_PROTO_CANDIDATE_CONFIRMING_COMMIT, 60).
-define(CONFD_PROTO_CANDIDATE_ROLLBACK_RUNNING,  61).
-define(CONFD_PROTO_CANDIDATE_COMMIT,            62).
-define(CONFD_PROTO_CANDIDATE_RESET,             63).
-define(CONFD_PROTO_CANDIDATE_VALIDATE,          64).
-define(CONFD_PROTO_CANDIDATE_CHK_NOT_MODIFIED,  65).
-define(CONFD_PROTO_LOCK,                        66).
-define(CONFD_PROTO_UNLOCK,                      67).
-define(CONFD_PROTO_DELETE_CONFIG,               68).
-define(CONFD_PROTO_ADD_CHECKPOINT_RUNNING,      69).
-define(CONFD_PROTO_DEL_CHECKPOINT_RUNNING,      70).
-define(CONFD_PROTO_ACTIVATE_CHECKPOINT_RUNNING, 71).
-define(CONFD_PROTO_COPY_RUNNING_TO_STARTUP,     72).
-define(CONFD_PROTO_LOCK_PARTIAL,                73).
-define(CONFD_PROTO_UNLOCK_PARTIAL,              74).
-define(CONFD_PROTO_RUNNING_CHK_NOT_MODIFIED,    75).

-define(CONFD_PROTO_DB_REPLY,                    90).

%%- flags for db callbacks
-define(MASK_DB_CANDIDATE_COMMIT,               (1 bsl 0)).
-define(MASK_DB_CANDIDATE_CONFIRMING_COMMIT,    (1 bsl 1)).
-define(MASK_DB_CANDIDATE_RESET,                (1 bsl 2)).
-define(MASK_DB_CANDIDATE_CHK_NOT_MODIFIED,     (1 bsl 3)).
-define(MASK_DB_CANDIDATE_ROLLBACK_RUNNING,     (1 bsl 4)).
-define(MASK_DB_CANDIDATE_VALIDATE,             (1 bsl 5)).
-define(MASK_DB_ADD_CHECKPOINT_RUNNING,         (1 bsl 6)).
-define(MASK_DB_DEL_CHECKPOINT_RUNNING,         (1 bsl 7)).
-define(MASK_DB_ACTIVATE_CHECKPOINT_RUNNING,    (1 bsl 8)).
-define(MASK_DB_COPY_RUNNING_TO_STARTUP,        (1 bsl 9)).
-define(MASK_DB_LOCK,                           (1 bsl 10)).
-define(MASK_DB_UNLOCK,                         (1 bsl 11)).
-define(MASK_DB_DELETE_CONFIG,                  (1 bsl 12)).
-define(MASK_DB_LOCK_PARTIAL,                   (1 bsl 13)).
-define(MASK_DB_UNLOCK_PARTIAL,                 (1 bsl 14)).
-define(MASK_DB_RUNNING_CHK_NOT_MODIFIED,       (1 bsl 15)).


%%- data callbacks
-define(CONFD_DATA_CB_GET_NEXT,                   101).
-define(CONFD_DATA_CB_GET_ELEM,                   102).
-define(CONFD_DATA_CB_GET_OBJECT,                 103).
-define(CONFD_DATA_CB_SET_ELEM,                   104).
-define(CONFD_DATA_CB_CREATE,                     105).
-define(CONFD_DATA_CB_DELETE,                     106).
-define(CONFD_DATA_CB_EXISTS_OPTIONAL,            107).
-define(CONFD_DATA_CB_NUM_INSTANCES,              108).
-define(CONFD_DATA_CB_GET_NEXT_OBJECT,            109).
-define(CONFD_DATA_CB_GET_CASE,                   110).
-define(CONFD_DATA_CB_SET_CASE,                   111).
-define(CONFD_DATA_CB_GET_ATTRS,                  112).
-define(CONFD_DATA_CB_SET_ATTR,                   113).
-define(CONFD_DATA_CB_MOVE_AFTER,                 114).
-define(CONFD_DATA_CB_WRITE_ALL,                  115).
-define(CONFD_DATA_CB_FIND_NEXT,                  116).
-define(CONFD_DATA_CB_FIND_NEXT_OBJECT,           117).

%%- flags for data callbacks
-define(MASK_DATA_EXISTS_OPTIONAL,  (1 bsl 0)).
-define(MASK_DATA_GET_ELEM,         (1 bsl 1)).
-define(MASK_DATA_GET_NEXT,         (1 bsl 2)).
-define(MASK_DATA_SET_ELEM,         (1 bsl 3)).
-define(MASK_DATA_CREATE,           (1 bsl 4)).
-define(MASK_DATA_REMOVE,           (1 bsl 5)).
-define(MASK_DATA_NUM_INSTANCES,    (1 bsl 6)).
-define(MASK_DATA_GET_OBJECT,       (1 bsl 7)).
-define(MASK_DATA_GET_NEXT_OBJECT,  (1 bsl 8)).
-define(MASK_DATA_GET_CASE,         (1 bsl 9)).
-define(MASK_DATA_SET_CASE,         (1 bsl 10)).
-define(MASK_DATA_GET_ATTRS,        (1 bsl 11)).
-define(MASK_DATA_SET_ATTR,         (1 bsl 12)).
-define(MASK_DATA_MOVE_AFTER,       (1 bsl 13)).
-define(MASK_DATA_WRITE_ALL,        (1 bsl 14)).
-define(MASK_DATA_FIND_NEXT,        (1 bsl 15)).
-define(MASK_DATA_FIND_NEXT_OBJECT, (1 bsl 16)).


%%- callback type
-define(CONFD_PROTO_DATA_CB,                    130).
-define(CONFD_PROTO_VALIDATE_CB,                131).
-define(CONFD_PROTO_ACTION_CB,                  132).
-define(CONFD_PROTO_NOTIF_STREAM_CB,            133).
-define(CONFD_PROTO_NOTIF_SUB_CB,               134).
-define(CONFD_PROTO_NOTIF_SUB_SNMP_CB,          135).
-define(CONFD_PROTO_NOTIF_SNMP_INFORM_CB,       136).
-define(CONFD_PROTO_AUTHORIZATION_CB,           137).
-define(CONFD_PROTO_USERTYPE_CB,                138).
-define(CONFD_PROTO_SERVICE_CB,                 139).
-define(CONFD_PROTO_NANO_SERVICE_CB,            140).


%%- validate callbacks
-define(CONFD_PROTO_NEW_VALIDATE,               145).
-define(CONFD_PROTO_CLOSE_VALIDATE,             146).
-define(CONFD_VALIDATE_VALUE,                   147).


%%- action callbacks
-define(CONFD_PROTO_NEW_ACTION,                 150).
-define(CONFD_PROTO_ABORT_ACTION,               151).
-define(CONFD_CALL_ACTION,                      152).
-define(CONFD_CALL_ACTION_COMMAND,              153).
-define(CONFD_CALL_ACTION_COMPLETION,           154).
-define(CONFD_CALL_ACTION_SYNC,                 155).

%%- flags for action callbacks
-define(MASK_ACT_INIT,         (1 bsl 0)).
-define(MASK_ACT_ABORT,        (1 bsl 1)).
-define(MASK_ACT_ACTION,       (1 bsl 2)).
-define(MASK_ACT_COMMAND,      (1 bsl 3)).
-define(MASK_ACT_COMPLETION,   (1 bsl 4)).


%%- notification callbacks
-define(CONFD_PROTO_NOTIF_GET_LOG_TIMES,        160).
-define(CONFD_PROTO_NOTIF_REPLAY,               161).
-define(CONFD_PROTO_NOTIF_RECV_SNMP,            162).
-define(CONFD_PROTO_NOTIF_SNMP_INFORM_TARGETS,  163).
-define(CONFD_PROTO_NOTIF_SNMP_INFORM_RESULT,   164).

%%- flags for notification callbacks
-define(MASK_NOTIF_GET_LOG_TIMES,        (1 bsl 0)).
-define(MASK_NOTIF_REPLAY,               (1 bsl 1)).

%%- flags for SNMP inform callbacks
-define(MASK_NOTIF_SNMP_INFORM_TARGETS,  (1 bsl 0)).
-define(MASK_NOTIF_SNMP_INFORM_RESULT,   (1 bsl 1)).

%%- notification requests
-define(CONFD_PROTO_NOTIF_SEND,                 170).
-define(CONFD_PROTO_NOTIF_REPLAY_COMPLETE,      171).
-define(CONFD_PROTO_NOTIF_REPLAY_FAILED,        172).
-define(CONFD_PROTO_NOTIF_SEND_SNMP,            173).
-define(CONFD_PROTO_NOTIF_FLUSH,                174).

%%- aux callbacks
-define(CONFD_PROTO_ERROR_CB,                   180).
-define(CONFD_PROTO_AUTH_CB,                    181).

%%- error types for error cb
-define(CONFD_ERRTYPE_VALIDATION,         (1 bsl 0)).

%%- authorization callbacks
-define(CONFD_PROTO_CHK_CMD_ACCESS,             190).
-define(CONFD_PROTO_CHK_DATA_ACCESS,            191).

-define(CONFD_PROTO_DAEMON_TIMEOUT,             195).

%%- flags for authorization callbacks
-define(MASK_CHK_CMD_ACCESS,              (1 bsl 0)).
-define(MASK_CHK_DATA_ACCESS,             (1 bsl 1)).

%%- 'op'/'how' bit values for authorization callbacks
-define(CONFD_ACCESS_OP_READ,             (1 bsl 0)).
-define(CONFD_ACCESS_OP_EXECUTE,          (1 bsl 1)).
-define(CONFD_ACCESS_OP_CREATE,           (1 bsl 2)).
-define(CONFD_ACCESS_OP_UPDATE,           (1 bsl 3)).
-define(CONFD_ACCESS_OP_DELETE,           (1 bsl 4)).
-define(CONFD_ACCESS_OP_WRITE,            (1 bsl 5)).
-define(CONFD_ACCESS_CHK_INTERMEDIATE,    (1 bsl 8)).
-define(CONFD_ACCESS_CHK_FINAL,           (1 bsl 9)).

%%- NCS service callbacks
-define(CONFD_SERVICE_CB_PRE_MODIFICATION,      200).
-define(CONFD_SERVICE_CB_POST_MODIFICATION,     201).
-define(CONFD_SERVICE_CB_CREATE,                202).
-define(CONFD_SERVICE_CB_UPDATE,                203).
-define(CONFD_SERVICE_CB_DELETE,                204).
-define(CONFD_SERVICE_CB_PRE_LOCK_CREATE,       205).

%%- NCS nano service callbacks
-define(CONFD_NANO_SERVICE_CB_CREATE,           210).
-define(CONFD_NANO_SERVICE_CB_DELETE,           211).


%%- flags for NCS service callbacks
-define(MASK_SERVICE_PRE_MODIFICATION,    (1 bsl 0)).
-define(MASK_SERVICE_POST_MODIFICATION,   (1 bsl 1)).
-define(MASK_SERVICE_CREATE,              (1 bsl 2)).
-define(MASK_SERVICE_UPDATE,              (1 bsl 3)).
-define(MASK_SERVICE_DELETE,              (1 bsl 4)).
-define(MASK_SERVICE_PRE_LOCK_CREATE,     (1 bsl 5)).

%%- flags for NCS service callbacks
-define(MASK_NANO_SERVICE_CREATE,         (1 bsl 0)).
-define(MASK_NANO_SERVICE_DELETE,         (1 bsl 1)).

%%- commands for type_drv
-define(CONFD_TYPECMD_INIT,           0).
-define(CONFD_TYPECMD_LOAD,           1).
-define(CONFD_TYPECMD_GET_POINTS,     2).
-define(CONFD_TYPECMD_STR2VAL,        3).
-define(CONFD_TYPECMD_VAL2STR,        4).
-define(CONFD_TYPECMD_CHECK_VAL,      5).
-define(CONFD_TYPECMD_SAVE_STATE,     6).
-define(CONFD_TYPECMD_CLEAN_STATE,    7).
-define(CONFD_TYPECMD_CLEAN_SAVED,    8).
-define(CONFD_TYPECMD_RESTORE_SAVED,  9).


%%- COMMON END

%% keep in sync with confd_lib.h.in enum confd_dbname
-define(CONFD_PROTO_CANDIDATE,   1).
-define(CONFD_PROTO_RUNNING,     2).
-define(CONFD_PROTO_STARTUP,     3).
-define(CONFD_PROTO_OPERATIONAL, 4).
-define(CONFD_PROTO_TRANSACTION, 5).  % trans_in_trans
-define(CONFD_PROTO_PRE_COMMIT_RUNNING, 6).

%% session type
-define(CONFD_PROTO_DATA_SESS,     1).

%% keep in sync with confd_lib.h.in enum confd_trans_mode
-define(CONFD_PROTO_READ,       1).
-define(CONFD_PROTO_READ_WRITE, 2).

%% keep in sync with confd_lib.h.in enum confd_find_next_type
-define(CONFD_PROTO_FIND_NEXT,         0).
-define(CONFD_PROTO_FIND_SAME_OR_NEXT, 1).

-endif.
