%%%-------------------------------------------------------------------
%%% @copyright 2006 Tail-F Systems AB
%%% @version {$Id: confd.hrl 6279 2006-10-30 13:44:03Z mbj $}
%%%-------------------------------------------------------------------

%% Keep in sync with confd_lib.h.in and ErrorCode.java and
%% confd_lib.c/confd_strerror() and confd_lib_lib(3)/ERRORS !!!!

-define( CONFD_ERR_NOEXISTS,                1).
-define( CONFD_ERR_ALREADY_EXISTS,          2).
-define( CONFD_ERR_ACCESS_DENIED,           3).
-define( CONFD_ERR_NOT_WRITABLE,            4).
-define( CONFD_ERR_BADTYPE,                 5).
-define( CONFD_ERR_NOTCREATABLE,            6).
-define( CONFD_ERR_NOTDELETABLE,            7).
-define( CONFD_ERR_BADPATH,                 8).
-define( CONFD_ERR_NOSTACK,                 9).
-define( CONFD_ERR_LOCKED,                 10).
-define( CONFD_ERR_INUSE,                  11).

%% Error codes from apply and validate
-define( CONFD_ERR_NOTSET,                  12).
-define( CONFD_ERR_NON_UNIQUE,              13).
-define( CONFD_ERR_BAD_KEYREF,              14).
-define( CONFD_ERR_TOO_FEW_ELEMS,           15).
-define( CONFD_ERR_TOO_MANY_ELEMS,          16).
-define( CONFD_ERR_BADSTATE,                17).

-define( CONFD_ERR_INTERNAL,                18).
-define( CONFD_ERR_EXTERNAL,                19).
-define( CONFD_ERR_MALLOC,                  20).
-define( CONFD_ERR_PROTOUSAGE,              21).
-define( CONFD_ERR_NOSESSION,               22).
-define( CONFD_ERR_TOOMANYTRANS,            23).
-define( CONFD_ERR_OS,                      24).

%% HA related error codes
-define(CONFD_ERR_HA_CONNECT,          25).
-define(CONFD_ERR_HA_CLOSED,           26).
-define(CONFD_ERR_HA_BADFXS,           27).
-define(CONFD_ERR_HA_BADTOKEN,         28).
-define(CONFD_ERR_HA_BADNAME,          29).
-define(CONFD_ERR_HA_BIND,             30).
-define(CONFD_ERR_HA_NOTICK,           31).

-define(CONFD_ERR_VALIDATION_WARNING,  32).
-define(CONFD_ERR_SUBAGENT_DOWN,       33).
-define(CONFD_ERR_TOO_MANY_SESSIONS,   35). %% 34 is used internally in libconfd
-define(CONFD_ERR_BAD_CONFIG,          36).

%% maapi for extended errors
-define(CONFD_ERR_RESOURCE_DENIED,      37).
-define(CONFD_ERR_INCONSISTENT_VALUE,   38).
-define(CONFD_ERR_APPLICATION_INTERNAL, 39).

%% More error codes from apply and validate
-define(CONFD_ERR_UNSET_CHOICE,        40).
-define(CONFD_ERR_MUST_FAILED,         41).
-define(CONFD_ERR_MISSING_INSTANCE,    42).
-define(CONFD_ERR_INVALID_INSTANCE,    43).

%% set/get_attr (and perhaps others)
-define(CONFD_ERR_UNAVAILABLE,         44).

%% maapi_move*
-define(CONFD_ERR_NOTMOVABLE,          46). %% 45 is used internally in libconfd

%% in-service upgrade vs ha state conflict
-define(CONFD_ERR_HA_WITH_UPGRADE,     47).

%% timeout - currently only for init_upgrade
-define(CONFD_ERR_TIMEOUT,             48).

%% aborted - currently only for init_upgrade
-define(CONFD_ERR_ABORTED,             49).

%% xpath compilation/evaluation
-define(CONFD_ERR_XPATH,               50).

%% not implemented CDB/MAAPI op
-define(CONFD_ERR_NOT_IMPLEMENTED,     51).

%% HA incompatible version
-define(CONFD_ERR_HA_BADVSN,           52).

%% error codes for configuration policies from apply and validate
-define(CONFD_ERR_POLICY_FAILED,             53).
-define(CONFD_ERR_POLICY_COMPILATION_FAILED, 54).
-define(CONFD_ERR_POLICY_EVALUATION_FAILED,  55).

%% ncs specific errors
-define(NCS_ERR_CONNECTION_REFUSED,          56).

%% error code when maapi_start_phase() fails
-define(CONFD_ERR_START_FAILED,              57).

%% more maapi for extended error
-define(CONFD_ERR_DATA_MISSING,              58).

%% error code for cli command
-define(CONFD_ERR_CLI_CMD,                   59).

%% error code for disallowed operations in upgrade phase.
-define(CONFD_ERR_UPGRADE_IN_PROGRESS,       60).

%% invalid transaction handle passed to a maapi function
-define(CONFD_ERR_NOTRANS,                   61).

%% NCS specific errors
-define(NCS_ERR_SERVICE_CONFLICT,            62).
-define(NCS_ERR_CONNECTION_TIMEOUT,          63).
-define(NCS_ERR_CONNECTION_CLOSED,           64).
-define(NCS_ERR_DEVICE,                      65).
-define(NCS_ERR_TEMPLATE,                    66).
