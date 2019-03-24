%%%-------------------------------------------------------------------
%%% @copyright 2006 Tail-F Systems AB
%%% @version {$Id: confd_proto.hrl 6279 2006-10-30 13:44:03Z mbj $}
%%% @doc A blocking Erlang interface equivalent to the CDB C-API
%%%-------------------------------------------------------------------

%%- COMMON START - these definitions are common to erlang and c and java

%% user session api
-define(MAAPI_START_USER_SESSION,              100).
-define(MAAPI_END_USER_SESSION,                101).
-define(MAAPI_KILL_USER_SESSION,               102).
-define(MAAPI_GET_USER_SESSIONS,               103).
-define(MAAPI_GET_USER_SESSION,                104).
-define(MAAPI_GET_MY_USER_SESSION,             105).
-define(MAAPI_RESTART_NODE,                    106).
-define(MAAPI_SET_USER_SESSION,                107).
-define(MAAPI_GET_USER_SESSION_IDENTIFICATION, 108).
-define(MAAPI_GET_USER_SESSION_OPAQUE,         109).
-define(MAAPI_GET_AUTHORIZATION_INFO,          110).
-define(MAAPI_SET_NEXT_USER_SESSION_ID,        111).

%% db api
-define(MAAPI_LOCK,                         120).
-define(MAAPI_UNLOCK,                       121).
-define(MAAPI_IS_LOCK_SET,                  122).
-define(MAAPI_CANDIDATE_VALIDATE,           123).
-define(MAAPI_DELETE_CONFIG,                124).
-define(MAAPI_CANDIDATE_COMMIT,             125).
-define(MAAPI_CANDIDATE_CONFIRMED_COMMIT,   126).
-define(MAAPI_CANDIDATE_RESET,              127).
-define(MAAPI_COPY_RUNNING_TO_STARTUP,      128).
-define(MAAPI_GET_NAMESPACES,               129).
-define(MAAPI_CONFIRMED_COMMIT_IN_PROGRESS, 130).
-define(MAAPI_CANDIDATE_ABORT_COMMIT,       131).
-define(MAAPI_LOCK_PARTIAL,                 132).
-define(MAAPI_UNLOCK_PARTIAL,               133).
-define(MAAPI_IS_RUNNING_MODIFIED,          134).
-define(MAAPI_IS_CANDIDATE_MODIFIED,        135).

%% maapi_get_last_errstr(int sock);

%% transaction api
-define(MAAPI_START_TRANS,             140).
-define(MAAPI_STOP_TRANS,              142).
-define(MAAPI_APPLY_TRANS,             143).
-define(MAAPI_VALIDATE_TRANS,          144).
-define(MAAPI_PREPARE_TRANS,           145).
-define(MAAPI_COMMIT_TRANS,            146).
-define(MAAPI_ABORT_TRANS,             147).
-define(MAAPI_COMMIT_QUEUE_RESULT,     148).
-define(MAAPI_APPLY_TRANS_WITH_RESULT, 149).

%% read/write api towards a transaction
-define(MAAPI_SET_NAMESPACE,       160).
-define(MAAPI_CD,                  161).
-define(MAAPI_PUSHD,               162).
-define(MAAPI_POPD,                163).
-define(MAAPI_EXISTS,              164).
-define(MAAPI_NUM_INSTANCES,       165).
-define(MAAPI_GET_ELEM,            166).
-define(MAAPI_GET_ELEM2,           167).
-define(MAAPI_GET_BULK_ELEM,       168).
-define(MAAPI_GET_NEXT,            169).
-define(MAAPI_GET_BULK_NEXT,       170).
-define(MAAPI_GET_ELEM_NO_DEFAULT, 171).
-define(MAAPI_SET_ELEM,            172).
-define(MAAPI_SET_ELEM2,           173).
-define(MAAPI_DELETE,              174).
-define(MAAPI_CREATE,              175).
-define(MAAPI_GET_OBJECT,          176).
-define(MAAPI_SET_OBJECT,          177).
-define(MAAPI_COPY,                178).
-define(MAAPI_COPY_TREE,           179).
-define(MAAPI_INSERT,              180).
-define(MAAPI_MOVE,                181).
-define(MAAPI_MOVE2,               182).
-define(MAAPI_DIFF_ITER,           183).
-define(MAAPI_DIFF_IKP_ITER,       184).
-define(MAAPI_SET_FLAGS,           185).
-define(MAAPI_GET_CASE,            186).
-define(MAAPI_DELETE_ALL,          187).
-define(MAAPI_GETCWD,              188).
-define(MAAPI_GET_ATTRS,           189).
-define(MAAPI_SET_ATTR,            190).
-define(MAAPI_MOVE_ORDERED,        191).
-define(MAAPI_MOVE_ORDERED2,       192).
-define(MAAPI_GET_OBJECTS,         193).
-define(MAAPI_GET_VALUES,          194).
-define(MAAPI_SET_VALUES,          195).
-define(MAAPI_COPY_PATH,           196).
-define(MAAPI_FIND_NEXT,           197).
-define(MAAPI_ITERATE,             198).
-define(MAAPI_SET_DELAYED_WHEN,    199).
-define(MAAPI_REVERT,              200).
-define(MAAPI_SET_LABEL,           201).
-define(MAAPI_SET_COMMENT,         202).

%% utility
-define(MAAPI_ATTACH,                220).
-define(MAAPI_DETACH,                221).
-define(MAAPI_AUTHENTICATE,          222).
-define(MAAPI_LOAD_ROLLBACK,         223).
-define(MAAPI_LIST_ROLLBACK,         224).
-define(MAAPI_SNMP_SEND_TRAP,        225).
-define(MAAPI_GET_RUNNING_DB_STATUS, 226).
-define(MAAPI_SET_RUNNING_DB_STATUS, 227).
-define(MAAPI_XPATH2HKP,             228).
-define(MAAPI_SET_READONLY,          229).
-define(MAAPI_HKP2IKP,               230).
-define(MAAPI_LOAD_HASH_DB,          231).
-define(MAAPI_LOAD_ALL_NS,           232).
-define(MAAPI_DISCONNECT_REMOTE,     233).
-define(MAAPI_SAVE_CONFIG,           234).
-define(MAAPI_SAVE_CONFIG_RESULT,    235).
-define(MAAPI_LOAD_CONFIG_FILE,      236).
-define(MAAPI_DO_DISPLAY,            237).
-define(MAAPI_ROLL_CONFIG,           238).
-define(MAAPI_ROLL_CONFIG_RESULT,    239).
-define(MAAPI_REQUEST_ACTION,        240).
-define(MAAPI_REQUEST_ACTION_TH,     241).
-define(MAAPI_INIT_UPGRADE,          242).
-define(MAAPI_PERFORM_UPGRADE,       243).
-define(MAAPI_COMMIT_UPGRADE,        244).
-define(MAAPI_ABORT_UPGRADE,         245).
-define(MAAPI_XPATH_EVAL,            246).
-define(MAAPI_XPATH_EVAL_EXPR,       247).
-define(MAAPI_GET_CRYPTO_KEYS,       248).
-define(MAAPI_REQUEST_ACTION_STR_TH, 249).
-define(MAAPI_LOAD_CONFIG,           250).
-define(MAAPI_LOAD_CONFIG_RESULT,    251).
-define(MAAPI_DEREF,                 252).
-define(MAAPI_LOAD_NS_LIST,          253).
-define(MAAPI_LOAD_CONFIG_CMDS,      254).
-define(MAAPI_QUERY_START,           255).
-define(MAAPI_QUERY_RESULT,          256).
-define(MAAPI_QUERY_RESET,           257).
-define(MAAPI_QUERY_STOP,            258).
-define(MAAPI_GET_STREAM_PROGRESS,   259).
-define(MAAPI_QUERY_RESULT_COUNT,    260).
-define(MAAPI_DISCONNECT_SOCKETS,    261).
-define(MAAPI_GET_SCHEMA_FILE_PATH,  262).
-define(MAAPI_VALIDATE_TOKEN,        263).


%% CLI interaction
-define(MAAPI_CLI_PROMPT,        300).
-define(MAAPI_CLI_WRITE,         301).
-define(MAAPI_CLI_READ_EOF,      302).
-define(MAAPI_CLI_CMD,           303).
-define(MAAPI_CLI_GET,           304).
-define(MAAPI_CLI_SET,           305).
-define(MAAPI_CLI_CMD_IO,        306).
-define(MAAPI_CLI_CMD_IO_RESULT, 307).
-define(MAAPI_CLI_CMD_TO_PATH,   308).
-define(MAAPI_CLI_DIFF_CMD,      309).
-define(MAAPI_CLI_PATH_CMD,      310).
-define(MAAPI_CLI_CMD_TO_PATH2,  311).
-define(MAAPI_CLI_ACCOUNTING,    312).

%% User messaging
-define(MAAPI_USER_MESSAGE,      350).
-define(MAAPI_SYS_MESSAGE,       351).
-define(MAAPI_PRIO_MESSAGE,      352).


%% NCS specific
-define(MAAPI_NCS_COMMIT_SAVE_TRANS,       360).
-define(MAAPI_NCS_APPLY_DIFF_SET,          361).
-define(MAAPI_NCS_TOOGLE_FASTMAP,          362).
-define(MAAPI_NCS_FUN_CALL,                363).
-define(MAAPI_NCS_SHARED_CREATE,           364).
-define(MAAPI_NCS_MOVE_PRIVATE_DATA,       365).
-define(MAAPI_NCS_APPLY_TEMPLATE,          366).
-define(MAAPI_NCS_TEMPLATES,               367).
-define(MAAPI_SHARED_SET_ELEM,             368).
-define(MAAPI_SHARED_SET_ELEM2,            369).
-define(MAAPI_NCS_TEMPLATE_VARIABLES,      370).
-define(MAAPI_SHARED_SET_VALUES,           371).
-define(MAAPI_NCS_UPDATE_PLAN_HISTORY,     372).
-define(MAAPI_NCS_COMMIT_SAVE_NO_DIFFSET,  373).
-define(MAAPI_NCS_WRITE_SERVICE_LOG_ENTRY, 374).

%% ConfD daemon a.o. control
-define(MAAPI_AAA_RELOAD,       400).
-define(MAAPI_START_PHASE,      401).
-define(MAAPI_WAIT_START,       402).
-define(MAAPI_RELOAD_CONFIG,    403).
-define(MAAPI_STOP,             404).
-define(MAAPI_AAA_RELOAD_PATH,  405).
-define(MAAPI_REOPEN_LOGS,      406).
-define(MAAPI_REBIND_LISTENER,  407).
-define(MAAPI_CLEAR_OPCACHE,    408).
-define(MAAPI_SNMPA_RELOAD,     409).


%% Per-(socket+trans) flags
-define(MAAPI_FLAG_HINT_BULK,         (1 bsl 0)).
-define(MAAPI_FLAG_NO_DEFAULTS,       (1 bsl 1)).
-define(MAAPI_FLAG_CONFIG_ONLY,       (1 bsl 2)).
-define(MAAPI_FLAG_HIDE_INACTIVE,     (1 bsl 3)).
-define(MAAPI_FLAG_NO_CONFIG_CACHE,   (1 bsl 4)).
-define(MAAPI_FLAG_CONFIG_CACHE_ONLY, (1 bsl 5)).
-define(MAAPI_FLAG_DELAYED_WHEN,      (1 bsl 6)).
-define(MAAPI_FLAG_RUN_SET_HOOKS,     (1 bsl 7)).

%% Bit Flags for executing commands
-define(MAAPI_CMD_NO_FULLPATH,         (1 bsl 0)).
-define(MAAPI_CMD_NO_HIDDEN,           (1 bsl 1)).
-define(MAAPI_CMD_NO_AAA,              (1 bsl 2)).

%% Bit Flags for saving/loading config
-define(MAAPI_CONFIG_XML,              (1 bsl 0)).
-define(MAAPI_CONFIG_J,                (1 bsl 1)).
-define(MAAPI_CONFIG_C,                (1 bsl 2)).
-define(MAAPI_CONFIG_WITH_DEFAULTS,    (1 bsl 3)).
-define(MAAPI_CONFIG_SHOW_DEFAULTS,    (1 bsl 4)).
-define(MAAPI_CONFIG_C_IOS,            (1 bsl 5)).
-define(MAAPI_CONFIG_MERGE,            (1 bsl 6)).
-define(MAAPI_CONFIG_WITH_OPER,        (1 bsl 7)).
-define(MAAPI_CONFIG_XPATH,            (1 bsl 8)).
-define(MAAPI_CONFIG_XML_PRETTY,       (1 bsl 9)).
-define(MAAPI_CONFIG_REPLACE,         (1 bsl 10)).
-define(MAAPI_CONFIG_HIDE_ALL,        (1 bsl 11)).
-define(MAAPI_CONFIG_UNHIDE_ALL,      (1 bsl 12)).
-define(MAAPI_CONFIG_AUTOCOMMIT,      (1 bsl 13)).
-define(MAAPI_CONFIG_CONTINUE_ON_ERROR, (1 bsl 14)).
-define(MAAPI_CONFIG_SUPPRESS_ERRORS, (1 bsl 15)).
-define(MAAPI_CONFIG_XML_LOAD_LAX,    (1 bsl 16)).
-define(MAAPI_CONFIG_JSON,            (1 bsl 17)).
-define(MAAPI_CONFIG_WITH_SERVICE_META, (1 bsl 18)).
-define(MAAPI_CONFIG_NO_PARENTS,      (1 bsl 19)).
-define(MAAPI_CONFIG_OPER_ONLY,       (1 bsl 20)).


%% Flags for MAAPI_INIT_UPGRADE
-define(MAAPI_UPGRADE_KILL_ON_TIMEOUT, (1 bsl 0)).

%% Flags for MAAPI_CLI_DIFF_CMD
-define(MAAPI_FLAG_EMIT_PARENTS,        (1 bsl 0)).
-define(MAAPI_FLAG_DELETE,              (1 bsl 1)).
-define(MAAPI_FLAG_NON_RECURSIVE,       (1 bsl 2)).

%% Flags for MAAPI_LOAD_NS_LIST (s/MAAPI/CONFD/ in C API)
-define(MAAPI_LOAD_SCHEMA_NODES,        (1 bsl 0)).
-define(MAAPI_LOAD_SCHEMA_TYPES,        (1 bsl 1)).

%% Listeners for MAAPI_REBIND_LISTENER (s/MAAPI/CONFD/ in C API)
-define(MAAPI_LISTENER_IPC,             (1 bsl 0)).
-define(MAAPI_LISTENER_NETCONF,         (1 bsl 1)).
-define(MAAPI_LISTENER_SNMP,            (1 bsl 2)).
-define(MAAPI_LISTENER_CLI,             (1 bsl 3)).
-define(MAAPI_LISTENER_WEBUI,           (1 bsl 4)).

%%- COMMON END

%% Flags for MAAPI_DIFF_ITER
-define(ITER_WANT_ATTR,                (1 bsl 2)).
-define(ITER_WANT_P_CONTAINER,         (1 bsl 8)).
