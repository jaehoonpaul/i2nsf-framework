%% generated from lib/build_tools/src/logdefs.txt
-module(econfd_logsyms).
-export([get_descr/1, get_logsymstr/1, get_logsym/1, array/0]).

-include("../include/econfd_logsyms.hrl").


%% @spec (LogSym::integer()) -> Descr::string()
get_descr(LogSym)     -> element(2, get_logsym(LogSym)).

%% @spec (LogSym::integer()) -> SymStr::string()
get_logsymstr(LogSym) -> element(1, get_logsym(LogSym)).

%% @spec () -> [{string(),string()}]
array() ->
    list_to_tuple(array(1, max_sym())).

array(Max, Max) -> [];
array(N, Max)   -> [get_logsym(N)|array(N+1, Max)].

%% @spec (LogSym::integer()) -> {LogSymStr::string(), Descr::string()}
get_logsym(?LOGSYM_CDB_BOOT_ERR) -> {"CDB_BOOT_ERR","CDB failed to start. Some grave error in the cdb data files prevented CDB from starting - a recovery from backup is necessary."};
get_logsym(?LOGSYM_BIND_ERR) -> {"BIND_ERR","ConfD failed to bind to one of the internally used listen sockets."};
get_logsym(?LOGSYM_MISSING_NS) -> {"MISSING_NS","While validating the consistency of the config - a required namespace was missing."};
get_logsym(?LOGSYM_MISSING_NS2) -> {"MISSING_NS2","While validating the consistency of the config - a required namespace was missing."};
get_logsym(?LOGSYM_BAD_NS_HASH) -> {"BAD_NS_HASH","Two namespaces have the same hash value. The namespace hashvalue MUST be unique.  You can pass the flag --nshash <value> to confdc when linking the .xso files to force another value for the namespace hash."};
get_logsym(?LOGSYM_NO_SUCH_IDENTITY) -> {"NO_SUCH_IDENTITY","The fxs file with the base identity is not loaded"};
get_logsym(?LOGSYM_NO_SUCH_NS) -> {"NO_SUCH_NS","A nonexistent namespace was referred to. Typically this means that a .fxs was missing from the loadPath."};
get_logsym(?LOGSYM_NO_SUCH_TYPE) -> {"NO_SUCH_TYPE","A nonexistent type was referred to from a ns. Typically this means that a bad version of an .fxs file was found in the loadPath."};
get_logsym(?LOGSYM_EXT_BIND_ERR) -> {"EXT_BIND_ERR","ConfD failed to bind to one of the externally visible listen sockets."};
get_logsym(?LOGSYM_ACCEPT_FDLIMIT) -> {"ACCEPT_FDLIMIT","ConfD failed to accept a connection due to reaching the process or system-wide file descriptor limit."};
get_logsym(?LOGSYM_ACCEPT_FATAL) -> {"ACCEPT_FATAL","ConfD encountered an OS-specific error indicating that networking support is unavailable."};
get_logsym(?LOGSYM_DUPLICATE_PREFIX) -> {"DUPLICATE_PREFIX","Duplicate prefix found."};
get_logsym(?LOGSYM_FILE_ERROR) -> {"FILE_ERROR","File error"};
get_logsym(?LOGSYM_DAEMON_DIED) -> {"DAEMON_DIED","An external database daemon closed its control socket."};
get_logsym(?LOGSYM_DAEMON_TIMEOUT) -> {"DAEMON_TIMEOUT","An external database daemon did not respond to a query."};
get_logsym(?LOGSYM_NO_CALLPOINT) -> {"NO_CALLPOINT","ConfD tried to populate an XML tree but no code had registered under the relevant callpoint."};
get_logsym(?LOGSYM_CDB_DB_LOST) -> {"CDB_DB_LOST","CDB found it's data schema file but not it's data file. CDB recovers by starting from an empty database."};
get_logsym(?LOGSYM_CDB_CONFIG_LOST) -> {"CDB_CONFIG_LOST","CDB found it's data files but no schema file. CDB recovers by starting from an empty database."};
get_logsym(?LOGSYM_CDB_UPGRADE_FAILED) -> {"CDB_UPGRADE_FAILED","Automatic CDB upgrade failed. This means that the data model has been changed in a non-supported way."};
get_logsym(?LOGSYM_CDB_INIT_LOAD) -> {"CDB_INIT_LOAD","CDB is processing an initialization file."};
get_logsym(?LOGSYM_CDB_OP_INIT) -> {"CDB_OP_INIT","The operational DB was deleted and re-initialized (because of upgrade or corrupt file)"};
get_logsym(?LOGSYM_CDB_CLIENT_TIMEOUT) -> {"CDB_CLIENT_TIMEOUT","A CDB client failed to answer within the timeout period. The client will be disconnected."};
get_logsym(?LOGSYM_INTERNAL_ERROR) -> {"INTERNAL_ERROR","A ConfD internal error - should be reported to support@tail-f.com."};
get_logsym(?LOGSYM_AAA_LOAD_FAIL) -> {"AAA_LOAD_FAIL","Failed to load the AAA data, it could be that an external db is misbehaving or AAA is mounted/populated badly"};
get_logsym(?LOGSYM_EXTAUTH_BAD_RET) -> {"EXTAUTH_BAD_RET","Authentication is external and the external program returned badly formatted data."};
get_logsym(?LOGSYM_BRIDGE_DIED) -> {"BRIDGE_DIED","ConfD is configured to start the confd_aaa_bridge and the C program died."};
get_logsym(?LOGSYM_PHASE0_STARTED) -> {"PHASE0_STARTED","ConfD has just started its start phase 0."};
get_logsym(?LOGSYM_PHASE1_STARTED) -> {"PHASE1_STARTED","ConfD has just started its start phase 1."};
get_logsym(?LOGSYM_STARTED) -> {"STARTED","ConfD has started."};
get_logsym(?LOGSYM_UPGRADE_INIT_STARTED) -> {"UPGRADE_INIT_STARTED","In-service upgrade initialization has started."};
get_logsym(?LOGSYM_UPGRADE_INIT_SUCCEEDED) -> {"UPGRADE_INIT_SUCCEEDED","In-service upgrade initialization succeeded."};
get_logsym(?LOGSYM_UPGRADE_PERFORMED) -> {"UPGRADE_PERFORMED","In-service upgrade has been performed (not committed yet)."};
get_logsym(?LOGSYM_UPGRADE_COMMITTED) -> {"UPGRADE_COMMITTED","In-service upgrade was committed."};
get_logsym(?LOGSYM_UPGRADE_ABORTED) -> {"UPGRADE_ABORTED","In-service upgrade was aborted."};
get_logsym(?LOGSYM_CONSULT_FILE) -> {"CONSULT_FILE","ConfD is reading its configuration file."};
get_logsym(?LOGSYM_STOPPING) -> {"STOPPING","ConfD is stopping (due to e.g. confd --stop)."};
get_logsym(?LOGSYM_RELOAD) -> {"RELOAD","Reload of daemon configuration has been initiated."};
get_logsym(?LOGSYM_BADCONFIG) -> {"BADCONFIG","confd.conf contained bad data."};
get_logsym(?LOGSYM_WRITE_STATE_FILE_FAILED) -> {"WRITE_STATE_FILE_FAILED","Writing of a state file failed"};
get_logsym(?LOGSYM_READ_STATE_FILE_FAILED) -> {"READ_STATE_FILE_FAILED","Reading of a state file failed"};
get_logsym(?LOGSYM_SSH_SUBSYS_ERR) -> {"SSH_SUBSYS_ERR","Typically errors where the client doesn't properly send the \"subsystem\" command."};
get_logsym(?LOGSYM_SESSION_LIMIT) -> {"SESSION_LIMIT","Session limit reached, rejected new session request."};
get_logsym(?LOGSYM_CONFIG_TRANSACTION_LIMIT) -> {"CONFIG_TRANSACTION_LIMIT","Configuration transaction limit reached, rejected new transaction request."};
get_logsym(?LOGSYM_ABORT_CAND_COMMIT) -> {"ABORT_CAND_COMMIT","Aborting candidate commit, request from user, reverting configuration."};
get_logsym(?LOGSYM_ABORT_CAND_COMMIT_TIMER) -> {"ABORT_CAND_COMMIT_TIMER","Candidate commit timer expired, reverting configuration."};
get_logsym(?LOGSYM_ABORT_CAND_COMMIT_TERM) -> {"ABORT_CAND_COMMIT_TERM","Candidate commit session terminated, reverting configuration."};
get_logsym(?LOGSYM_ROLLBACK_REMOVE) -> {"ROLLBACK_REMOVE","Found half created rollback0 file - removing and creating new."};
get_logsym(?LOGSYM_ROLLBACK_REPAIR) -> {"ROLLBACK_REPAIR","Found half created rollback0 file - repairing."};
get_logsym(?LOGSYM_ROLLBACK_FAIL_REPAIR) -> {"ROLLBACK_FAIL_REPAIR","Failed to repair rollback files."};
get_logsym(?LOGSYM_ROLLBACK_FAIL_CREATE) -> {"ROLLBACK_FAIL_CREATE","Error while creating rollback file."};
get_logsym(?LOGSYM_ROLLBACK_FAIL_RENAME) -> {"ROLLBACK_FAIL_RENAME","Failed to rename rollback file."};
get_logsym(?LOGSYM_NS_LOAD_ERR) -> {"NS_LOAD_ERR","System tried to process a loaded namespace and failed."};
get_logsym(?LOGSYM_NS_LOAD_ERR2) -> {"NS_LOAD_ERR2","System tried to process a loaded namespace and failed."};
get_logsym(?LOGSYM_FILE_LOAD_ERR) -> {"FILE_LOAD_ERR","System tried to load a file in its load path and failed."};
get_logsym(?LOGSYM_FILE_LOADING) -> {"FILE_LOADING","System starts to load a file."};
get_logsym(?LOGSYM_SKIP_FILE_LOADING) -> {"SKIP_FILE_LOADING","System skips a file."};
get_logsym(?LOGSYM_FILE_LOAD) -> {"FILE_LOAD","System loaded a file."};
get_logsym(?LOGSYM_LISTENER_INFO) -> {"LISTENER_INFO","ConfD starts or stops to listen for incoming connections."};
get_logsym(?LOGSYM_NETCONF_HDR_ERR) -> {"NETCONF_HDR_ERR","The cleartext header indicating user and groups was badly formatted."};
get_logsym(?LOGSYM_LIB_BAD_VSN) -> {"LIB_BAD_VSN","An application connecting to ConfD used a library version that doesn't match the ConfD version (e.g. old version of the client library)."};
get_logsym(?LOGSYM_LIB_BAD_SIZES) -> {"LIB_BAD_SIZES","An application connecting to ConfD used a library version that can't handle the depth and number of keys used by the data model."};
get_logsym(?LOGSYM_LIB_NO_ACCESS) -> {"LIB_NO_ACCESS","Access check failure occurred when an application connected to ConfD."};
get_logsym(?LOGSYM_SNMP_NOT_A_TRAP) -> {"SNMP_NOT_A_TRAP","An UDP package was received on the trap receiving port, but it's not an SNMP trap."};
get_logsym(?LOGSYM_SNMP_TRAP_V1) -> {"SNMP_TRAP_V1","An SNMP v1 trap was received on the trap receiving port, but forwarding v1 traps is not supported."};
get_logsym(?LOGSYM_SNMP_TRAP_NOT_FORWARDED) -> {"SNMP_TRAP_NOT_FORWARDED","An SNMP trap was to be forwarded, but couldn't be."};
get_logsym(?LOGSYM_SNMP_TRAP_UNKNOWN_SENDER) -> {"SNMP_TRAP_UNKNOWN_SENDER","An SNMP trap was to be forwarded, but the sender was not listed in confd.conf."};
get_logsym(?LOGSYM_SNMP_TRAP_OPEN_PORT) -> {"SNMP_TRAP_OPEN_PORT","The port for listening to SNMP traps could not be opened."};
get_logsym(?LOGSYM_SNMP_TRAP_NOT_RECOGNIZED) -> {"SNMP_TRAP_NOT_RECOGNIZED","An SNMP trap was received on the trap receiving port, but its definition is not known"};
get_logsym(?LOGSYM_XPATH_EVAL_ERROR1) -> {"XPATH_EVAL_ERROR1","An error occurred while evaluating an XPath expression."};
get_logsym(?LOGSYM_XPATH_EVAL_ERROR2) -> {"XPATH_EVAL_ERROR2","An error occurred while evaluating an XPath expression."};
get_logsym(?LOGSYM_CANDIDATE_BAD_FILE_FORMAT) -> {"CANDIDATE_BAD_FILE_FORMAT","The candidate database file has a bad format. The candidate database is reset to the empty database."};
get_logsym(?LOGSYM_CANDIDATE_CORRUPT_FILE) -> {"CANDIDATE_CORRUPT_FILE","The candidate database file is corrupt and cannot be read. The candidate database is reset to the empty database."};
get_logsym(?LOGSYM_MISSING_DES3CBC_SETTINGS) -> {"MISSING_DES3CBC_SETTINGS","DES3CBC keys were not found in confd.conf"};
get_logsym(?LOGSYM_MISSING_AESCFB128_SETTINGS) -> {"MISSING_AESCFB128_SETTINGS","AESCFB128 keys were not found in confd.conf"};
get_logsym(?LOGSYM_SNMP_MIB_LOADING) -> {"SNMP_MIB_LOADING","SNMP Agent loading a MIB file"};
get_logsym(?LOGSYM_SNMP_CANT_LOAD_MIB) -> {"SNMP_CANT_LOAD_MIB","The SNMP Agent failed to load a MIB file"};
get_logsym(?LOGSYM_SNMP_WRITE_STATE_FILE_FAILED) -> {"SNMP_WRITE_STATE_FILE_FAILED","Write SNMP agent state file failed"};
get_logsym(?LOGSYM_SNMP_READ_STATE_FILE_FAILED) -> {"SNMP_READ_STATE_FILE_FAILED","Read SNMP agent state file failed"};
get_logsym(?LOGSYM_SNMP_REQUIRES_CDB) -> {"SNMP_REQUIRES_CDB",   "The SNMP agent requires CDB to be enabled in order to be started."};
get_logsym(?LOGSYM_FXS_MISMATCH) -> {"FXS_MISMATCH","A slave connected to a master where the fxs files are different"};
get_logsym(?LOGSYM_TOKEN_MISMATCH) -> {"TOKEN_MISMATCH","A slave connected to a master with a bad auth token"};
get_logsym(?LOGSYM_HA_SLAVE_KILLED) -> {"HA_SLAVE_KILLED","A slave node didn't produce its ticks"};
get_logsym(?LOGSYM_HA_DUPLICATE_NODEID) -> {"HA_DUPLICATE_NODEID","A slave arrived with a node id which already exists"};
get_logsym(?LOGSYM_HA_FAILED_CONNECT) -> {"HA_FAILED_CONNECT","An attempted library become slave call failed because the slave couldn't connect to the master"};
get_logsym(?LOGSYM_HA_BAD_VSN) -> {"HA_BAD_VSN","A slave connected to a master with an incompatible HA protocol version"};
get_logsym(?LOGSYM_NETCONF) -> {"NETCONF","NETCONF traffic log message"};
get_logsym(?LOGSYM_DEVEL_WEBUI) -> {"DEVEL_WEBUI","Developer webui log message"};
get_logsym(?LOGSYM_DEVEL_AAA) -> {"DEVEL_AAA","Developer aaa log message"};
get_logsym(?LOGSYM_DEVEL_CAPI) -> {"DEVEL_CAPI","Developer C api log message"};
get_logsym(?LOGSYM_DEVEL_CDB) -> {"DEVEL_CDB","Developer CDB log message"};
get_logsym(?LOGSYM_DEVEL_CONFD) -> {"DEVEL_CONFD","Developer ConfD log message"};
get_logsym(?LOGSYM_DEVEL_SNMPGW) -> {"DEVEL_SNMPGW","Developer snmp GW log message"};
get_logsym(?LOGSYM_DEVEL_SNMPA) -> {"DEVEL_SNMPA","Developer snmp agent log message"};
get_logsym(?LOGSYM_NOTIFICATION_REPLAY_STORE_FAILURE) -> {"NOTIFICATION_REPLAY_STORE_FAILURE","A failure occurred in the builtin notification replay store"};
get_logsym(?LOGSYM_EVENT_SOCKET_TIMEOUT) -> {"EVENT_SOCKET_TIMEOUT","An event notification subscriber did not reply within the configured timeout period"};
get_logsym(?LOGSYM_EVENT_SOCKET_WRITE_BLOCK) -> {"EVENT_SOCKET_WRITE_BLOCK",       "Write on an event socket blocked for too long time"};
get_logsym(?LOGSYM_CLI_CMD) -> {"CLI_CMD","User executed a CLI command."};
get_logsym(?LOGSYM_CLI_DENIED) -> {"CLI_DENIED","User was denied to execute a CLI command due to permissions."};
get_logsym(?LOGSYM_LOCAL_AUTH_SUCCESS) -> {"LOCAL_AUTH_SUCCESS","A locally authenticated user logged in."};
get_logsym(?LOGSYM_LOCAL_AUTH_FAIL) -> {"LOCAL_AUTH_FAIL","Authentication for a locally configured user failed."};
get_logsym(?LOGSYM_PAM_AUTH_SUCCESS) -> {"PAM_AUTH_SUCCESS","A PAM authenticated user logged in."};
get_logsym(?LOGSYM_PAM_AUTH_FAIL) -> {"PAM_AUTH_FAIL","A user failed to authenticate through PAM."};
get_logsym(?LOGSYM_EXT_AUTH_SUCCESS) -> {"EXT_AUTH_SUCCESS","An externally authenticated user logged in."};
get_logsym(?LOGSYM_EXT_AUTH_FAIL) -> {"EXT_AUTH_FAIL","External authentication failed for a user."};
get_logsym(?LOGSYM_AUTH_LOGIN_SUCCESS) -> {"AUTH_LOGIN_SUCCESS","A user logged into ConfD."};
get_logsym(?LOGSYM_AUTH_LOGIN_FAIL) -> {"AUTH_LOGIN_FAIL","A user failed to log in to ConfD."};
get_logsym(?LOGSYM_AUTH_LOGOUT) -> {"AUTH_LOGOUT","A user was logged out from ConfD."};
get_logsym(?LOGSYM_GROUP_ASSIGN) -> {"GROUP_ASSIGN","A user was assigned to a set of groups."};
get_logsym(?LOGSYM_GROUP_NO_ASSIGN) -> {"GROUP_NO_ASSIGN","A user was logged in but wasn't assigned to any groups at all."};
get_logsym(?LOGSYM_MAAPI_LOGOUT) -> {"MAAPI_LOGOUT","A maapi user was logged out."};
get_logsym(?LOGSYM_NOAAA_CLI_LOGIN) -> {"NOAAA_CLI_LOGIN","A user used the --noaaa flag to confd_cli"};
get_logsym(?LOGSYM_WEB_CMD) -> {"WEB_CMD","User executed a Web UI command."};
get_logsym(?LOGSYM_WEB_ACTION) -> {"WEB_ACTION","User executed a Web UI action."};
get_logsym(?LOGSYM_WEB_COMMIT) -> {"WEB_COMMIT","User performed Web UI commit."};
get_logsym(?LOGSYM_SNMP_AUTHENTICATION_FAILED) -> {"SNMP_AUTHENTICATION_FAILED","An SNMP authentication failed."};
get_logsym(?LOGSYM_LOGIN_REJECTED) -> {"LOGIN_REJECTED","Authentication for a user was rejected by application callback."};
get_logsym(?LOGSYM_COMMIT_INFO) -> {"COMMIT_INFO","Information about configuration changes committed to the running data store."};
get_logsym(?LOGSYM_CLI_CMD_DONE) -> {"CLI_CMD_DONE","CLI command finished successfully."};
get_logsym(?LOGSYM_CLI_CMD_ABORTED) -> {"CLI_CMD_ABORTED","CLI command aborted."};
get_logsym(?LOGSYM_DEVEL_SLS) -> {"DEVEL_SLS","Developer smartlicensing api log message"};
get_logsym(?LOGSYM_JSONRPC_REQUEST) -> {"JSONRPC_REQUEST","JSON-RPC method requested."};
get_logsym(?LOGSYM_DEVEL_ECONFD) -> {"DEVEL_ECONFD","Developer econfd api log message"};
get_logsym(?LOGSYM_CDB_FATAL_ERROR) -> {"CDB_FATAL_ERROR","CDB encounterad an unrecoverable error"};
get_logsym(?LOGSYM_LOGGING_STARTED) -> {"LOGGING_STARTED","Logging subsystem started"};
get_logsym(?LOGSYM_LOGGING_SHUTDOWN) -> {"LOGGING_SHUTDOWN","Logging subsystem terminating"};
get_logsym(?LOGSYM_REOPEN_LOGS) -> {"REOPEN_LOGS","Logging subsystem, reopening log files"};
get_logsym(?LOGSYM_OPEN_LOGFILE) -> {"OPEN_LOGFILE","Indicate target file for certain type of logging"};
get_logsym(?LOGSYM_LOGGING_STARTED_TO) -> {"LOGGING_STARTED_TO","Write logs for a subsystem to a specific file"};
get_logsym(?LOGSYM_LOGGING_DEST_CHANGED) -> {"LOGGING_DEST_CHANGED","The target logfile will change to another file"};
get_logsym(?LOGSYM_LOGGING_STATUS_CHANGED) -> {"LOGGING_STATUS_CHANGED","Notify a change of logging status (enabled/disabled) for a subsystem"};
get_logsym(?LOGSYM_ERRLOG_SIZE_CHANGED) -> {"ERRLOG_SIZE_CHANGED","Notify change of log size for error log"};
get_logsym(?LOGSYM_CGI_REQUEST) -> {"CGI_REQUEST","CGI script requested."};
get_logsym(?LOGSYM_MMAP_SCHEMA_FAIL) -> {"MMAP_SCHEMA_FAIL","Failed to setup the shared memory schema"};
get_logsym(?LOGSYM_KICKER_MISSING_SCHEMA) -> {"KICKER_MISSING_SCHEMA","Failed to load kicker schema"};
get_logsym(?LOGSYM_JSONRPC_REQUEST_IDLE_TIMEOUT) -> {"JSONRPC_REQUEST_IDLE_TIMEOUT","JSON-RPC idle timeout."};
get_logsym(?LOGSYM_JSONRPC_REQUEST_ABSOLUTE_TIMEOUT) -> {"JSONRPC_REQUEST_ABSOLUTE_TIMEOUT","JSON-RPC absolute timeout."};
get_logsym(?LOGSYM_BAD_DEPENDENCY) -> {"BAD_DEPENDENCY","A dependency was not found"};
get_logsym(?LOGSYM_ABORT_CAND_COMMIT_REBOOT) -> {"ABORT_CAND_COMMIT_REBOOT","ConfD restarted while having a ongoing candidate commit timer, reverting configuration."};
get_logsym(?LOGSYM_CAND_COMMIT_ROLLBACK_FAILURE) -> {"CAND_COMMIT_ROLLBACK_FAILURE","Failed to rollback candidate commit"};
get_logsym(Sym) -> erlang:error({no_such_logsym, Sym}).

max_sym() -> 140.

