%% File generated from lib/build_tools/src/logdefs.txt
%% 

%% generated from lib/build_tools/src/logdefs.txt


%% ----------------------------------------------------------------------
%% All of these can be produced by ConfD while starting, they all
%% are fatal and ConfD will stop. They are syslog messages.
%% ----------------------------------------------------------------------

%% 
%%
%% "CDB failed to start. Some grave error in the cdb data files prevented CDB from starting - a recovery from backup is necessary."
-define( LOGSYM_CDB_BOOT_ERR, 1).
%% 
%%
%% "ConfD failed to bind to one of the internally used listen sockets."
-define( LOGSYM_BIND_ERR, 2).
%% 
%%
%% "While validating the consistency of the config - a required namespace was missing."
-define( LOGSYM_MISSING_NS, 3).
%% 
%%
%% "While validating the consistency of the config - a required namespace was missing."
-define( LOGSYM_MISSING_NS2, 4).
%% 
%%
%% "Two namespaces have the same hash value. The namespace hashvalue MUST be unique.  You can pass the flag --nshash <value> to confdc when linking the .xso files to force another value for the namespace hash."
-define( LOGSYM_BAD_NS_HASH, 5).
%% 
%%
%% "The fxs file with the base identity is not loaded"
-define( LOGSYM_NO_SUCH_IDENTITY, 6).
%% 
%%
%% "A nonexistent namespace was referred to. Typically this means that a .fxs was missing from the loadPath."
-define( LOGSYM_NO_SUCH_NS, 7).
%% 
%%
%% "A nonexistent type was referred to from a ns. Typically this means that a bad version of an .fxs file was found in the loadPath."
-define( LOGSYM_NO_SUCH_TYPE, 8).
%% 
%%
%% "ConfD failed to bind to one of the externally visible listen sockets."
-define( LOGSYM_EXT_BIND_ERR, 9).
%% 
%%
%% "ConfD failed to accept a connection due to reaching the process or system-wide file descriptor limit."
-define( LOGSYM_ACCEPT_FDLIMIT, 10).
%% 
%%
%% "ConfD encountered an OS-specific error indicating that networking support is unavailable."
-define( LOGSYM_ACCEPT_FATAL, 11).
%% 
%%
%% "Duplicate prefix found."
-define( LOGSYM_DUPLICATE_PREFIX, 12).
%% 
%%
%% "File error"
-define( LOGSYM_FILE_ERROR, 13).

%% ----------------------------------------------------------------------
%% All of these are regular syslog messages.
%% ----------------------------------------------------------------------

%% 
%%
%% "An external database daemon closed its control socket."
-define( LOGSYM_DAEMON_DIED, 14).
%% 
%%
%% "An external database daemon did not respond to a query."
-define( LOGSYM_DAEMON_TIMEOUT, 15).
%% 
%%
%% "ConfD tried to populate an XML tree but no code had registered under the relevant callpoint."
-define( LOGSYM_NO_CALLPOINT, 16).
%% 
%%
%% "CDB found it's data schema file but not it's data file. CDB recovers by starting from an empty database."
-define( LOGSYM_CDB_DB_LOST, 17).
%% 
%%
%% "CDB found it's data files but no schema file. CDB recovers by starting from an empty database."
-define( LOGSYM_CDB_CONFIG_LOST, 18).
%% 
%%
%% "Automatic CDB upgrade failed. This means that the data model has been changed in a non-supported way."
-define( LOGSYM_CDB_UPGRADE_FAILED, 19).
%% 
%%
%% "CDB is processing an initialization file."
-define( LOGSYM_CDB_INIT_LOAD, 20).
%% 
%%
%% "The operational DB was deleted and re-initialized (because of upgrade or corrupt file)"
-define( LOGSYM_CDB_OP_INIT, 21).
%% 
%%
%% "A CDB client failed to answer within the timeout period. The client will be disconnected."
-define( LOGSYM_CDB_CLIENT_TIMEOUT, 22).
%% 
%%
%% "A ConfD internal error - should be reported to support@tail-f.com."
-define( LOGSYM_INTERNAL_ERROR, 23).
%% 
%%
%% "Failed to load the AAA data, it could be that an external db is misbehaving or AAA is mounted/populated badly"
-define( LOGSYM_AAA_LOAD_FAIL, 24).
%% 
%%
%% "Authentication is external and the external program returned badly formatted data."
-define( LOGSYM_EXTAUTH_BAD_RET, 25).
%% 
%%
%% "ConfD is configured to start the confd_aaa_bridge and the C program died."
-define( LOGSYM_BRIDGE_DIED, 26).
%% 
%%
%% "ConfD has just started its start phase 0."
-define( LOGSYM_PHASE0_STARTED, 27).
%% 
%%
%% "ConfD has just started its start phase 1."
-define( LOGSYM_PHASE1_STARTED, 28).
%% 
%%
%% "ConfD has started."
-define( LOGSYM_STARTED, 29).
%% 
%%
%% "In-service upgrade initialization has started."
-define( LOGSYM_UPGRADE_INIT_STARTED, 30).
%% 
%%
%% "In-service upgrade initialization succeeded."
-define( LOGSYM_UPGRADE_INIT_SUCCEEDED, 31).
%% 
%%
%% "In-service upgrade has been performed (not committed yet)."
-define( LOGSYM_UPGRADE_PERFORMED, 32).
%% 
%%
%% "In-service upgrade was committed."
-define( LOGSYM_UPGRADE_COMMITTED, 33).
%% 
%%
%% "In-service upgrade was aborted."
-define( LOGSYM_UPGRADE_ABORTED, 34).
%% 
%%
%% "ConfD is reading its configuration file."
-define( LOGSYM_CONSULT_FILE, 35).
%% 
%%
%% "ConfD is stopping (due to e.g. confd --stop)."
-define( LOGSYM_STOPPING, 36).
%% 
%%
%% "Reload of daemon configuration has been initiated."
-define( LOGSYM_RELOAD, 37).
%% 
%%
%% "confd.conf contained bad data."
-define( LOGSYM_BADCONFIG, 38).
%% 
%%
%% "Writing of a state file failed"
-define( LOGSYM_WRITE_STATE_FILE_FAILED, 39).
%% 
%%
%% "Reading of a state file failed"
-define( LOGSYM_READ_STATE_FILE_FAILED, 40).
%% 
%%
%% "Typically errors where the client doesn't properly send the \"subsystem\" command."
-define( LOGSYM_SSH_SUBSYS_ERR, 41).
%% 
%%
%% "Session limit reached, rejected new session request."
-define( LOGSYM_SESSION_LIMIT, 42).
%% 
%%
%% "Configuration transaction limit reached, rejected new transaction request."
-define( LOGSYM_CONFIG_TRANSACTION_LIMIT, 43).
%% 
%%
%% "Aborting candidate commit, request from user, reverting configuration."
-define( LOGSYM_ABORT_CAND_COMMIT, 44).
%% 
%%
%% "Candidate commit timer expired, reverting configuration."
-define( LOGSYM_ABORT_CAND_COMMIT_TIMER, 45).
%% 
%%
%% "Candidate commit session terminated, reverting configuration."
-define( LOGSYM_ABORT_CAND_COMMIT_TERM, 46).
%% 
%%
%% "Found half created rollback0 file - removing and creating new."
-define( LOGSYM_ROLLBACK_REMOVE, 47).
%% 
%%
%% "Found half created rollback0 file - repairing."
-define( LOGSYM_ROLLBACK_REPAIR, 48).
%% 
%%
%% "Failed to repair rollback files."
-define( LOGSYM_ROLLBACK_FAIL_REPAIR, 49).
%% 
%%
%% "Error while creating rollback file."
-define( LOGSYM_ROLLBACK_FAIL_CREATE, 50).
%% 
%%
%% "Failed to rename rollback file."
-define( LOGSYM_ROLLBACK_FAIL_RENAME, 51).
%% 
%%
%% "System tried to process a loaded namespace and failed."
-define( LOGSYM_NS_LOAD_ERR, 52).
%% 
%%
%% "System tried to process a loaded namespace and failed."
-define( LOGSYM_NS_LOAD_ERR2, 53).
%% 
%%
%% "System tried to load a file in its load path and failed."
-define( LOGSYM_FILE_LOAD_ERR, 54).
%% 
%%
%% "System starts to load a file."
-define( LOGSYM_FILE_LOADING, 55).
%% 
%%
%% "System skips a file."
-define( LOGSYM_SKIP_FILE_LOADING, 56).
%% 
%%
%% "System loaded a file."
-define( LOGSYM_FILE_LOAD, 57).
%% 
%%
%% "ConfD starts or stops to listen for incoming connections."
-define( LOGSYM_LISTENER_INFO, 58).
%% 
%%
%% "The cleartext header indicating user and groups was badly formatted."
-define( LOGSYM_NETCONF_HDR_ERR, 59).
%% 
%%
%% "An application connecting to ConfD used a library version that doesn't match the ConfD version (e.g. old version of the client library)."
-define( LOGSYM_LIB_BAD_VSN, 60).
%% 
%%
%% "An application connecting to ConfD used a library version that can't handle the depth and number of keys used by the data model."
-define( LOGSYM_LIB_BAD_SIZES, 61).
%% 
%%
%% "Access check failure occurred when an application connected to ConfD."
-define( LOGSYM_LIB_NO_ACCESS, 62).
%% 
%%
%% "An UDP package was received on the trap receiving port, but it's not an SNMP trap."
-define( LOGSYM_SNMP_NOT_A_TRAP, 63).
%% 
%%
%% "An SNMP v1 trap was received on the trap receiving port, but forwarding v1 traps is not supported."
-define( LOGSYM_SNMP_TRAP_V1, 64).
%% 
%%
%% "An SNMP trap was to be forwarded, but couldn't be."
-define( LOGSYM_SNMP_TRAP_NOT_FORWARDED, 65).
%% 
%%
%% "An SNMP trap was to be forwarded, but the sender was not listed in confd.conf."
-define( LOGSYM_SNMP_TRAP_UNKNOWN_SENDER, 66).
%% 
%%
%% "The port for listening to SNMP traps could not be opened."
-define( LOGSYM_SNMP_TRAP_OPEN_PORT, 67).
%% 
%%
%% "An SNMP trap was received on the trap receiving port, but its definition is not known"
-define( LOGSYM_SNMP_TRAP_NOT_RECOGNIZED, 68).
%% 
%%
%% "An error occurred while evaluating an XPath expression."
-define( LOGSYM_XPATH_EVAL_ERROR1, 69).
%% 
%%
%% "An error occurred while evaluating an XPath expression."
-define( LOGSYM_XPATH_EVAL_ERROR2, 70).
%% 
%%
%% "The candidate database file has a bad format. The candidate database is reset to the empty database."
-define( LOGSYM_CANDIDATE_BAD_FILE_FORMAT, 71).
%% 
%%
%% "The candidate database file is corrupt and cannot be read. The candidate database is reset to the empty database."
-define( LOGSYM_CANDIDATE_CORRUPT_FILE, 72).
%% 
%%
%% "DES3CBC keys were not found in confd.conf"
-define( LOGSYM_MISSING_DES3CBC_SETTINGS, 73).
%% 
%%
%% "AESCFB128 keys were not found in confd.conf"
-define( LOGSYM_MISSING_AESCFB128_SETTINGS, 74).
%% 
%%
%% "SNMP Agent loading a MIB file"
-define( LOGSYM_SNMP_MIB_LOADING, 75).
%% 
%%
%% "The SNMP Agent failed to load a MIB file"
-define( LOGSYM_SNMP_CANT_LOAD_MIB, 76).
%% 
%%
%% "Write SNMP agent state file failed"
-define( LOGSYM_SNMP_WRITE_STATE_FILE_FAILED, 77).
%% 
%%
%% "Read SNMP agent state file failed"
-define( LOGSYM_SNMP_READ_STATE_FILE_FAILED, 78).
%% 
%%
%%    "The SNMP agent requires CDB to be enabled in order to be started."
-define( LOGSYM_SNMP_REQUIRES_CDB, 79).
%% 
%%
%% "A slave connected to a master where the fxs files are different"
-define( LOGSYM_FXS_MISMATCH, 80).
%% 
%%
%% "A slave connected to a master with a bad auth token"
-define( LOGSYM_TOKEN_MISMATCH, 81).
%% 
%%
%% "A slave node didn't produce its ticks"
-define( LOGSYM_HA_SLAVE_KILLED, 82).
%% 
%%
%% "A slave arrived with a node id which already exists"
-define( LOGSYM_HA_DUPLICATE_NODEID, 83).
%% 
%%
%% "An attempted library become slave call failed because the slave couldn't connect to the master"
-define( LOGSYM_HA_FAILED_CONNECT, 84).
%% 
%%
%% "A slave connected to a master with an incompatible HA protocol version"
-define( LOGSYM_HA_BAD_VSN, 85).
%% 
%%
%% "NETCONF traffic log message"
-define( LOGSYM_NETCONF, 86).
%% 
%%
%% "Developer webui log message"
-define( LOGSYM_DEVEL_WEBUI, 87).
%% 
%%
%% "Developer aaa log message"
-define( LOGSYM_DEVEL_AAA, 88).
%% 
%%
%% "Developer C api log message"
-define( LOGSYM_DEVEL_CAPI, 89).
%% 
%%
%% "Developer CDB log message"
-define( LOGSYM_DEVEL_CDB, 90).
%% 
%%
%% "Developer ConfD log message"
-define( LOGSYM_DEVEL_CONFD, 91).
%% 
%%
%% "Developer snmp GW log message"
-define( LOGSYM_DEVEL_SNMPGW, 92).
%% 
%%
%% "Developer snmp agent log message"
-define( LOGSYM_DEVEL_SNMPA, 93).
%% 
%%
%% "A failure occurred in the builtin notification replay store"
-define( LOGSYM_NOTIFICATION_REPLAY_STORE_FAILURE, 94).
%% 
%%
%% "An event notification subscriber did not reply within the configured timeout period"
-define( LOGSYM_EVENT_SOCKET_TIMEOUT, 95).
%% 
%%
%%        "Write on an event socket blocked for too long time"
-define( LOGSYM_EVENT_SOCKET_WRITE_BLOCK, 96).

%% ----------------------------------------------------------------------
%% Here come all the audit messages.
%% ----------------------------------------------------------------------

%% 
%%
%% "User executed a CLI command."
-define( LOGSYM_CLI_CMD, 97).
%% 
%%
%% "User was denied to execute a CLI command due to permissions."
-define( LOGSYM_CLI_DENIED, 98).
%% 
%%
%% "A locally authenticated user logged in."
-define( LOGSYM_LOCAL_AUTH_SUCCESS, 99).
%% 
%%
%% "Authentication for a locally configured user failed."
-define( LOGSYM_LOCAL_AUTH_FAIL, 100).
%% 
%%
%% "A PAM authenticated user logged in."
-define( LOGSYM_PAM_AUTH_SUCCESS, 101).
%% 
%%
%% "A user failed to authenticate through PAM."
-define( LOGSYM_PAM_AUTH_FAIL, 102).
%% 
%%
%% "An externally authenticated user logged in."
-define( LOGSYM_EXT_AUTH_SUCCESS, 103).
%% 
%%
%% "External authentication failed for a user."
-define( LOGSYM_EXT_AUTH_FAIL, 104).
%% 
%%
%% "A user logged into ConfD."
-define( LOGSYM_AUTH_LOGIN_SUCCESS, 105).
%% 
%%
%% "A user failed to log in to ConfD."
-define( LOGSYM_AUTH_LOGIN_FAIL, 106).
%% 
%%
%% "A user was logged out from ConfD."
-define( LOGSYM_AUTH_LOGOUT, 107).
%% 
%%
%% "A user was assigned to a set of groups."
-define( LOGSYM_GROUP_ASSIGN, 108).
%% 
%%
%% "A user was logged in but wasn't assigned to any groups at all."
-define( LOGSYM_GROUP_NO_ASSIGN, 109).
%% 
%%
%% "A maapi user was logged out."
-define( LOGSYM_MAAPI_LOGOUT, 110).
%% 
%%
%% "A user used the --noaaa flag to confd_cli"
-define( LOGSYM_NOAAA_CLI_LOGIN, 111).
%% 
%%
%% "User executed a Web UI command."
-define( LOGSYM_WEB_CMD, 112).
%% 
%%
%% "User executed a Web UI action."
-define( LOGSYM_WEB_ACTION, 113).
%% 
%%
%% "User performed Web UI commit."
-define( LOGSYM_WEB_COMMIT, 114).
%% 
%%
%% "An SNMP authentication failed."
-define( LOGSYM_SNMP_AUTHENTICATION_FAILED, 115).
%% 
%%
%% "Authentication for a user was rejected by application callback."
-define( LOGSYM_LOGIN_REJECTED, 116).
%% 
%%
%% "Information about configuration changes committed to the running data store."
-define( LOGSYM_COMMIT_INFO, 117).
%% 
%%
%% "CLI command finished successfully."
-define( LOGSYM_CLI_CMD_DONE, 118).
%% 
%%
%% "CLI command aborted."
-define( LOGSYM_CLI_CMD_ABORTED, 119).
%% 
%%
%% "Developer smartlicensing api log message"
-define( LOGSYM_DEVEL_SLS, 120).
%% 
%%
%% "JSON-RPC method requested."
-define( LOGSYM_JSONRPC_REQUEST, 121).
%% 
%%
%% "Developer econfd api log message"
-define( LOGSYM_DEVEL_ECONFD, 122).
%% 
%%
%% "CDB encounterad an unrecoverable error"
-define( LOGSYM_CDB_FATAL_ERROR, 123).
%% 
%%
%% "Logging subsystem started"
-define( LOGSYM_LOGGING_STARTED, 124).
%% 
%%
%% "Logging subsystem terminating"
-define( LOGSYM_LOGGING_SHUTDOWN, 125).
%% 
%%
%% "Logging subsystem, reopening log files"
-define( LOGSYM_REOPEN_LOGS, 126).
%% 
%%
%% "Indicate target file for certain type of logging"
-define( LOGSYM_OPEN_LOGFILE, 127).
%% 
%%
%% "Write logs for a subsystem to a specific file"
-define( LOGSYM_LOGGING_STARTED_TO, 128).
%% 
%%
%% "The target logfile will change to another file"
-define( LOGSYM_LOGGING_DEST_CHANGED, 129).
%% 
%%
%% "Notify a change of logging status (enabled/disabled) for a subsystem"
-define( LOGSYM_LOGGING_STATUS_CHANGED, 130).
%% 
%%
%% "Notify change of log size for error log"
-define( LOGSYM_ERRLOG_SIZE_CHANGED, 131).
%% 
%%
%% "CGI script requested."
-define( LOGSYM_CGI_REQUEST, 132).
%% 
%%
%% "Failed to setup the shared memory schema"
-define( LOGSYM_MMAP_SCHEMA_FAIL, 133).
%% 
%%
%% "Failed to load kicker schema"
-define( LOGSYM_KICKER_MISSING_SCHEMA, 134).
%% 
%%
%% "JSON-RPC idle timeout."
-define( LOGSYM_JSONRPC_REQUEST_IDLE_TIMEOUT, 135).
%% 
%%
%% "JSON-RPC absolute timeout."
-define( LOGSYM_JSONRPC_REQUEST_ABSOLUTE_TIMEOUT, 136).
%% 
%%
%% "A dependency was not found"
-define( LOGSYM_BAD_DEPENDENCY, 137).
%% 
%%
%% "ConfD restarted while having a ongoing candidate commit timer, reverting configuration."
-define( LOGSYM_ABORT_CAND_COMMIT_REBOOT, 138).
%% 
%%
%% "Failed to rollback candidate commit"
-define( LOGSYM_CAND_COMMIT_ROLLBACK_FAILURE, 139).
