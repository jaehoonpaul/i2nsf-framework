%%%-------------------------------------------------------------------
%%% @copyright 2006 Tail-F Systems AB
%%% @version {$Id: confd.hrl 6279 2006-10-30 13:44:03Z mbj $}
%%%-------------------------------------------------------------------

-ifndef(CONFD_HRL).
-define(CONFD_HRL, true).

-define(CONFD_PORT, 4565).
-define(NCS_PORT,   4569).

%% callback fun() records
%% transaction statemachine callbacks
-record(confd_trans_cbs, {
          init,         %% econfd:cb_ctx()
          trans_lock,   %% econfd:cb_trans_lock()
          trans_unlock, %% econfd:cb_ctx()
          write_start,  %% econfd:cb_write()
          prepare,      %% econfd:cb_write()
          abort,        %% econfd:cb_ctx()
          commit,       %% econfd:cb_ctx()
          finish}).     %% econfd:cb_ok()


%% validation phase callbacks
-record(confd_trans_validate_cbs, {
          init,         %% econfd:cb_ctx()
          stop          %% econfd:cb_ok()
         }).


%% extern database db callbacks
-record(confd_db_cbs, {
          candidate_commit,            %% econfd:cb_candidate_commit()
          candidate_confirming_commit, %% econfd:cb_ok_db()
          candidate_reset,             %% econfd:cb_ok_db()
          candidate_chk_not_modified,  %% econfd:cb_ok_db()
          candidate_rollback_running,  %% econfd:cb_ok_db()
          candidate_validate,          %% econfd:cb_ok_db()
          add_checkpoint_running,      %% econfd:cb_ok_db()
          del_checkpoint_running,      %% econfd:cb_ok_db()
          activate_checkpoint_running, %% econfd:cb_ok_db()
          copy_running_to_startup,     %% econfd:cb_ok_db()
          running_chk_not_modified,    %% econfd:cb_ok_db()
          lock,                        %% econfd:cb_db()
          unlock,                      %% econfd:cb_db()
          lock_partial,                %% econfd:cb_lock_partial()
          unlock_partial,              %% econfd:cb_unlock_partial()
          delete_config                %% econfd:cb_db()
         }).


%% data callbacks
-record(confd_data_cbs, {
          callpoint,        %% atom()  (the name of the cp in the data model)

                            %% The exists_optional callback must be present
                            %% if the data model has optional typeless elements

          exists_optional,  %% econfd:cb_exists_optional()
          get_elem,         %% econfd:cb_get_elem()
          get_next,         %% econfd:cb_get_next()

          %% The following 3 callbacks must be implemented only when we
          %% use external database config data, e.g. not for statistics
          set_elem,         %% econfd:cb_set_elem()
          create,           %% econfd:cb_create()
          remove,           %% econfd:cb_remove()

          %% optional (get_next by key values)
          find_next,        %% econfd:cb_find_next()

          %% optional optimizations
          num_instances,    %% econfd:cb_num_instances()
          get_object,       %% econfd:cb_get_object()
          get_next_object,  %% econfd:cb_get_next_object()
          find_next_object, %% econfd:cb_find_next_object()

          %% These callbacks must be implemented if we are using 'choice'
          get_case          :: econfd:cb_get_case(),
          set_case          :: econfd:cb_set_case(),

          %% These callbacks must be implemented by a config data provider
          %% if /confdConfig/enableAttributes in confd.conf is 'true'
          get_attrs,        %% econfd:cb_get_attrs()
          set_attr,         %% econfd:cb_set_attr()

          %% This callback must be implemented if we are using 'ordered-by user'
          move_after,       %% econfd:cb_move_after()

          %% This callback must (only) be implemented for a
          %% per-transaction-invoked transaction hook
          write_all,        %% econfd:cb_write_all()

          index  %% Internal field, set and used by econfd
         }).


%% validation callbacks
-record(confd_valpoint_cb,{
          valpoint,   %% atom() (name of valpoint in the data model)
          validate,   %% econfd:cb_validate()

          index  %% Internal field, set and used by econfd
         }).


%% action callbacks
-record(confd_action_cb,{
          actionpoint,   %% atom() (name of actionpoint in the data model)
          action,        %% econfd:cb_action()
          completion,    %% econfd:completion_action()
          index  %% Internal field, set and used by econfd
         }).


%% authentication callback
-record(confd_authentication_cb,{
          auth           %% econfd:cb_authentication()
         }).


%% notification replay callbacks
-record(confd_notification_stream_cbs, {
          streamname,         %% binary()
          get_log_times,      %% econfd:cb_get_log_times()
          replay,             %% econfd:cb_replay()
          opaque              %% term()
         }).


%% callbacks for user-defined types
-record(confd_type_cbs, {
          typepoint,    %% atom() (name of typepoint in the data model)
          str_to_val,   %% econfd:cb_str_to_val()
          val_to_str,   %% econfd:cb_val_to_str()
          validate      %% econfd:cb_validate_value()
         }).


%% callbacks for NCS services
%% Proplist = [{binary(), binary()}]
%% all callbacks return {ok, Proplist} | {ok, Proplist, Tctx} |
%%    {error, econfd:error_reason()}
-record(ncs_service_cbs, {
          servicepoint,
          pre_modification,  %% (Tctx, Type:atom(), econfd:ikeypath(), Proplist)
          post_modification, %% (Tctx, Type:atom(), econfd:ikeypath(), Proplist)
          pre_lock_create, %%(Tctx,econfd:ikeypath(),Proplist,TrInTr::integer())
          create,      %% (Tctx, econfd:ikeypath(), Proplist, TrInTr::integer())
          index
         }).

-record(ncs_nano_service_cbs, {
          servicepoint,
          create,      %% (Tctx, econfd:ikeypath(), Component:binary(),
                       %%  State:binary(), Proplist, TrInTr::integer())
          delete,      %% (Tctx, econfd:ikeypath(), Component:binary(),
                       %%  State:binary(), Proplist, TrInTr::integer())
          index
         }).


%% extended error reporting
-record(confd_error, {
          code,          %% in_use | resource_denied |
                         %% inconsistent_value | access_denied |
                         %% application | application_internal |
                         %% data_missing | interrupt
          apptag,        %% undefined | econfd:qtag()
          str,           %% undefined | binary()
          info           %% undefined | [tagval()]
         }).


-record(confd_crypto_keys, {
          des3_key1,   %% binary(8)
          des3_key2,   %% binary(8)
          des3_key3,   %% binary(8)
          des3_ivec,   %% binary(8)
          des3_keys_initialized = false, %% bool()

          aes_key,  %% binary(16)
          aes_ivec, %% binary(16)
          aes_keys_initialized = false%% bool()
          }).





%% proto() in #confd_user_info{} and notification records
-define(CONFD_PROTO_UNKNOWN, 0). %% never to be used
-define(CONFD_PROTO_TCP,     1).
-define(CONFD_PROTO_SSH,     2).
-define(CONFD_PROTO_SYSTEM,  3). %% ConfD initiated transactions
-define(CONFD_PROTO_CONSOLE, 4).
-define(CONFD_PROTO_SSL,     5).
-define(CONFD_PROTO_HTTP,    6).
-define(CONFD_PROTO_HTTPS,   7).
-define(CONFD_PROTO_UDP,     8).


-define(CONFD_USESS_LOCK_MODE_NONE      , 0).
-define(CONFD_USESS_LOCK_MODE_PRIVATE   , 1).
-define(CONFD_USESS_LOCK_MODE_EXCLUSIVE , 2).
-define(CONFD_USESS_LOCK_MODE_SHARED    , 3).

-define(CONFD_USESS_FLAG_FORWARD, (1 bsl 0)).


-record(confd_user_info, {
          username = <<"">>,    %% binary()
          context = <<"">>,     %% binary(),
          ip = {0,0,0,0},       %% confd::ip()
          port = 0,             %% integer()
          proto = ?CONFD_PROTO_TCP,   %% proto()
          usid,   %% integer()
          logintime, %% integer() (seconds since 1970-01-01 00:00:00 UTC)
          lockmode,  %% 0 | 1 | 2 | 3
          snmp_v3_ctx = <<"">>,  %% binary()
          clearpass = <<"">>,    %% binary() (empty if we don't have the pass)
          flags = 0,             %% int(), CONFD_USESS_FLAG_...
          actx                   %% #confd_action_ctx{} in action callback
         }).

-record(confd_user_identification, {
          vendor          :: undefined | string(),
          product         :: undefined | string(),
          version         :: undefined | string(),
          client_identity :: undefined | string()
         }).

-record(maapi_cursor, {
          n = -1,
          secondary_index = '',
          prevterm = first,
          ikp,
          isrel,
          socket,
          thandle,
          cursor_id}).

-record(maapi_rollback, {
          nr       :: integer(),
          creator  :: binary(),
          date     :: binary(),
          via      :: binary(),
          fixed_nr :: integer(),
          label    :: binary(),
          comment  :: binary()
         }).

%% the defines below is the ConfD value notation. I.e. whenever our code
%% receives or use any values such as the keys in ikeypaths or the
%% actual value to econfd_maapi:set_elem() econfd::value() is always
%% on the form below. Use the CONFD_ macros to manipulate the values

-define(C_NOEXISTS,  1).   %% end marker
-define(C_XMLTAG,    2).   %% xml_tag

-define(C_BUF,       5).   %% YANG string Erep: binary()
-define(C_INT8,      6).   %% YANG int8   Erep: {6, Val}
-define(C_INT16,     7).   %% YANG int16  Erep: {7, Val}
-define(C_INT32,     8).   %% YANG int32  Erep: Val
-define(C_INT64,     9).   %% YANG int64  Erep: {9, Val}
-define(C_UINT8,     10).  %% YANG unsigned int8          Erep:  {10, Val}
-define(C_UINT16,    11).  %% YANG unsigned int16         Erep: {11, Val}
-define(C_UINT32,    12).  %% YANG unsigned int32         Erep: {12, Val}
-define(C_UINT64,    13).  %% YANG unsigned int64         Erep: {13, Val}
-define(C_DOUBLE,    14).  %% XML xs:float, xs:double     Erep: Val
-define(C_IPV4,      15).  %% YANG inet:ipv4-address      Erep: 4-tuple
-define(C_IPV6,      16).  %% YANG inet:ipv6-address      Erep: 8-tuple
-define(C_BOOL,      17).  %% YANG boolean                Erep: true|false
-define(C_QNAME,     18).  %% XML  xs:QName       Erep: {binary(), binary()}
-define(C_DATETIME,  19).  %% YANG yang:date-and-time
                           %% Erep: {19, {Y,M,D,H,M,S,Mcr,TZ,TZM}}
-define(C_DATE,      20).  %% XML xs:date        Erep: {20, {Y,M,D,TZ,TZM}}
-define(C_TIME,      23).  %% XML xs:time        Erep: {23, {H,M,S,Mcr,TZ,TZM}}
-define(C_DURATION,  27).  %% XML xs:duration    Erep: {27, {Y,M,D,H,M,S,Mcr}}
-define(C_ENUM_VALUE,28).  %% YANG enum value    Erep: {28, Int}
-define(C_BIT32,     29).  %% YANG bits size 32  Erep: {29, Int}
-define(C_BIT64,     30).  %% YANG bits size 64  Erep: {30, Int}
-define(C_LIST,      31).  %% YANG leaf-list     Erep: [Value|Values]
-define(C_XMLBEGIN,  32).  %% start/end of container
-define(C_XMLEND,    33).  %% used to construct array replies
-define(C_INSTANCE_IDENTIFIER, 34).
                           %% YANG instance-identifier
                           %% Erep: {34, econfd:ikeypath()}
-define(C_UNION,     35).  %% union - not used in API functions
-define(C_OID,       38).  %% YANG yang:object-identifier
                           %% Erep: {38, Int32Binary}
-define(C_BINARY,     39).  %% YANG binary            Erep: {39, binary()}
-define(C_IPV4PREFIX, 40).  %% YANG inet:ipv4-prefix  Erep: {40, {4-tuple, Int}}
-define(C_IPV6PREFIX, 41).  %% YANG inet:ipv6-prefix  Erep: {41, {8-tuple, Int}}
-define(C_DECIMAL64,  43).  %% YANG decimal64)        Erep: {43, {Int, Int}}
-define(C_IDENTITYREF,44).  %% YANG identityref       Erep: {44, {Int, Int}}
-define(C_DQUAD,       46). %% YANG yang:dotted-quad  Erep: {46, binary()}
-define(C_HEXSTR,      47). %% YANG yang:hex-string   Erep: {47, binary()}
-define(C_IPV4_AND_PLEN, 48). %% YANG tailf:ipv4-address-and-prefix-length
                              %% Erep: {48, {4-tuple, Int}}
-define(C_IPV6_AND_PLEN, 49). %% YANG tailf:ipv6-address-and-prefix-length
                              %% Erep: {49, {8-tuple, Int}}
-define(C_BITBIG,     50).  %% YANG bits size > 64    Erep: {50, binary()}

-define(C_ENUM_HASH,  28).  %% backwards compat
-define(C_OBJECTREF,  34).  %% backwards compat


%% Use the macros below to construct and match values
%% in code such as:
%%  case econfd_cdb:get_elem(...) of
%%        ?CONFD_INT64(42) ->
%%            foo;
%%
%% or
%%  econfd_cdb:set_elem(... ?CONFD_INT64(777));
%%

-define(CONFD_XMLTAG(Ns, Tag), [Ns|Tag]).
-define(CONFD_BUF(X),    X).
-define(CONFD_INT8(X),   {?C_INT8, X}).
-define(CONFD_INT16(X),  {?C_INT16, X}).
-define(CONFD_INT32(X),  X).
-define(CONFD_INT64(X),  {?C_INT64, X}).
-define(CONFD_UINT8(X),  {?C_UINT8, X}).
-define(CONFD_UINT16(X), {?C_UINT16, X}).
-define(CONFD_UINT32(X), {?C_UINT32, X}).
-define(CONFD_UINT64(X), {?C_UINT64, X}).
-define(CONFD_DOUBLE(X), X).
-define(CONFD_IPV4(X),   X).
-define(CONFD_IPV6(X),   X).
-define(CONFD_BOOL(X),   X).
-define(CONFD_QNAME(X),      {?C_QNAME, X}).
-define(CONFD_DATETIME(X),   {?C_DATETIME, X}).
-define(CONFD_DATE(X),       {?C_DATE, X}).
-define(CONFD_TIME(X),       {?C_TIME, X}).
-define(CONFD_DURATION(X),   {?C_DURATION, X}).
-define(CONFD_ENUM_VALUE(X), {?C_ENUM_VALUE, X}).
-define(CONFD_BIT32(X),      {?C_BIT32, X}).
-define(CONFD_BIT64(X),      {?C_BIT64, X}).
-define(CONFD_BITBIG(X),     {?C_BITBIG, X}).
-define(CONFD_LIST(X),       X).
-define(CONFD_OBJECTREF(X),  {?C_OBJECTREF, X}).
-define(CONFD_OID(X),        {?C_OID, X}).
-define(CONFD_BINARY(X),     {?C_BINARY, X}).
-define(CONFD_IPV4PREFIX(X), {?C_IPV4PREFIX, X}).
-define(CONFD_IPV6PREFIX(X), {?C_IPV6PREFIX, X}).
-define(CONFD_DECIMAL64(X),  {?C_DECIMAL64, X}).
-define(CONFD_IDENTITYREF(X), {?C_IDENTITYREF, X}).
-define(CONFD_DQUAD(X),      {?C_DQUAD, X}).
-define(CONFD_HEXSTR(X),     {?C_HEXSTR, X}).
-define(CONFD_IPV4_AND_PLEN(X), {?C_IPV4_AND_PLEN, X}).
-define(CONFD_IPV6_AND_PLEN(X), {?C_IPV6_AND_PLEN, X}).
-define(CONFD_ENUM_HASH(X),  {?C_ENUM_HASH, X}). % backwards compat
-define(CONFD_XMLTAG(X),     {?C_XMLTAG, X}).    % deprecated
-define(CONFD_INSTANCE_IDENTIFIER(X), {?C_INSTANCE_IDENTIFIER, X}).

%% Attribute values
%% CONFD_ATTR_TAGS: value is C_LIST of C_BUF/C_STR
-define(CONFD_ATTR_TAGS,       16#80000000).
%% CONFD_ATTR_ANNOTATION: value is C_BUF/C_STR
-define(CONFD_ATTR_ANNOTATION, 16#80000001).
%% CONFD_ATTR_INACTIVE: value is 'true' (C_BOOL)
-define(CONFD_ATTR_INACTIVE,   16#00000000).


%% NCS specific attributes
%% value = int32, used by NCS fastmap code
-define(CONFD_ATTR_REFCOUNT,  16#80000002).
%% value is [instance-identifier], used by fastmap code
-define(CONFD_ATTR_BACKPOINTER,  16#80000003).
-define(CONFD_ATTR_WHEN,  16#80000004).
-define(CONFD_ATTR_ORIGINAL_VALUE,  16#80000005).

-record(confd_daemon_ctx, {
          trans_cb,            %% #confd_trans_cbs{}
          trans_validate_cb,   %% #confd_trans_validate_cbs{}
          db_cb,               %% #confd_db_cbs{}
          data_cbs = [],       %% [#confd_data_cbs{}]
          service_cbs = [],    %% [#ncs_service_cbs{}]
          valp_cbs = [],       %% [#confd_valpoint_cb{}]
          action_cbs = [],     %% [#confd_action_cb{}]
          authentication_cb,   %% #confd_authentication_cb{}
          notif_cbs = [],      %% [#confd_notification_stream_cbs{}]
          ctl,                 %% confd::socket()
          worker,              %% confd::socket()
          notif_worker,        %% confd::socket() | undefined
          wint,                %% integer()
          nwint,               %% integer()
          name,                %% binary()
          daemon_pid,          %% pid()
          daemon_id,           %% integer()
          debug_level,         %% integer()
          worker_pid,          %% pid()
          user_sessions,       %% ets:tid()
          transactions,        %% ets:tid()
          estream,             %% file::file_handle()
          ip, port,            %% addr of ConfD
          d_opaque,            %% OPaque::term()
          flags                %% integer()
         }).

-record(confd_trans_ctx, {
          dx,                   %%  #confd_daemon_ctx{}
          mode,                 %%  integer() READ | READ_WRITE
          dbname,               %%  integer() CANDIDATE | RUNNING | STARTUP
          thandle,              %%  integer()
          uinfo,                %%  #confd_user_info{}
          opaque,               %%  private user data
          secondary_index,      %%  atom()
          callpoint_opaque,     %%  atom() from data model
          request_data,         %%  [{atom(), term()}] from northbound agent
          hide_inactive,        %%  boolean() hide config w CONFD_ATTR_INACTIVE

          %%%% confd internal fields
          lastop,
          last_proto_op,
          query_ref,
          reused = 0,
          in_num_instances = 0
         }).

%% Only used if we have external data
-record(confd_db_ctx, {
          dx,     %%  #confd_daemon_ctx{}
          lastop,
          did,
          qref,
          uinfo}).

%% For action callbacks
-record(confd_action_ctx, {
          dx,                  %% #confd_daemon_ctx{}
          thandle,             %% integer() from CLI / Web UI (-1 == invalid)
          actionpoint_opaque,  %% atom() from data model

          %%%% confd internal fields
          query_ref
         }).

%% For authentication callback
-record(confd_authentication_ctx, {
          uinfo,               %% #confd_user_info{}
          method,              %% binary()
          success,             %% boolean()
          ainfo                %% [Group::binary()] |
                               %%   {Logno::integer(), Reason::binary()}
         }).

%% For NETCONF notifications
-record(confd_notification_ctx, {
          streamname,   %%  binary()
          notif_worker, %%  econfd::socket()
          opaque,       %%  private user data, passed from cbs record

          %%%% confd internal fields
          subid = 0,
          query_ref,
          flags
         }).


%% Transactions mode
-define(CONFD_READ, 1).
-define(CONFD_READ_WRITE, 2).

%% DbType
-define(CONFD_NO_DB,       0).
-define(CONFD_CANDIDATE,   1).
-define(CONFD_RUNNING,     2).
-define(CONFD_STARTUP,     3).
-define(CONFD_OPERATIONAL, 4).
-define(CONFD_TRANSACTION, 5).  % trans_in_trans
-define(CONFD_PRE_COMMIT_RUNNING, 6).

%% FindNextType
-define(CONFD_FIND_NEXT,          0).
-define(CONFD_FIND_SAME_OR_NEXT,  1).


-define(CONFD_SILENT, 0).
-define(CONFD_DEBUG,  1).
-define(CONFD_TRACE,  2).      %% trace callback calls

%% CDB subscription types for econfd_cdb:subscribe/5
-define(CDB_SUB_RUNNING,          1).
-define(CDB_SUB_RUNNING_TWOPHASE, 2).
-define(CDB_SUB_OPERATIONAL,      3).

%% CDB notification types provided to an arity-3 fun passed to wait/3
-define(CDB_SUB_PREPARE, 1).
-define(CDB_SUB_COMMIT,  2).
-define(CDB_SUB_ABORT,   3).
-define(CDB_SUB_OPER,    4).

%% CDB notification flags provided to an arity-3 fun passed to wait/3
-ifndef(CDB_SUB_FLAG_IS_LAST).
-define(CDB_SUB_FLAG_IS_LAST,  (1 bsl 0)).
-define(CDB_SUB_FLAG_TRIGGER,  (1 bsl 1)).
-define(CDB_SUB_FLAG_REVERT,   (1 bsl 2)).
-define(CDB_SUB_FLAG_HA_SYNC,  (1 bsl 3)).
-endif.

%% CDB sync types
-define(CDB_DONE_PRIORITY, 1).
-define(CDB_DONE_SOCKET, 2).
-define(CDB_DONE_TRANSACTION, 3).
-define(CDB_DONE_OPERATIONAL, 4).

%% operations and return values for diff_iterate()

-define(MOP_CREATED,    1).
-define(MOP_DELETED,    2).
-define(MOP_MODIFIED,   3).
-define(MOP_VALUE_SET,  4).
-define(MOP_MOVED_AFTER,5).

-define(ITER_STOP, 1).
-define(ITER_RECURSE , 2).
-define(ITER_CONTINUE , 3).

-define(CDB_ITER_WANT_PREV,            (1 bsl 0)).
-define(CDB_ITER_WANT_ANCESTOR_DELETE, (1 bsl 1)).
-define(MAAPI_ITER_WANT_ATTR,          (1 bsl 2)).
-define(CDB_ITER_WANT_CLI_STR,         (1 bsl 3)).   % internal use
-define(CDB_ITER_WANT_SCHEMA_ORDER,    (1 bsl 4)).
-define(CDB_ITER_WANT_LEAF_FIRST_ORDER,(1 bsl 5)).
-define(CDB_ITER_WANT_LEAF_LAST_ORDER, (1 bsl 6)).
-define(CDB_ITER_WANT_REVERSE,         (1 bsl 7)).
-define(MAAPI_ITER_WANT_P_CONTAINER,   (1 bsl 8)).
-define(CDB_ITER_WANT_LEAF_LIST_AS_LEAF,   (1 bsl 9)).   % DEPRECATED
-define(MAAPI_ITER_WANT_LEAF_LIST_AS_LEAF, (1 bsl 9)).   % DEPRECATED


%% econfd_cdb::dbtype()
-define(CDB_RUNNING,     1).
-define(CDB_STARTUP,     2).
-define(CDB_OPERATIONAL, 3).
-define(CDB_PRE_COMMIT_RUNNING, 4).

-ifndef(CDB_LOCK_WAIT).
%% Flags for econfd_cdb:new_session/3
-define(CDB_LOCK_WAIT,     (1 bsl 0)).
-define(CDB_LOCK_SESSION,  (1 bsl 1)).
-define(CDB_LOCK_REQUEST,  (1 bsl 2)).
-define(CDB_LOCK_PARTIAL,  (1 bsl 3)).
-endif.

-ifndef(MAAPI_FLAG_HINT_BULK).
%% Flags for econfd_maapi:start_trans/5 and econfd_maapi:set_flags/3
-define(MAAPI_FLAG_HINT_BULK,     (1 bsl 0)).
-define(MAAPI_FLAG_NO_DEFAULTS,   (1 bsl 1)).
-define(MAAPI_FLAG_CONFIG_ONLY,   (1 bsl 2)).
-define(MAAPI_FLAG_HIDE_INACTIVE, (1 bsl 3)). % econfd_maapi:start_trans/5 only
-define(MAAPI_FLAG_DELAYED_WHEN,  (1 bsl 6)). % econfd_maapi:start_trans/5 only

%% Flag for econfd_maapi:start_trans_in_trans/5
-define(MAAPI_FLAG_RUN_SET_HOOKS, (1 bsl 7)).

%% Flags for econfd_maapi:init_upgrade/3
-define(MAAPI_UPGRADE_KILL_ON_TIMEOUT, (1 bsl 0)).
-endif.

%% Flags for econfd:set_daemon_flags/2
-define(CONFD_DAEMON_FLAG_STRINGSONLY,            (1 bsl 1)).
-define(CONFD_DAEMON_FLAG_NO_DEFAULTS,            (1 bsl 2)).
-define(CONFD_DAEMON_FLAG_REG_REPLACE_DISCONNECT, (1 bsl 4)).
-define(CONFD_DAEMON_FLAG_LEAF_LIST_AS_LEAF,      (1 bsl 5)).   % DEPRECATED
-define(CONFD_DAEMON_FLAG_PREFER_BULK_GET,        (1 bsl 6)).
-define(CONFD_DAEMON_FLAG_BULK_GET_CONTAINER,     (1 bsl 7)).

%% Daemon flag for internal library use
-define(CONFD_DAEMON_FLAG_SEND_IKP,               (1 bsl 0)).


-define(confd_trace(Dx, Fmt, Args),
        if Dx#confd_daemon_ctx.debug_level == ?CONFD_TRACE ->
                io:format(Dx#confd_daemon_ctx.estream, "TRACE " ++ Fmt, Args) ;
           true ->
                ok
        end).

-define(confd_trace_ret(Dx, Fmt, Args),
        if Dx#confd_daemon_ctx.debug_level == ?CONFD_TRACE ->
                io:format(Dx#confd_daemon_ctx.estream, Fmt, Args) ;
           true ->
                ok
        end).

-endif.


-ifndef(EM_UNDEFINED).

%% notification bitmasks, set several of these to get
%% different sorts of notifications.
-define(CONFD_NOTIF_AUDIT ,          (1 bsl 0)).
-define(CONFD_NOTIF_DAEMON ,         (1 bsl 1)).
-define(CONFD_NOTIF_TAKEOVER_SYSLOG, (1 bsl 2)).
-define(CONFD_NOTIF_COMMIT_SIMPLE ,  (1 bsl 3)).
-define(CONFD_NOTIF_COMMIT_DIFF ,    (1 bsl 4)).
-define(CONFD_NOTIF_USER_SESSION,    (1 bsl 5)).
-define(CONFD_NOTIF_HA_INFO,         (1 bsl 6)).
-define(CONFD_NOTIF_SUBAGENT_INFO,   (1 bsl 7)).
-define(CONFD_NOTIF_COMMIT_FAILED,   (1 bsl 8)).
-define(CONFD_NOTIF_SNMPA,           (1 bsl 9)).
-define(CONFD_NOTIF_FORWARD_INFO,    (1 bsl 10)).
-define(CONFD_NOTIF_NETCONF,         (1 bsl 11)).
-define(CONFD_NOTIF_DEVEL  ,         (1 bsl 12)).
-define(CONFD_NOTIF_HEARTBEAT  ,     (1 bsl 13)).
-define(CONFD_NOTIF_CONFIRMED_COMMIT, (1 bsl 14)).
-define(CONFD_NOTIF_UPGRADE_EVENT,   (1 bsl 15)).
-define(CONFD_NOTIF_COMMIT_PROGRESS, (1 bsl 16)).
-define(CONFD_NOTIF_HEALTH_CHECK,    (1 bsl 18)).
-define(CONFD_NOTIF_STREAM_EVENT,    (1 bsl 19)).
-define(NCS_NOTIF_PACKAGE_RELOAD,    (1 bsl 21)).
-define(NCS_NOTIF_CQ_PROGRESS,       (1 bsl 22)).
-define(CONFD_NOTIF_REOPEN_LOGS,     (1 bsl 23)).

%% kept for backwards compatibility - don't use
-define(CONFD_NOTIF_SYSLOG, ?CONFD_NOTIF_DAEMON).
-define(CONFD_NOTIF_SYSLOG_TAKEOVER,
        (?CONFD_NOTIF_DAEMON bor ?CONFD_NOTIF_TAKEOVER_SYSLOG).

%% Various records returned from econfd_notif:handle_notif/1
-record(econfd_notif_audit, {
          logno,     %% integer() The logno as defined per  econfd_logsyms.hrl
          user,      %% binary()  The username
          usid,      %% integer() The user session id
          msg        %% binary()  The actual audit message
         }).

%% event delivered from the CONFD_NOTIF_DAEMON the CONFD_NOTIF_NETCONF
%% and the CONFD_NOTIF_DEVEL flags
-record(econfd_notif_syslog, {
          logno,     %% integer() The logno as defined per  econfd_logsyms.hrl
          prio,      %% integer() UNIX prio number - platform specific
          msg        %% binary()  Syslog msg string
         }).

%% flags in #econfd_notif_commit_simple{} and #econfd_notif_commit_diff{}
-define(CONFD_NOTIF_COMMIT_FLAG_CONFIRMED,           (1 bsl 0))
-define(CONFD_NOTIF_COMMIT_FLAG_CONFIRMED_EXTENDED,  (1 bsl 1)).

-record(econfd_notif_commit_simple, {
          db,        %% integer() - which db are we committing towards
          uinfo,     %% #confd_user_info{} The user who is committing
          commit_diff_available,  %% integer()
          flags,     %% integer() CONFD_NOTIF_COMMIT_FLAG_XXX
          %% backwards compatibility:
          user,      %% binary()  The username who is committing
          ip,        %% ip()      From which IP
          context,   %% binary()  which agent is used
          usid,      %% integer() The user session id
          proto      %% proto()   which proto was used
         }).


-record(econfd_notif_commit_diff, {
          db,        %% integer() - which db are we committing towards
          uinfo,     %% #confd_user_info{} The user who is committing
          th,        %% integer() The th that can be used to traverse the diff
          flags,     %% integer() CONFD_NOTIF_COMMIT_FLAG_XXX
          comment,   %% 'undefined' | binary() Commit comment
          label,     %% 'undefined' | binary() Commit label
          %% backwards compatibility:
          user,      %% binary()  The username who is comitting
          ip,        %% ip()      From which IP
          context,   %% binary()  which agent is used
          usid,      %% integer() The user session id
          proto      %% proto()   which proto was used
         }).


%% Values for ConfirmedCommitType::integer() in #econfd_notif_confirmed_commit{}
-define(CONFD_CONFIRMED_COMMIT,  1).
-define(CONFD_CONFIRMING_COMMIT, 2).
-define(CONFD_ABORT_COMMIT,      3).

-record(econfd_notif_confirmed_commit, {
          type,     %% ConfirmedCommitType::integer()
          timeout,  %% integer()  in seconds
                    %% timeout is > 0 when type is CONFD_CONFIRMED_COMMIT,
                    %% otherwise it is 0
          uinfo     %% #confd_user_info{}
         }).


%% session_db()
-define(EM_UNDEFINED,   0).
-define(EM_CANDIDATE,   1).
-define(EM_RUNNING,     2).
-define(EM_STARTUP,     3).
-define(EM_OPERATIONAL, 4).
-define(EM_TRANSACTION, 5).  % trans_in_trans

%% session_type()

-define(CONFD_USER_SESS_START, 1).      %% a user session is started
-define(CONFD_USER_SESS_STOP,  2).
-define(CONFD_USER_SESS_LOCK,  3).      %% a database is locked
-define(CONFD_USER_SESS_UNLOCK,4).
-define(CONFD_USER_SESS_START_TRANS, 5). %% a database transaction is started
-define(CONFD_USER_SESS_STOP_TRANS, 6).


-record(econfd_notif_user_session, {
          type,       %% session_type() - START, STOP etc as above
          uinfo,      %% #confd_user_info{}
          db,         %% session_db()
          %% backwards compatibility:
          user,       %% binary()  - username
          ip,         %% ip()      - logged in from where
          context,    %% binary()  - using which agent
          usid,       %% integer() - user session id
          proto,      %% proto()   - which proto
          logintime,  %% integer() - seconds since 1970-01-01 00:00:00 UTC
          clearpass   %% binary()  - password used
         }).


%% variuos values for SubagentInfoType::integer()
%% in #econfd_notif_subagent_info{}

%% Data::term() is SubagentName::binary()
-define(CONFD_SUBAGENT_INFO_UP,           1).
%% Data::term() is SubagentName::binary()
-define(CONFD_SUBAGENT_INFO_DOWN,         2).

-record(econfd_notif_subagent_info, {
          type,      %% integer() - type of event
          name       %% binary()  - which agent
         }).


%% provider()
-define(DP_CDB,        1).
-define(DP_NETCONF,    2).
-define(DP_EXTERNAL,   3).
-define(DP_SNMPGW,     4).
-define(DP_JAVASCRIPT, 5).


%% really ought to be a union - but erlang doesn't have them
-record(econfd_notif_commit_failed, {
          provider,   %% provider()
          dbname,     %% integer()

          ip,         %% ip()
          port,       %% integer()

          daemon_name %% binary()
         }
       ).



%% pdutype()
-define(CONFD_SNMPA_PDU_V1TRAP,           1).
-define(CONFD_SNMPA_PDU_V2TRAP,           2).
-define(CONFD_SNMPA_PDU_INFORM,           3).
-define(CONFD_SNMPA_PDU_GET_RESPONSE,     4).
-define(CONFD_SNMPA_PDU_GET_REQUEST,      5).
-define(CONFD_SNMPA_PDU_GET_NEXT_REQUEST, 6).
-define(CONFD_SNMPA_PDU_REPORT,           7).
-define(CONFD_SNMPA_PDU_GET_BULK_REQUEST, 8).
-define(CONFD_SNMPA_PDU_SET_REQUEST,      9).


-record(econfd_notif_snmpa, {
          pdutype,     %% pdutype()()
          ip,          %% ip()
          port,        %% integer()
          errstatus,   %% integer()
          errindex,    %% integer()
          varbind     %% snmp_varbind()
         }).




%% ha records
-record(ha_node, {
          nodeid,   %% econfd::value()
          addr      %% (IP) address
         }).

-define(CONFD_HA_STATE_NONE,        1).
-define(CONFD_HA_STATE_SLAVE,       2).
-define(CONFD_HA_STATE_MASTER,      3).
-define(CONFD_HA_STATE_SLAVE_RELAY, 4).

-record(ha_status, {
          status, %% ?CONFD_HA_STATE_
          data}). %% [#ha_node{}]



%% variuos values for HaInfoType::integer() in #econfd_notif_ha{}
%% notification message.
-define(CONFD_HA_INFO_NOMASTER,           1).  % we have no master
-define(CONFD_HA_INFO_SLAVE_DIED,         2).  % a slave disappeared
-define(CONFD_HA_INFO_SLAVE_ARRIVED,      3).  % a slave arrived to us
-define(CONFD_HA_INFO_SLAVE_INITIALIZED,  4).  % CDB is initialized
-define(CONFD_HA_INFO_IS_MASTER,          5).  % we are now master
-define(CONFD_HA_INFO_IS_NONE,            6).  % we are now none
-define(CONFD_HA_INFO_BESLAVE_RESULT,     7).  % result of async beslave()

-record(econfd_notif_ha, {
          type,   %% HaInfoType()::integer()
          data    %% depending on type, data is either of
                  %%
                  %% if ?CONFD_HA_INFO_NOMASTER  ?CONFD_ERR_HA_CLOSED |
                  %%                             ?CONFD_ERR_HA_NOTICK
                  %% if ?CONFD_HA_INFO_SLAVE_DIED DeadSlave::ha_node()
                  %% if ?CONFD_HA_INFO_SLAVE_ARRIVED NewSlave::ha_node()
                  %% if ?CONFD_HA_INFO_CDB_INITIALIZED initByCdbCopy::integer()
                  %% if ?CONFD_HA_INFO_BESLAVE_RESULT 0 | ?CONFD_ERR_HA_XXX
         }).


%% Values for ForwardInfoType::integer() in #econfd_notif_forward_info{}
-define(CONFD_FORWARD_INFO_UP,          1).
-define(CONFD_FORWARD_INFO_DOWN,        2).
-define(CONFD_FORWARD_INFO_FAILED,      3).


-record(econfd_notif_forward_info, {
          type,     %% ForwardInfoType::integer()
          target,   %% binary() - which forward target
          uinfo     %% #confd_user_info{}
         }).


%% Values for UpgradeEventType::integer() in #econfd_notif_upgrade{}
-define(CONFD_UPGRADE_INIT_STARTED,   1).
-define(CONFD_UPGRADE_INIT_SUCCEEDED, 2).
-define(CONFD_UPGRADE_PERFORMED,      3).
-define(CONFD_UPGRADE_COMMITED,       4).
-define(CONFD_UPGRADE_ABORTED,        5).

-record(econfd_notif_upgrade, {
          event     %% UpgradeEventType::integer()
         }).


-record(econfd_notif_commit_progress, {
          dbname,     %% integer()  CANDIDATE | RUNNING | STARTUP
          usid,       %% integer()  The user session id
          thandle,    %% integer()  The transaction handle
          msg         %% binary()   Progress message
         }).


-record(econfd_notif_stream_event, {
          type,         %% notification_event | notification_complete |
                        %% replay_complete | replay_failed
          event_time,   %% econfd:datetime()   Time for 'notification_event'
          values,       %% [econfd:tagval()]   XML for 'notification_event'
          replay_error  %% binary()            Error message for 'replay_failed'
         }).

-define(NCS_NOTIF_CQ_PROGRESS_WAITING,           1).
-define(NCS_NOTIF_CQ_PROGRESS_EXECUTING,         2).
-define(NCS_NOTIF_CQ_PROGRESS_LOCKED,            3).
-define(NCS_NOTIF_CQ_PROGRESS_COMPLETED,         4).
-define(NCS_NOTIF_CQ_PROGRESS_FAILED,            5).
-define(NCS_NOTIF_CQ_PROGRESS_DELETED,           6).

-record(econfd_notif_ncs_cq_progress, {
          type,              %% NcsCommitQProgress::integer()
          cq_tag,            %% parent item tag
          cq_id,             %% cq item
          timestamp,         %% datetime
          completed_devices, %% [device]
          transient_devices, %% [device]
          failed_devices,    %% [{device, reason}]
          completed_services,%% [{service, [completed_devs]}]
          failed_services    %% [{service, [completed_devs], [failed_devs]}]
         }).


-endif. %% EM_UNDEFINED

%% Schema information

-record(confd_nsinfo, {
          namespace,      %% econfd:namespace()
          prefix,         %% string()
          revision,       %% undefined | binary()
          module          %% undefined | binary()
         }).

%% flag bits in #confd_cs_node.flags
-define(CONFD_CS_IS_LIST,          (1 bsl 0)).
-define(CONFD_CS_IS_WRITE,         (1 bsl 1)).
-define(CONFD_CS_IS_CDB,           (1 bsl 2)).
-define(CONFD_CS_IS_ACTION,        (1 bsl 3)).
-define(CONFD_CS_IS_PARAM,         (1 bsl 4)).
-define(CONFD_CS_IS_RESULT,        (1 bsl 5)).
-define(CONFD_CS_IS_NOTIF,         (1 bsl 6)).
-define(CONFD_CS_IS_CASE,          (1 bsl 7)).
-define(CONFD_CS_IS_CONTAINER,     (1 bsl 8)).
-define(CONFD_CS_HAS_WHEN,         (1 bsl 9)).
-define(CONFD_CS_HAS_DISPLAY_WHEN, (1 bsl 10)).
-define(CONFD_CS_META_DATA,        (1 bsl 11)).
-define(CONFD_CS_IS_WRITE_ALL,     (1 bsl 12)).
-define(CONFD_CS_IS_LEAF_LIST,     (1 bsl 13)).
-define(CONFD_CS_IS_LEAFREF,       (1 bsl 14)).

%% values for #confd_cs_node.cmp
-define(CONFD_CS_CMP_NORMAL,        0).
-define(CONFD_CS_CMP_SNMP,          1).
-define(CONFD_CS_CMP_SNMP_IMPLIED,  2).
-define(CONFD_CS_CMP_USER,          3).
-define(CONFD_CS_CMP_UNSORTED,      4).

-record(confd_cs_node, {
          tagpath,        %% econfd:tagpath()
          namespace,      %% econfd:namespace() (top-level namespace)
          type,           %% undefined | econfd:type()
          primitive_type, %% undefined | integer() (?C_INT8 etc)
          default,        %% undefined | econfd:value()
          min_occurs = 1, %% integer()
          max_occurs = 1, %% integer() | unbounded
          children = [],  %% [econfd:qtag()|#confd_cs_choice{}]
          keys = [],      %% [econfd:tag()] (key children)
          flags,          %% integer()  (?CONFD_CS_IS_XXX/?CONFD_CS_HAS_XXX)
          actions = [],   %% [econfd:qtag()] (action/rpc children)
          cmp = 0,        %% integer() (?CONFD_CS_CMP_XXX)
          notifs = [],    %% [econfd:qtag()] (notification children)
          meta_data = []  %% [{binary(), binary()|undefined}]
         }).

-record(confd_cs_choice, {
          name,           %% econfd:qtag()
          cases = [],     %% [#confd_cs_case{}]
          default = '',   %% '' | econfd:qtag() (default case, '' if no default)
          min_occurs = 0  %% 0 | 1 (1 means mandatory)
         }).

-record(confd_cs_case, {
          name,           %% econfd:qtag()
          children = []   %% [econfd:qtag()]
         }).

-define(CONFD_LEVEL_ERROR, 2).
-define(CONFD_LEVEL_INFO,  3).
-define(CONFD_LEVEL_TRACE, 4).

%%%%%% utility macros
-ifndef(liof).

-define(liof(Fmt, Args), io:format(user, "~w:~w " ++ Fmt,[?MODULE,?LINE|Args])).
-define(liof_bt(Fmt, Args), io:format(user, "~w:~w ~s ~p\n",
                             [?MODULE, ?LINE,
                              io_lib:format(Fmt, Args), ?stack()])).
-endif.
