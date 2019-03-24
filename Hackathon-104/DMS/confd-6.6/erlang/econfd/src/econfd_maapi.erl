%%%-------------------------------------------------------------------
%%% @copyright 2006 Tail-F Systems AB
%%% @version {$Id$}
%%% @doc A n Erlang interface equivalent to the MAAPI C-API
%%%
%%% This modules implements the Management Agent API. All functions in
%%% this module have an equivalent function in the C library.  The
%%% actual semantics of each of the API functions described here is
%%% better described in the man page confd_lib_maapi(3).
%%% @end
%%%-------------------------------------------------------------------

-module(econfd_maapi).

-include("econfd_cdb_api.hrl").
-include("econfd_internal.hrl").
-include("../include/econfd.hrl").
-include("../include/econfd_errors.hrl").
-undef(MAAPI_UPGRADE_KILL_ON_TIMEOUT).
-undef(MAAPI_FLAG_HINT_BULK).
-undef(MAAPI_FLAG_NO_DEFAULTS).
-undef(MAAPI_FLAG_CONFIG_ONLY).
-undef(MAAPI_FLAG_HIDE_INACTIVE).
-undef(MAAPI_FLAG_DELAYED_WHEN).
-undef(MAAPI_FLAG_RUN_SET_HOOKS).
-include("econfd_maapi_proto.hrl").

-import(econfd_cdb, [parse_keystring/1]).


-export([connect/2,
         start_user_session/6,
         start_user_session/7,
         start_user_session/8,
         close/1,
         end_user_session/1,
         get_running_db_status/1,
         set_running_db_status/2,
         kill_user_session/2,
         get_my_user_session_id/1,
         install_crypto_keys/1,
         attach/3,
         attach2/4,
         attach_init/1,
         detach/2,
         authenticate/4,
         authenticate2/8,
         get_user_sessions/1,
         set_user_session/2,
         get_user_session/2,
         lock/2,
         unlock/2,
         is_lock_set/2,
         lock_partial/3,
         unlock_partial/2,
         candidate_validate/1,
         delete_config/2,
         candidate_commit/1,
         candidate_commit/2,
         candidate_commit_info/3,
         candidate_commit_info/4,
         candidate_abort_commit/1,
         candidate_abort_commit/2,
         confirmed_commit_in_progress/1,
         candidate_confirmed_commit/2,
         candidate_confirmed_commit/4,
         candidate_confirmed_commit_info/4,
         candidate_confirmed_commit_info/6,
         candidate_reset/1,
         copy_running_to_startup/1,
         is_running_modified/1,
         is_candidate_modified/1,
         start_trans/3,
         start_trans/4,
         start_trans/5,
         start_trans_in_trans/4,
         start_trans_in_trans/5,
         set_flags/3,
         set_delayed_when/3,
         set_label/3,
         set_comment/3,
         finish_trans/2,
         apply_trans/3,
         apply_trans/4,
         validate_trans/4,
         prepare_trans/2,
         prepare_trans/3,
         abort_trans/2,
         commit_trans/2,
         list_rollbacks/1,
         load_rollback/3,
         copy/3,
         exists/3,
         num_instances/3,
         create/3,
         shared_create/3,
         shared_create/4,
         delete/3,
         get_elem/3,
         get_object/3,
         get_objects/2,
         get_values/4,
         get_elem_no_defaults/3,
         set_elem/4,
         set_elem2/4,
         shared_set_elem/4,
         shared_set_elem2/4,
         set_object/4,
         set_values/4,
         shared_set_values/5,
         insert/3,
         move/4,
         move_ordered/4,
         copy_tree/4,
         init_cursor/3,
         get_next/1,
         all_keys/3,
         find_next/3,
         get_case/4,
         get_attrs/4,
         set_attr/5,
         revert/2,
         diff_iterate/4,
         diff_iterate/5,
         keypath_diff_iterate/5,
         keypath_diff_iterate/6,
         iterate/6,
         xpath_eval/7,
         xpath_eval/6,
         xpath_eval_expr/5,
         xpath_eval_expr/4,
         hkeypath2ikeypath/2,
         request_action/3,
         set_readonly_mode/2,
         init_upgrade/3,
         perform_upgrade/2,
         commit_upgrade/1,
         abort_upgrade/1,
         aaa_reload/2,
         snmpa_reload/2,
         start_phase/3,
         wait_start/1,
         wait_start/2,
         reload_config/1,
         stop/1,
         stop/2,
         ncs_apply_template/8,
         ncs_templates/1,
         ncs_template_variables/3,
         ncs_write_service_log_entry/5
        ]).

-export([cli_prompt/4, cli_prompt/5,
         cli_read_eof/3, cli_read_eof/4,
         cli_prompt_oneof/4, cli_prompt_oneof/5,
         cli_write/3]).

-export([sys_message/3, prio_message/3, user_message/4]).

%%% Private exports
-export([ncs_commit_save_trans/5,
         ncs_apply_reverse_diffset/3,
         ncs_update_plan_history/7,
         ncs_commit_save_no_diffset/5]).

%%% Internal exports
-export([iterate_loop/3]).       % Used by econfd_cdb:diff_iter/5

%%% types

-type err() :: {'error', {integer(), binary()}} | {'error', 'closed'}.
%% @type err() = {error, {integer(), binary()}} |
%%               {error, closed}.
%% Errors can be either
%% <ul><li> {error, Ecode::integer(), Reason::binary()} where Ecode is
%% one of the error codes defined in econfd_errors.hrl, and Reason is
%% (possibly empty) textual description </li>
%% <li> {error, closed} if the socket gets closed </li></ul>

%%%--------------------------------------------------------------------
%%% External functions
%%%--------------------------------------------------------------------

%% @spec connect(Address::ip(), Port::integer()) ->
%%   {ok, Socket::term()} | {error, R::term()}
%% @doc Connect a maapi socket to ConfD
connect(Address, Port) ->
    case econfd_internal:connect(Address, Port, ?CLIENT_MAAPI, []) of
        {ok, Socket} ->
            {ok, Socket};
        Error ->
            Error
    end.

%% @spec start_user_session(Socket::term(), UserName::binary(),
%%                          Context::binary(),
%%                          Groups::[binary()], SrcIp::ip(), Proto::integer())
%% -> ok | err()
%% @equiv start_user_session(Socket, UserName, Context, Groups, SrcIp, 0, Proto)
start_user_session(Socket, UserName, Context, Groups, Ip, Proto) ->
    start_user_session(Socket, UserName, Context, Groups, Ip, 0, Proto).

%% @spec start_user_session(Socket::term(), UserName::binary(),
%%                          Context::binary(),
%%                          Groups::[binary()], SrcIp::ip(), SrcPort::integer(),
%%                          Proto::integer())
%% -> ok | err()
%% @equiv start_user_session(Socket, UserName, Context, Groups, SrcIp,
%%                           0, Proto, undefined)
start_user_session(Socket, UserName, Context, Groups, Ip, Port, Proto) ->
    R = [UserName, {Ip, Port}, ?b2a(Context), Proto, 1 | Groups],
    intcall(Socket, ?MAAPI_START_USER_SESSION, -1, R).

%% @spec start_user_session(Socket::term(), UserName::binary(),
%%                          Context::binary(),
%%                          Groups::[binary()], SrcIp::ip(), SrcPort::integer(),
%%                          Proto::integer(),
%%                          undefined | #confd_user_identification{})
%% -> ok | err()
%% @doc Initiate a new maapi user session.
%% returns a maapi session id. Before we can execute any maapi functions
%% we must always have an associated user session.
start_user_session(Socket, UserName, Context, Groups, Ip, Port, Proto, UId) ->
    R = {UserName, {Ip, Port}, ?b2a(Context), Proto, _UseIKp = true, Groups,
         mk_uident(UId)},
    intcall(Socket, ?MAAPI_START_USER_SESSION, -1, R).

%% @spec close(Socket::term()) -> ok | {error, R::term()}
%% @doc Close socket.
close(Socket) ->
    %% Don't end the user session here, it might be one we got via
    %% attach()/set_user_session()/start_trans()
    %% end_user_session(Socket),
    econfd_internal:close(Socket).

%% @spec end_user_session(Sock::port()) -> ok | err()
%% @doc Ends a user session.
end_user_session(Sock) ->
    intcall(Sock, ?MAAPI_END_USER_SESSION, -1, <<>>).

%% @spec get_running_db_status(Sock::port()) -> {ok, 1}|{ok,0}|err()
%% @doc Get the "running status"
get_running_db_status(Sock) ->
    intcall(Sock, ?MAAPI_GET_RUNNING_DB_STATUS, -1, <<>>).

%% @spec set_running_db_status(Sock::port(), 1|0) -> ok | err()
%% @doc Set the "running status"
set_running_db_status(Sock, Status) when is_integer(Status) ->
    intcall(Sock, ?MAAPI_SET_RUNNING_DB_STATUS, -1, Status).

%% @spec kill_user_session(Sock::port(), UsessID::integer()) -> ok | err()
%% @doc Kill a user session
kill_user_session(Sock, UsessID) ->
    intcall(Sock, ?MAAPI_KILL_USER_SESSION, -1, UsessID).

%% @spec get_my_user_session_id(Sock::port()) -> {ok, Id::integer()} | err()
%% @doc Get my user session id.
get_my_user_session_id(Sock) ->
    intcall(Sock, ?MAAPI_GET_MY_USER_SESSION, -1, <<>>).

-spec install_crypto_keys(Sock::port()) -> ok | err().
%% @doc Fetch keys for the data types tailf:des3-cbc-encrypted-string
%% and tailf:aes-cfb-128-encrypted-string from the server.
install_crypto_keys(Sock) ->
    case intcall(Sock, ?MAAPI_GET_CRYPTO_KEYS, -1, <<>>) of
        {ok, {DesKey1, DesKey2, DesKey3, DesIVec, AesKey, AesIVec}} ->
            if is_binary(DesKey1) ->
                    ets:insert(confd_installed_crypto_keys,
                               {des3, DesKey1, DesKey2, DesKey3, DesIVec});
               true ->
                    ok
            end,
            if is_binary(AesKey) ->
                    ets:insert(confd_installed_crypto_keys,
                               {aes128, AesKey, AesIVec});
               true ->
                    ok
            end,
            ok;
        Err ->
            Err
    end.

%% @spec attach(Sock::port(), NameSpace, Tctx::#confd_trans_ctx{}) ->
%% ok | err()
%% NameSpace = atom() | 0
%% @doc Attach to a running transaction. Give NameSpace as 0 if
%% it doesn't matter (-1 works too but is deprecated).
attach(Sock, NameSpace, Tctx) ->
    attach2(Sock, NameSpace, (Tctx#confd_trans_ctx.uinfo)#confd_user_info.usid,
            Tctx#confd_trans_ctx.thandle).

%% @spec attach2(Sock::port(), NameSpace, Usid::integer(),
%%               Thandle::integer()) -> ok | err()
%% NameSpace = atom() | 0
%% @doc  Attach to a running transaction. Give NameSpace as 0 if
%% it doesn't matter (-1 works too but is deprecated).
attach2(_Sock, _NameSpace, _Usid, -1) ->
    {error, {?CONFD_ERR_NOEXISTS, <<"-1 is an invalid transaction handle">>}};
attach2(Sock, NameSpace, Usid, Thandle) ->
    R = {NameSpace, Usid, useikp},
    intcall(Sock, ?MAAPI_ATTACH, Thandle, R).

%% @spec attach_init(Sock::port()) ->
%% {ok, Thandle::integer()} | err()
%% @doc Attach to the CDB init/upgrade transaction in phase0. Returns
%% the transaction handle to use in subsequent maapi calls on success.
attach_init(Sock) ->
    InitTh = -2,
    case attach2(Sock, 0, -2, InitTh) of
        ok -> {ok, InitTh};
        Error -> Error
    end.

%% @spec detach(Sock::port(), Thandle::integer()) -> ok | err()
%% @doc Detach from the transaction.
detach(Sock, Thandle) ->
    intcall(Sock, ?MAAPI_DETACH, Thandle, <<>>).

%% @spec authenticate(Sock::port(), User::binary(), Pass::binary(),
%%                    Groups::[binary()]) ->  ok | err()
%% @doc Autenticate a user using ConfD AAA
authenticate(Sock, User, Pass, _Groups) ->
    R = {User, Pass},
    intcall(Sock, ? MAAPI_AUTHENTICATE,-1, R).

%% @spec authenticate2(Sock::port(), User::binary(), Pass::binary(),
%%                     SrcAddr::ip(), SrcPort::integer(),
%%                     Context::binary(), Proto::integer(),
%%                     Groups::[binary()]) ->  ok | err()
%% @doc Autenticate a user using ConfD AAA
authenticate2(Sock, User, Pass, SrcAddr, SrcPort, Context, Proto, _Groups) ->
    R = {User, Pass, SrcAddr, SrcPort, Context, Proto},
    intcall(Sock, ? MAAPI_AUTHENTICATE,-1, R).

%% @spec get_user_sessions(Sock::port()) -> {ok, [Usid::integer()]} | err()
%% @doc get all user sessions
get_user_sessions(Sock) ->
    intcall(Sock, ?MAAPI_GET_USER_SESSIONS, -1, <<>>).

%% @spec set_user_session(Sock::port(), Usid::integer()) -> ok | err()
%% @doc Assign a user session
set_user_session(Sock, USid) ->
    intcall(Sock, ?MAAPI_SET_USER_SESSION, -1, {USid, useikp}).


%% @spec get_user_session(Sock::port(), UsessId::integer()) ->
%% {ok, #confd_user_info{}} | err()
%% @doc Get session info for a user session.
get_user_session(Sock, UsessId) ->
    case intcall(Sock, ?MAAPI_GET_USER_SESSION, -1, UsessId) of
        {ok, {Utuple, LockMode}} ->
            U = econfd:mk_uinfo(Utuple),
            {ok, U#confd_user_info{lockmode = LockMode}};
        Err ->
            Err
    end.

%% @spec lock(Sock::integer(), DbName::integer()) -> ok | err()
%% @doc Lock a database
lock(Sock, DbName) ->
    intcall(Sock, ?MAAPI_LOCK, -1, DbName).

%% @spec unlock(Sock::integer(), DbName::integer()) -> ok | err()
%% @doc Unlock a database
unlock(Sock, DbName) ->
    intcall(Sock, ?MAAPI_UNLOCK, -1, DbName).

%% @spec is_lock_set(Sock::integer(), DbName::integer()) -> {ok, integer()} |
%%                                                           err()
%% @doc Check if a db is locked or not. Return 0 or the Usid of the lock owner.
is_lock_set(Sock, DbName) ->
    intcall(Sock, ?MAAPI_IS_LOCK_SET, -1, DbName).

%% @spec lock_partial(Sock::integer(), DbName::integer(), [XPath::binary()]) ->
%% {ok, LockId::integer()} | err()
%% @doc Request a partial lock on a database - the set of nodes to lock
%% is specified as a list of XPath expressions.
lock_partial(Sock, DbName, XPathList) ->
    intcall(Sock, ?MAAPI_LOCK_PARTIAL, -1, {DbName, XPathList}).

%% @spec unlock_partial(Sock::integer(), LockId::integer())
%% -> ok | err()
%% @doc Remove the partial lock identified by LockId.
unlock_partial(Sock, LockId) ->
    intcall(Sock, ?MAAPI_UNLOCK_PARTIAL, -1, LockId).

%% @spec candidate_validate(Sock::port()) -> ok | err()
%% @doc Validate the candidate config
candidate_validate(Sock) ->
    intcall(Sock, ?MAAPI_CANDIDATE_VALIDATE, -1, <<>>).

%% @spec delete_config(Sock::port(), DbName::integer()) -> ok | err()
%% @doc Delete all data from a data store
delete_config(Sock, DbName) ->
    intcall(Sock, ?MAAPI_DELETE_CONFIG, -1, DbName).

-spec candidate_commit(Sock::econfd:socket()) ->
                              ok | err().
%% @doc Copies candidate to running or confirms a confirmed commit
candidate_commit(Sock) ->
    candidate_commit_info(Sock, undefined, <<>>, <<>>).

-spec candidate_commit(Sock::econfd:socket(), PersistId::binary()) ->
                              ok | err().
%% @doc Confirms persistent confirmed commit
candidate_commit(Sock, PersistId) ->
    candidate_commit_info(Sock, PersistId, <<>>, <<>>).

-spec candidate_commit_info(Sock::econfd:socket(),
                            Label::binary(), Comment::binary()) ->
                                   ok | err().
%% @doc Like {@link candidate_commit/1}, but set the "Label" and/or "Comment"
%% that is stored in the rollback file when the candidate is committed
%% to running. To set only the "Label", give Comment as an empty binary,
%% and to set only the "Comment", give Label as an empty binary.
%%
%% Note: To ensure that the "Label" and/or "Comment" are stored
%% in the rollback file in all cases when doing a confirmed commit,
%% they must be given both with the confirmed commit (using
%% {@link candidate_confirmed_commit_info/4}) and
%% with the confirming commit (using this function).
candidate_commit_info(Sock, Label, Comment) ->
    candidate_commit_info(Sock, undefined, Label, Comment).

-spec candidate_commit_info(Sock::econfd:socket(),
                            PersistId::binary() | 'undefined',
                            Label::binary(), Comment::binary()) ->
                                   ok | err().
%% @doc Combines {@link candidate_commit/2} and {@link candidate_commit_info/3}
%% - set "Label" and/or "Comment" when confirming a persistent confirmed commit.
%%
%% Note: To ensure that the "Label" and/or "Comment" are stored
%% in the rollback file in all cases when doing a confirmed commit,
%% they must be given both with the confirmed commit (using
%% {@link candidate_confirmed_commit_info/6}) and
%% with the confirming commit (using this function).
candidate_commit_info(Sock, PersistId, Label, Comment) ->
    intcall(Sock,  ?MAAPI_CANDIDATE_COMMIT, -1, {PersistId, Label, Comment}).

-spec candidate_confirmed_commit(Sock::econfd:socket(),
                                 TimeoutSecs::integer()) ->
                                        ok | err().
%% @doc Copy candidate into running, but rollback if not confirmed by a
%% call of {@link candidate_commit/1}.
candidate_confirmed_commit(Sock, TimeoutSecs) ->
    candidate_confirmed_commit_info(Sock, TimeoutSecs, undefined, undefined,
                                    <<>>, <<>>).

-spec candidate_confirmed_commit(Sock::econfd:socket(),
                                 TimeoutSecs::integer(),
                                 Persist :: binary() | 'undefined',
                                 PersistId :: binary() | 'undefined') ->
                                        ok | err().
%% @doc Starts or extends persistent confirmed commit
candidate_confirmed_commit(Sock, TimeoutSecs, Persist, PersistId) ->
    candidate_confirmed_commit_info(Sock, TimeoutSecs, Persist, PersistId,
                                    <<>>, <<>>).

-spec candidate_confirmed_commit_info(Sock::econfd:socket(),
                                      TimeoutSecs::integer(),
                                      Label::binary(), Comment::binary()) ->
                                             ok | err().
%% @doc Like {@link candidate_confirmed_commit/2}, but set the "Label"
%% and/or "Comment" that is stored in the rollback file when the
%% candidate is committed to running. To set only the "Label", give
%% Comment as an empty binary, and to set only the "Comment", give Label
%% as an empty binary.
%%
%% Note: To ensure that the "Label" and/or "Comment" are stored in the
%% rollback file in all cases when doing a confirmed commit, they must
%% be given both with the confirmed commit (using this function) and
%% with the confirming commit (using {@link candidate_commit_info/3}).
candidate_confirmed_commit_info(Sock, TimeoutSecs, Label, Comment) ->
    candidate_confirmed_commit_info(Sock, TimeoutSecs, undefined, undefined,
                                    Label, Comment).

-spec candidate_confirmed_commit_info(Sock::econfd:socket(),
                                      TimeoutSecs::integer(),
                                      Persist :: binary() | 'undefined',
                                      PersistId :: binary() | 'undefined',
                                      Label::binary(), Comment::binary()) ->
                                             ok | err().
%% @doc Combines {@link candidate_confirmed_commit/4} and {@link
%% candidate_confirmed_commit_info/4} - set "Label" and/or "Comment"
%% when starting or extending a persistent confirmed commit.
%%
%% Note: To ensure that the "Label" and/or "Comment" are stored
%% in the rollback file in all cases when doing a confirmed commit,
%% they must be given both with the confirmed commit (using this function)
%% and with the confirming commit (using {@link candidate_commit_info/4}).
candidate_confirmed_commit_info(Sock, TimeoutSecs, Persist, PersistId,
                                Label, Comment) ->
    intcall(Sock, ?MAAPI_CANDIDATE_CONFIRMED_COMMIT,  -1,
            {TimeoutSecs, Persist, PersistId, Label, Comment}).

%% @spec candidate_abort_commit(Sock::port()) -> ok | err()
%% @doc Cancel a pending confirmed commit
candidate_abort_commit(Sock) ->
    intcall(Sock,  ?MAAPI_CANDIDATE_ABORT_COMMIT, -1, <<>>).

%% @spec candidate_abort_commit(Sock::port(), PersistId::binary()) -> ok | err()
%% @doc Cancel persistent confirmed commit
candidate_abort_commit(Sock, PersistId) ->
    intcall(Sock,  ?MAAPI_CANDIDATE_ABORT_COMMIT, -1, PersistId).

%% @spec confirmed_commit_in_progress(Sock::port()) -> {ok, bool()} | err()
%% @doc Is a confirmed commit in progress.
confirmed_commit_in_progress(Sock) ->
    ibool(intcall(Sock,  ?MAAPI_CONFIRMED_COMMIT_IN_PROGRESS, -1, <<>>)).

%% @spec candidate_reset(Sock::port()) ->  ok | err()
%% @doc Copy running into candidate.
candidate_reset(Sock) ->
    intcall(Sock, ?MAAPI_CANDIDATE_RESET, -1, <<>>).

%% @spec copy_running_to_startup(Sock::port()) -> ok | err()
%% @doc Copy running to startup.
copy_running_to_startup(Sock) ->
    intcall(Sock, ?MAAPI_COPY_RUNNING_TO_STARTUP, -1, <<>>).

%% @spec is_running_modified(Sock::port()) -> {ok, bool()} | err()
%% @doc Check if running has been modified since
%% the last copy to startup was done.
is_running_modified(Sock) ->
    ibool(intcall(Sock, ?MAAPI_IS_RUNNING_MODIFIED, -1, <<>>)).

%% @spec is_candidate_modified(Sock::port()) -> {ok, bool()} | err()
%% @doc Check if candidate has been modified.
is_candidate_modified(Sock) ->
    ibool(intcall(Sock, ?MAAPI_IS_CANDIDATE_MODIFIED, -1, <<>>)).

%% @spec start_trans(Sock::port(), DbName::integer(), RwMode::integer()) ->
%% {ok,Tid::integer()} | err()
%% @doc Start a new transaction
start_trans(Sock, DbName, RwMode) ->
    start_trans(Sock, DbName, RwMode, 0, 0, undefined).

%% @spec start_trans(Sock::port(), DbName::integer(), RwMode::integer(),
%%                   Usid::integer()) ->
%% {ok,Tid::integer()} | err()
%% @doc Start a new transaction within an existing user session
start_trans(Sock, DbName, RwMode, Usid) ->
    start_trans(Sock, DbName, RwMode, Usid, 0, undefined).

%% @spec start_trans(Sock::port(), DbName::integer(), RwMode::integer(),
%%                   Usid::integer(), Flags::integer()) ->
%% {ok,Tid::integer()} | err()
%% @doc Start a new transaction within an existing user session and/or
%% with flags. See ?MAAPI_FLAG_XXX in econfd.hrl for the available flags.
%% To use the existing user session of the socket, give Usid = 0.
start_trans(Sock, DbName, RwMode, Usid, Flags) ->
    start_trans(Sock, DbName, RwMode, Usid, Flags, undefined).

start_trans(Sock, DbName, RwMode, Usid, Flags, UId) ->
    intcall(Sock, ?MAAPI_START_TRANS, -1,
            {DbName, RwMode, Usid, 1, Flags, mk_uident(UId)}).

%% @spec start_trans_in_trans(Sock::port(), RwMode::integer(),
%%                   Usid::integer(), Tid::integer()) ->
%% {ok,Tid::integer()} | err()
%% @doc Start a new transaction with an existing transaction as backend.
%% To use the existing user session of the socket, give Usid = 0.
start_trans_in_trans(Sock, RwMode, Usid, Tid) ->
    intcall(Sock, ?MAAPI_START_TRANS, -1,
            {trintr, RwMode, Usid, 1, Tid}).

%% @spec start_trans_in_trans(Sock::port(), RwMode::integer(),
%%                   Usid::integer(), Tid::integer(), Flags::integer()) ->
%% {ok,Tid::integer()} | err()
%% @doc Start a new transaction with an existing transaction as backend.
%% To use the existing user session of the socket, give Usid = 0.
start_trans_in_trans(Sock, RwMode, Usid, Tid, Flags) ->
    intcall(Sock, ?MAAPI_START_TRANS, -1,
            {trintr, RwMode, Usid, 1, Tid, Flags}).

%% @spec set_flags(Sock::port(), Tid::integer(), Flags::integer()) -> ok | err()
%% @doc Change flag settings for a transaction. See ?MAAPI_FLAG_XXX in
%% econfd.hrl for the available flags, however ?MAAPI_FLAG_HIDE_INACTIVE
%% and ?MAAPI_FLAG_DELAYED_WHEN cannot be changed after transaction start
%% (but see set_delayed_when/3).
set_flags(Sock, Tid, Flags) ->
    intcall(Sock, ?MAAPI_SET_FLAGS, Tid, Flags).

%% @spec set_delayed_when(Sock::port(), Tid::integer(), Value::boolean()) ->
%%          {ok, OldValue::boolean()} | err()
%% @doc Enable/disable the "delayed when" mode for a transaction. Returns
%% the old setting on success.
set_delayed_when(Sock, Tid, Value) ->
    ibool(intcall(Sock, ?MAAPI_SET_DELAYED_WHEN, Tid, bool2int(Value))).

-spec set_label(Sock::econfd:socket(), Tid::integer(), Label::binary()) ->
                       ok | err().
%% @doc Set the "Label" that is stored in the rollback file when a
%% transaction towards running is committed.
set_label(Sock, Tid, Label) ->
    intcall(Sock, ?MAAPI_SET_LABEL, Tid, Label).

-spec set_comment(Sock::econfd:socket(), Tid::integer(), Comment::binary()) ->
                       ok | err().
%% @doc Set the "Comment" that is stored in the rollback file when a
%% transaction towards running is committed.
set_comment(Sock, Tid, Comment) ->
    intcall(Sock, ?MAAPI_SET_COMMENT, Tid, Comment).

%% @spec finish_trans(Sock::port(), Tid::integer()) -> ok | err()
%% @doc Finish a transaction
finish_trans(Sock, Tid) ->
    intcall(Sock, ?MAAPI_STOP_TRANS, Tid, <<>>).

%% @spec apply_trans(Sock::port(), Tid::integer(), KeepOpen::bool()) ->
%%             ok | err()
%% @doc Apply all in the transaction.
%% This is the combination of validate/prepare/commit done in the
%% right order.
apply_trans(Sock, Tid, KeepOpen) ->
    apply_trans(Sock, Tid, KeepOpen, 0).

apply_trans(Sock, Tid, KeepOpen, Flags) ->
    intcall(Sock, ?MAAPI_APPLY_TRANS, Tid, {KeepOpen, Flags}).

%% @spec validate_trans(Sock::port(), Tid::integer(),
%%                      UnLock::bool(), ForceValidation::bool()) ->
%% ok | err()
%% @doc Validate the transaction.
validate_trans(Sock, Tid, UnLock, ForceValidation) ->
    intcall(Sock, ?MAAPI_VALIDATE_TRANS,Tid, {UnLock, ForceValidation}).

%% @spec prepare_trans(Sock::port(), Tid::integer()) -> ok | err()
%% @doc Prepare for commit
prepare_trans(Sock, Tid) ->
    prepare_trans(Sock, Tid, 0).

%% @spec prepare_trans(Sock::port(), Tid::integer(), Flags::integer()) ->
%%   ok | err()
%% @doc Prepare for commit
prepare_trans(Sock, Tid, Flags) ->
    intcall(Sock, ?MAAPI_PREPARE_TRANS, Tid, Flags).

%% @spec abort_trans(Sock::port(), Tid::integer()) -> ok | err()
%% @doc Abort transaction
abort_trans(Sock, Tid) ->
    intcall(Sock, ?MAAPI_ABORT_TRANS, Tid,<<>>).

%% @spec commit_trans(Sock::port(), Tid::integer()) -> ok | err()
%% @doc Commit a transaction
commit_trans(Sock, Tid) ->
    intcall(Sock, ?MAAPI_COMMIT_TRANS, Tid,<<>>).

-spec list_rollbacks(Sock::econfd:socket()) ->
                            {ok, [#maapi_rollback{}]} | err().
%% @doc Get a list of available rollback files.
list_rollbacks(Sock) ->
    case intcall(Sock, ?MAAPI_LIST_ROLLBACK, -1, <<>>) of
        {ok, Rollbacks} ->
            F = fun ({Nr, Creator, Date, Via, FixedNr, Label, Comment}) ->
                        #maapi_rollback{nr = Nr, creator = Creator, date = Date,
                                        via = Via, fixed_nr = FixedNr,
                                        label = Label, comment = Comment};
                    ({Nr, Creator, Date, Via, FixedNr}) ->
                        %% old-style
                        #maapi_rollback{nr = Nr, creator = Creator, date = Date,
                                        via = Via, fixed_nr = FixedNr,
                                        label = <<>>, comment = <<>>}
                end,
            {ok, [F(Rollback) || Rollback <- Rollbacks]};
        Err ->
            Err
    end.

-spec load_rollback(Sock::econfd:socket(), Tid::integer(),
                    RollbackNr::integer()) ->
                           ok | err().
%% @doc Load a rollback file
load_rollback(Sock, Tid, RollbackNr) ->
    intcall(Sock, ?MAAPI_LOAD_ROLLBACK, Tid, RollbackNr).

%% @spec copy(Sock::port(), FromTH, ToTH) -> ok | err()
%% @doc Copy data from pne transaction to another.
copy(Sock, FromTH, ToTH) ->
    intcall(Sock, ?MAAPI_COPY, FromTH, ToTH).

-spec exists(Sock::econfd:socket(), Tid::integer(), IKP::econfd:ikeypath()) ->
                    {ok, boolean()} | err().
%% @doc  Check if an element exists
exists(Sock, Tid, IKP) ->
    ibool(intcall(Sock, ?MAAPI_EXISTS, Tid, reverse(IKP))).

%% @spec num_instances(Sock::port(), Tid::integer(), IKP::econfd:ikeypath())
%% -> {ok, integer()} | err()
%% @doc  Find the number of entries in a list
num_instances(Sock, Tid, IKP) ->
    intcall(Sock, ?MAAPI_NUM_INSTANCES, Tid, reverse(IKP)).

%% @spec create(Sock::port(), Tid::integer(), IKP::econfd:ikeypath()) ->
%%  ok | err()
%% @doc Create a new element
create(Sock, Tid, IKP) ->
    intcall(Sock, ?MAAPI_CREATE, Tid, reverse(IKP)).


%% @spec shared_create(Sock::port(), Tid::integer(),
%%       IKP::econfd:ikeypath()) ->
%%  ok | err()
%% @doc invoke shared_create(Sock, Tid, true, IKP).

shared_create(Sock, Tid, IKP) ->
    shared_create(Sock, Tid, true, IKP).


%% @spec shared_create(Sock::port(), Tid::integer(), BackP::boolean(),
%%       IKP::econfd:ikeypath()) ->
%%  ok | err()
%% @doc Create a new element, and also set an attribute indicating
%% how many times this element has been created. Update Back Pointer
%% if BackP is true. Used by NCS Fastmap code.
shared_create(Sock, Tid, BackP, IKP) ->
    intcall(Sock, ?MAAPI_NCS_SHARED_CREATE, Tid, {BackP, reverse(IKP)}).

%% @spec delete(Sock::port(), Tid::integer(), IKP::econfd:ikeypath()) ->
%%   ok | err()
%% @doc  Delete an element
delete(Sock, Tid, IKP) ->
    intcall(Sock, ?MAAPI_DELETE, Tid, reverse(IKP)).

%% @spec get_elem(Sock::port(), Tid::integer(), IKP::econfd:ikeypath()) ->
%%   {ok, V::econfd:value()} | err()
%% @doc Read an element
get_elem(Sock, Tid, IKP) ->
    intcall(Sock, ?MAAPI_GET_ELEM, Tid, reverse(IKP)).

%% @spec get_object(Sock::port(), Tid::integer(), IKP::econfd:ikeypath()) ->
%%              {ok, [econfd:value()]} | {error, err()}
%% @doc Read all the values in a container or list entry.
get_object(Sock, Tid, IKP) ->
    intcall(Sock, ?MAAPI_GET_OBJECT, Tid, reverse(IKP)).

%% @spec get_objects(C::#maapi_cursor{}, NumEntries::integer()) ->
%% {ok, C2::#maapi_cursor{}, Values::[[econfd:value()]]} |
%%   {done, Values::[[econfd:value()]]} |
%%   err()
%% @doc Read all the values for NumEntries list entries,
%% starting at the point given by the cursor C. The return value has one
%% Erlang list for each YANG list entry, i.e. it is a list of at most
%% NumEntries lists. If we reached the end of the YANG list,
%% {done, Values} is returned, and there will be fewer than NumEntries
%% lists in Values - otherwise {ok, C2, Values} is returned, where C2
%% can be used to continue the traversal.
get_objects(C, NumEntries) ->
    R = {C#maapi_cursor.prevterm, C#maapi_cursor.ikp,
         C#maapi_cursor.cursor_id, C#maapi_cursor.secondary_index, NumEntries},
    case intcall(C#maapi_cursor.socket, ?MAAPI_GET_OBJECTS,
                 C#maapi_cursor.thandle, R) of
        {ok, {false, Values}} ->
            %% we're done
            {done, Values};
        {ok, {Res, Values}} ->
            {ok, C#maapi_cursor{prevterm = Res}, Values};
        Err ->
            Err
    end.

%% @spec get_values(Sock::port(), Tid::integer(), IKP::econfd:ikeypath(),
%%                  Values::[econfd:tagval()]) ->
%%              {ok, [econfd:tagval()]} | err()
%% @doc Read the values for the leafs that have the "value" 'not_found'
%% in the Values list. This can be used to read an arbitrary set of
%% sub-elements of a container or list entry. The return value is a list
%% of the same length as Values, i.e. the requested leafs are in the same
%% position in the returned list as in the Values argument. The elements
%% in the returned list are always "canonical" though, i.e. of the form
%% {[Ns|Tag], value() | start | stop | leaf}.
get_values(Sock, Tid, IKP, Values) ->
    intcall(Sock, ?MAAPI_GET_VALUES, Tid, {Values, reverse(IKP)}).


%% @spec get_elem_no_defaults(Sock::port(), Tid::integer(),
%%                            IKP::econfd:ikeypath()) ->
%%   {ok, V::econfd:value()} | err()
%% @doc Read an element, but return 'default' instead of the value if
%% the default value is in effect.
%% @deprecated Use set_flags/3 with ?MAAPI_FLAG_NO_DEFAULTS instead - this
%% will take effect for all the functions that read values.
get_elem_no_defaults(Sock, Tid, IKP) ->
    intcall(Sock, ?MAAPI_GET_ELEM_NO_DEFAULT, Tid, reverse(IKP)).


%% @spec set_elem(Sock::port(), Tid::integer(),
%%                  IKP::econfd:ikeypath(), Val::econfd:value()) -> ok | err()
%% @doc Write an element
set_elem(Sock, Tid, IKP, Val) ->
    intcall(Sock, ?MAAPI_SET_ELEM, Tid, {Val,reverse(IKP)}).

%% @spec set_elem2(Sock::port(), Tid::integer(),
%%                   IKP::econfd:ikeypath(), Val::binary()) -> ok | err()
%% @doc Write an element using the textual value representation.
set_elem2(Sock, Tid, IKP, Val) ->
    intcall(Sock, ?MAAPI_SET_ELEM2, Tid, {Val,reverse(IKP)}).


%% @spec shared_set_elem(Sock::port(), Tid::integer(),
%%                       IKP::econfd:ikeypath(),
%%                       Val::econfd:value()) -> ok | err()
%% @doc Write an element from NCS FastMap
shared_set_elem(Sock, Tid, IKP, Val) ->
    intcall(Sock, ?MAAPI_SHARED_SET_ELEM, Tid, {Val,reverse(IKP)}).

%% @spec shared_set_elem2(Sock::port(), Tid::integer(),
%%                        IKP::econfd:ikeypath(), Val::binary()) -> ok | err()
%% @doc Write an element using the textual value representation
%%  from NCS fastmap.
shared_set_elem2(Sock, Tid, IKP, Val) ->
    intcall(Sock, ?MAAPI_SHARED_SET_ELEM2, Tid, {Val,reverse(IKP)}).

%% @spec set_object(Sock::port(), Tid::integer(),
%%                 IKP::econfd:ikeypath(), [Val::econfd:value()]) -> ok | err()
%% @doc Write an entire object, i.e. YANG list entry or container.
set_object(Sock, Tid, IKP, ValueList) ->
    intcall(Sock, ?MAAPI_SET_OBJECT, Tid, {ValueList, reverse(IKP)}).

%% @spec set_values(Sock::port(), Tid::integer(),
%%                 IKP::econfd:ikeypath(), [Val::econfd:tagval()]) -> ok | err()
%% @doc Write a list of tagged values.
%% This function is an alternative to
%% set_object/4, and allows for writing more complex structures
%% (e.g. multiple entries in a list).
set_values(Sock, Tid, IKP, ValueList) ->
    intcall(Sock, ?MAAPI_SET_VALUES, Tid, {ValueList, reverse(IKP)}).

%% @spec shared_set_values(Sock::port(), Tid::integer(),
%%                         IKP::econfd:ikeypath(), [Val::econfd:tagval()],
%%                         CreateBackpointer::boolean()) ->
%%    ok | err()
%% @doc Write a list of tagged values from NCS FastMap.
%% Update backpointer if CreateBackpointer is true.
shared_set_values(Sock, Tid, IKP, ValueList, CreateBackpointer) ->
    intcall(Sock, ?MAAPI_SHARED_SET_VALUES, Tid,
            {{ValueList, CreateBackpointer}, reverse(IKP)}).

-spec get_case(Sock::econfd:socket(), Tid::integer(), IKP::econfd:ikeypath(),
               Choice :: econfd:qtag() | [econfd:qtag()]) ->
                      {ok, Case::econfd:qtag()} | err().
%% @doc Get the current case for a choice
get_case(Sock, Tid, IKP, Choice) ->
    intcall(Sock, ?MAAPI_GET_CASE, Tid,
            {econfd_cdb:choice_path(Choice), reverse(IKP)}).

%% @spec get_attrs(Sock::port(), Tid::integer(), IKP::econfd:ikeypath(),
%%                 [Attr::integer()]) ->
%%   {ok, [{Attr::integer(), V::value()}]} | err()
%% @doc Get the selected attributes for an element. Calling with an
%% empty attribute list returns all attributes.
get_attrs(Sock, Tid, IKP, AttrL) ->
    intcall(Sock, ?MAAPI_GET_ATTRS, Tid, {AttrL, reverse(IKP)}).

%% @spec set_attr(Sock::port(), Tid::integer(), IKP::econfd:ikeypath(),
%%                Attr::integer(), Value) ->
%%   ok | err()
%%  Value = value() | undefined
%% @doc Set the an attribute for an element. Value == undefined means
%% that the attribute should be deleted.
set_attr(Sock, Tid, IKP, Attr, Value) ->
    intcall(Sock, ?MAAPI_SET_ATTR, Tid, {{Attr, Value}, reverse(IKP)}).

%% @spec insert(Sock::port(), Tid::integer(), IKP::econfd:ikeypath()) ->
%%    ok | err()
%% @doc Insert an entry in an integer-keyed list
insert(Sock, Tid, IKP) ->
    intcall(Sock, ?MAAPI_INSERT, Tid, reverse(IKP)).

%% @spec move(Sock::port(), Tid::integer(), IKP::econfd:ikeypath(),
%%            ToKey::econfd:key()) -> ok | err()
%% @doc Move (rename) an entry in a list
move(Sock, Tid, IKP, ToKey) ->
    intcall(Sock, ?MAAPI_MOVE, Tid, {reverse(IKP),ToKey}).

%% @spec move_ordered(Sock::port(), Tid::integer(), IKP::econfd:ikeypath(),
%%        'first' | 'last' | {'before'|'after', econfd:key()}) -> ok | err()
%% @doc Move an entry in an "ordered-by user" list
move_ordered(Sock, Tid, IKP, To) ->
    intcall(Sock, ?MAAPI_MOVE_ORDERED, Tid, {reverse(IKP),To}).

%% @spec revert(Sock::port(), Tid::integer()) -> ok | err()
%% @doc Remove all changes in the transaction.
revert(Sock, Tid) ->
    intcall(Sock, ?MAAPI_REVERT, Tid, <<>>).

%% @spec copy_tree(Sock::port(), Tid::integer(),   From::econfd:ikeypath(),
%%                                                 To::econfd:ikeypath()) ->
%%   ok | err()
%% @doc Copy an entire subtree in the configuration from one point to another.
copy_tree(Sock, Tid,  From, To) ->
    intcall(Sock, ?MAAPI_COPY_TREE, Tid, {false, reverse(From), reverse(To)}).



%% @spec all_keys(Sock::port(), Tid::integer(),
%%                IKP::econfd:ikeypath()) -> {ok, [econfd:key()]} | err()
%% @doc Utility function. Return all keys in a list

all_keys(MaapiSock, Th, IKP) ->
    Cursor = econfd_maapi:init_cursor(MaapiSock, Th, IKP),
    all_keys(Cursor, []).
all_keys(Cursor, Acc) ->
    case econfd_maapi:get_next(Cursor) of
        {ok, Key, C2} ->
            all_keys(C2, [Key | Acc]);
        done ->
            {ok, Acc};
        Err ->
            Err
    end.



%% @spec init_cursor(Sock::port(), Tid::integer(),
%%                     IKP::econfd:ikeypath()) -> #maapi_cursor{}
%% @doc Initalize a get_next() cursor
init_cursor(Sock, Tid, IKP) ->
    #maapi_cursor{ikp = reverse(IKP), isrel = false,
                  socket = Sock, thandle = Tid,
                  cursor_id =  erlang:phash(make_ref(), 16#fffffff)}.

%% @spec get_next(C::#maapi_cursor{}) ->
%% {ok, econfd:key(), C2::#maapi_cursor{}} | done | err()
%% @doc iterate through the entries of a list
get_next(C) ->
    BulkHint = 0,
    R = {C#maapi_cursor.prevterm, C#maapi_cursor.ikp,
         C#maapi_cursor.cursor_id, BulkHint, C#maapi_cursor.secondary_index},
    case intcall(C#maapi_cursor.socket, ?MAAPI_GET_NEXT,
                 C#maapi_cursor.thandle, R) of
        {ok, false} ->
            %% we're done
            done;
        {ok, Res} ->
            Keys = element(2, Res),
            {ok, Keys, C#maapi_cursor{prevterm = Res}};
        Err ->
            Err
    end.

%% @spec find_next(C::#maapi_cursor{}, FindNextType::integer(), Key::term()) ->
%% {ok, econfd:key(), C2::#maapi_cursor{}} | done | err()
%% @doc find the list entry matching FindNextType and Key
find_next(C, Type, Key) when Type == ?CONFD_FIND_NEXT;
                             Type == ?CONFD_FIND_SAME_OR_NEXT ->
    BulkHint = 0,
    R = {Key, C#maapi_cursor.ikp, C#maapi_cursor.cursor_id,
         Type, BulkHint, C#maapi_cursor.secondary_index},
    case intcall(C#maapi_cursor.socket, ?MAAPI_FIND_NEXT,
                 C#maapi_cursor.thandle, R) of
        {ok, false} ->
            %% we're done
            done;
        {ok, Res} ->
            Keys = element(2, Res),
            {ok, Keys, C#maapi_cursor{prevterm = Res}};
        Err ->
            Err
    end.


%% @equiv diff_iterate(Sock, Tid, Fun, 0, InitState)
diff_iterate(Sock, Tid, Fun, InitState) ->
    diff_iterate(Sock, Tid, Fun, 0, InitState).

%% @spec diff_iterate(Sock::port(), Tid::integer(),
%%                    Fun::(IKP::econfd:ikeypath(), Op::integer(),
%%                          Oval, Eval,
%%                          State::term()) ->
%%                       {ok, Ret::integer(), State2::term()} | {error, term()},
%%                    Flags::integer(),
%%                    InitState::term()) ->
%%              {ok, State::term()} | {error, term()}
%%    Oval = econfd:value() | undefined
%%    Eval = econfd:value() | undefined | econfd:key() | {}
%% @doc Iterate through a diff.
%% This function is used in combination with the notifications API
%% where we get a chance to iterate through the diff of a transaction
%% just before it gets commited. The transaction hangs until we have called
%% econfd_notif:notification_done/2.
%% The function can also be called from within validate() callbacks to
%% traverse a diff while validating.
%% Currently Oval is always the atom 'undefined'.
%% When Op == ?MOP_MOVED_AFTER (only for "ordered-by user" list entry),
%% Eval == {} means that the entry was moved first in the list, otherwise
%% Eval is a econfd:key() tuple that identifies the entry it was moved after.
diff_iterate(Sock, Tid, Fun, Flags, InitState) ->
    econfd_internal:bin_write(Sock,
                              <<?MAAPI_DIFF_ITER:32, Tid:32, 1:32, Flags:32>>),
    iterate_loop(Sock, Fun, InitState).

%% @spec keypath_diff_iterate(Sock::port(), Tid::integer(),
%%                            IKP::econfd:ikeypath(),
%%                            Fun::(IKP::econfd:ikeypath(), Op::integer(),
%%                                  Oval::econfd:value(), Eval,
%%                                  State::term()) ->
%%                       {ok, Ret::integer(), State2::term()} | {error, term()},
%%                    InitState::term()) ->
%%               {ok, State::term()} | {error, term()}
%%      Eval = econfd:value() | econfd:key()
%% @doc Iterate through a diff.
%% This function behaves like diff_iterate() with the exception that
%% the provided keypath IKP, prunes the tree and only diffs below that
%% path are considered.
keypath_diff_iterate(Sock, Tid, IKP, Fun, InitState) ->
    keypath_diff_iterate(Sock, Tid, IKP, Fun, 0, InitState).
keypath_diff_iterate(Sock, Tid, IKP, Fun, Flags, InitState) ->
    Term = {1, Flags, reverse(IKP)},
    B = ?t2b(Term),
    econfd_internal:bin_write(Sock,
                              <<?MAAPI_DIFF_IKP_ITER:32, Tid:32, B/binary>>),
    iterate_loop(Sock, Fun, InitState).


%% @spec iterate(Sock::port(), Tid::integer(), IKP::econfd:ikeypath(),
%%               Fun::(IKP::econfd:ikeypath(), Value, Attrs, State::term()) ->
%%                       {ok, Ret::integer(), State2::term()} | {error, term()},
%%               Flags::integer(), InitState::term()) ->
%%          {ok, State::term()} | {error, term()}
%%      Flags = integer()
%%      Value = econfd:value() | undefined
%%      Attrs = [{Attr::integer(), V::value()}] | undefined
%% @doc Iterate over all the data in the transaction and the underlying
%% data store. Flags can be given as ?MAAPI_ITER_WANT_ATTR to request that
%% attributes (if any) are passed to the Fun, otherwise it should be 0.
%% The possible values for Ret in the return value for Fun are the same
%% as for diff_iterate/5.
iterate(Sock, Tid, IKP, Fun, Flags, InitState) ->
    Term = {1, Flags, reverse(IKP)},
    B = ?t2b(Term),
    econfd_internal:bin_write(Sock,
                              <<?MAAPI_ITERATE:32, Tid:32, B/binary>>),
    iterate_loop(Sock, Fun, InitState).


iterate_loop(Sock, Fun, State) ->
    case econfd_internal:term_read(Sock, infinity) of
        {ok, {return}} -> {ok, State};
        {ok, {error}} -> {error, noexists};
        {ok, {error, Reason}} -> {error, Reason};
        {ok, {badstate}} -> {error, badstate};
        {ok, {badstate, Reason}} -> {error, {badstate, Reason}};
        {ok, {IKP, Op, OldValue, Value}} ->     % diff_iterate
            Res = try
                      Fun(IKP, Op, OldValue, Value, State)
                  catch
                      Class:Reason ->
                          econfd_internal:bin_write(Sock, <<?ITER_STOP:32>>),
                          erlang:raise(Class, Reason, erlang:get_stacktrace())
                  end,
            iterate_result(Sock, Fun, Res);
        {ok, {IKP, Op, OldValue, Value, {Str, Tokens}}} ->  % cli_diff_iterate
            Res = try
                      Fun(IKP, Op, OldValue, Value, Str, Tokens, State)
                  catch
                      Class:Reason ->
                          econfd_internal:bin_write(Sock, <<?ITER_STOP:32>>),
                          erlang:raise(Class, Reason, erlang:get_stacktrace())
                  end,
            iterate_result(Sock, Fun, Res);
        {ok, {IKP, Value, Attrs}} ->     % iterate
            Res = try
                      Fun(IKP, Value, Attrs, State)
                  catch
                      Class:Reason ->
                          econfd_internal:bin_write(Sock, <<?ITER_STOP:32>>),
                          erlang:raise(Class, Reason, erlang:get_stacktrace())
                  end,
            iterate_result(Sock, Fun, Res);
        Err ->
            Err
    end.

iterate_result(Sock, _Fun, {ok, ?ITER_STOP, State}) ->
    econfd_internal:bin_write(Sock, <<?ITER_STOP:32>>),
    {ok, State};
iterate_result(Sock, Fun, {ok, RetVal, State}) when RetVal == ?ITER_RECURSE;
                                                    RetVal == ?ITER_CONTINUE ->
    econfd_internal:bin_write(Sock, <<RetVal:32>>),
    iterate_loop(Sock, Fun, State);
iterate_result(Sock, _Fun, {error, Reason}) ->
    econfd_internal:bin_write(Sock, <<?ITER_STOP:32>>),
    {error, Reason}.


%% @spec xpath_eval(Sock::port(), Tid::integer(), Expr::binary(),
%%                  ResultFun::(IKP::econfd:ikeypath(), Value, State::term()) ->
%%                           {Ret::integer(), State2::term()},
%%                  TraceFun, InitState::term(), Context) ->
%%          {ok, State::term()} | err()
%%      TraceFun = (binary()) -> void() | undefined
%%      Context = econfd:ikeypath() | []
%%      Value = econfd:value() | undefined
%% @doc Evaluate the XPath expression Expr, invoking ResultFun for each node
%% in the resulting node set. The possible values for Ret in the
%% return value for ResultFun are ?ITER_CONTINUE and ?ITER_STOP.
%% This function is kept for backwards compatibility, use xpath_eval/6.
xpath_eval(Sock, Tid, Expr, ResultFun, TraceFun, InitState, Context) ->
    xpath_eval(Sock, Tid, Expr, ResultFun, InitState,
               [{tracefun,TraceFun},{initstate,InitState},{context, Context}]).


-type xpath_eval_option() :: {'tracefun', term()}
                           | {'context', econfd:ikeypath()}
                           | {'varbindings', [{Name::string(),
                                               ValueExpr::string()|binary()}]}
                           | {'root', econfd:ikeypath()}.

-spec xpath_eval(Sock::econfd:socket(),
                 Tid::integer(),
                 Expr::binary()|
                       {compiled,
                        Source::binary(),
                        Compiled::[binary() | tuple()]},
                 ResultFun::
                   fun((IKP::econfd:ikeypath(), Value::term(), State::term()) ->
                              {Ret::integer(), State2::term()}),
                 InitState::term(),
                 Options::[xpath_eval_option()]) ->
                        {'ok', State::term()} | err().
%% @doc Evaluate the XPath expression Expr, invoking ResultFun for each node
%% in the resulting node set. The possible values for Ret in the
%% return value for ResultFun are ?ITER_CONTINUE and ?ITER_STOP.
xpath_eval(Sock, Tid, Expr, ResultFun, InitState, Options) ->
    TraceFun = proplists:get_value(tracefun,    Options, undefined),
    Context  = proplists:get_value(context,     Options, []),
    LLPos    = proplists:get_value('leaf-list-pos', Options, 0),
    VarBinds = proplists:get_value(varbindings, Options, []),
    Root     = proplists:get_value(root,        Options, undefined),

    R = {Expr, TraceFun /= undefined, reverse(Context), LLPos, VarBinds, Root},
    case intcall(Sock, ?MAAPI_XPATH_EVAL, Tid, R) of
        ok ->
            xpath_eval_loop(Sock, ResultFun, TraceFun, InitState);
        Err ->
            Err
    end.

xpath_eval_loop(Sock, ResultFun, TraceFun, State) ->
    case econfd_internal:term_read(Sock, infinity) of
        {ok, {return}} -> {ok, State};
        {ok, {trace, Str}} ->
            TraceFun(Str),
            xpath_eval_loop(Sock, ResultFun, TraceFun, State);
        {ok, {error, Str}} ->
            {error, {?CONFD_ERR_XPATH, Str}};
        {ok, {IKP, Value}} ->
            case ResultFun(IKP, Value, State) of
                {?ITER_STOP, State2} ->
                    econfd_internal:bin_write(Sock, <<?ITER_STOP:32>>),
                    {ok, State2};
                {?ITER_CONTINUE, State2} ->
                    econfd_internal:bin_write(Sock, <<?ITER_CONTINUE:32>>),
                    xpath_eval_loop(Sock, ResultFun, TraceFun, State2)
            end;
        Err ->
            Err
    end.

%% @spec xpath_eval_expr(Sock::port(), Tid::integer(), Expr::binary(),
%%                       TraceFun, Context) ->
%%          {ok, Result::binary()} | err()
%%      TraceFun = (binary()) -> void() | undefined
%%      Context = econfd:ikeypath() | []
%% @doc Evaluate the XPath expression Expr, returning the result as a string.
xpath_eval_expr(Sock, Tid, Expr, TraceFun, Context) ->
    xpath_eval_expr(Sock, Tid, Expr, [{tracefun,TraceFun},{context,Context}]).

-spec xpath_eval_expr(Sock::econfd:socket(),
                      Tid::integer(),
                      Expr::binary()|
                            {compiled,
                             Source::binary(),
                             Compiled::[binary() | tuple()]},
                      Options::[xpath_eval_option()]) ->
                             {'ok', Result::binary()} | err().
%% @doc Evaluate the XPath expression Expr, returning the result as a string.
xpath_eval_expr(Sock, Tid, Expr, Options) ->
    TraceFun = proplists:get_value(tracefun,        Options, undefined),
    Context  = proplists:get_value(context,         Options, []),
    LLPos    = proplists:get_value('leaf-list-pos', Options, 0),
    VarBinds = proplists:get_value(varbindings,     Options, []),
    Root     = proplists:get_value(root,            Options, undefined),

    R = {Expr, TraceFun /= undefined, reverse(Context), LLPos, VarBinds, Root},
    case intcall(Sock, ?MAAPI_XPATH_EVAL_EXPR, Tid, R) of
        ok ->
            xpath_eval_expr_loop(Sock, TraceFun);
        Err ->
            Err
    end.

xpath_eval_expr_loop(Sock, TraceFun) ->
    case econfd_internal:term_read(Sock, infinity) of
        {ok, {ok, Result}} ->
            {ok, Result};
        {ok, {trace, Str}} ->
            TraceFun(Str),
            xpath_eval_expr_loop(Sock, TraceFun);
        {ok, {error, Str}} ->
            {error, {?CONFD_ERR_XPATH, Str}}
    end.


%% @spec hkeypath2ikeypath(Sock::port(),
%%                  HKP::hkeypath()) -> {ok, IKP::econfd:ikeypath()} | err()
%% @doc Convert a hkeypath to an ikeypath.
%% @deprecated hkeypaths are not used in the erlang API.
hkeypath2ikeypath(Sock, HKP) ->
    intcall(Sock, ?MAAPI_HKP2IKP, -1, HKP).


%% @spec request_action(Sock::port(), Params::[econfd:tagval()],
%%       IKP::econfd:ikeypath()) ->
%%   ok | {ok, Values::[econfd:tagval()]} | err()
%% @doc Invoke an action defined in the data model.
request_action(Sock, Params, IKP) ->
    [[Ns|_]|_] = RIKP = reverse(IKP),   % NOTE, must be real reverse
    R = {{exml, Params}, RIKP, Ns},
    intcall(Sock, ?MAAPI_REQUEST_ACTION, -1, R).


%% @spec cli_prompt(Sock::port(), Usess::integer(), Prompt::binary(),
%%                  Echo::boolean()) -> {ok, Res::binary()} | err()
%% @doc Prompt cli user for a reply
cli_prompt(Sock, Usess, Prompt, Echo) ->
    intcall(Sock, ?MAAPI_CLI_PROMPT, -1,
            {Usess, Prompt, [], Echo, infinity}).

%% @spec cli_prompt(Sock::port(), Usess::integer(), Prompt::binary(),
%%                  Echo::boolean(), Timeout::integer()) ->
%%   {ok, Res::binary()} | err()
%% @doc Prompt cli user for a reply - return error if no reply is
%%      received within Timeout seconds.
cli_prompt(Sock, Usess, Prompt, Echo, Timeout) ->
    intcall(Sock, ?MAAPI_CLI_PROMPT, -1,
            {Usess, Prompt, [], Echo, 1000 * Timeout}).

%% @spec cli_read_eof(Sock::port(), Usess::integer(),
%%                    Echo::boolean()) -> {ok, Res::binary()} | err()
%% @doc Read data from cli until EOF
cli_read_eof(Sock, Usess, Echo) ->
    intcall(Sock, ?MAAPI_CLI_READ_EOF, -1,
            {Usess, Echo, infinity}).

%% @spec cli_read_eof(Sock::port(), Usess::integer(),
%%                    Echo::boolean(), Timeout::integer()) ->
%%   {ok, Res::binary()} | err()
%% @doc Read data from cli until EOF - return error if no reply is
%%      received within Timeout seconds.
cli_read_eof(Sock, Usess, Echo, Timeout) ->
    intcall(Sock, ?MAAPI_CLI_READ_EOF, -1,
            {Usess, Echo, 1000 * Timeout}).

%% @spec cli_prompt_oneof(Sock::port(), Usess::integer(), Prompt::binary(),
%%                        Choice::[binary()]) ->
%%    {ok, Res::binary()} | err()
%% @doc Prompt cli user for a reply
cli_prompt_oneof(Sock, Usess, Prompt, Choice) ->
    intcall(Sock, ?MAAPI_CLI_PROMPT, -1,
            {Usess, Prompt, Choice, true, infinity}).

%% @spec cli_prompt_oneof(Sock::port(), Usess::integer(), Prompt::binary(),
%%                        Choice::[binary()], Timeout::integer()) ->
%%    {ok, Res::binary()} | err()
%% @doc Prompt cli user for a reply - return error if no reply is
%%      received within Timeout seconds.
cli_prompt_oneof(Sock, Usess, Prompt, Choice, Timeout) ->
    intcall(Sock, ?MAAPI_CLI_PROMPT, -1,
            {Usess, Prompt, Choice, true, 1000 * Timeout}).

%% @spec cli_write(Sock::port(), Usess::integer(), Msg::binary()) ->
%%    ok | err()
%% @doc Write mesage to the CLI
cli_write(Sock, Usess, Msg) ->
    intcall(Sock, ?MAAPI_CLI_WRITE, -1,
            {Usess, Msg}).

%% @spec sys_message(Sock::port(), To::binary(),  Msg::binary()) ->
%%    ok | err()
%% @doc Write system message
sys_message(Sock, To, Msg) ->
    intcall(Sock, ?MAAPI_SYS_MESSAGE, -1,
            {To, Msg}).

%% @spec user_message(Sock::port(), To::binary(), From::binary(),
%%                   Msg::binary()) ->
%%    ok | err()
%% @doc Write user message
user_message(Sock, To, From, Msg) ->
    intcall(Sock, ?MAAPI_USER_MESSAGE, -1,
            {To, Msg, From}).

%% @spec prio_message(Sock::port(), To::binary(),  Msg::binary()) ->
%%    ok | err()
%% @doc Write system message
prio_message(Sock, To, Msg) ->
    intcall(Sock, ?MAAPI_PRIO_MESSAGE, -1,
            {To, Msg}).

%% @spec set_readonly_mode(Sock::port(), Mode::boolean())
%% -> {ok, bool()} | err()
%% @doc  Control if we can create rw transactions
set_readonly_mode(Sock, Mode) ->
    ibool(intcall(Sock, ?MAAPI_SET_READONLY, -1,
                  if (Mode == true) -> 1;
                     true -> 0
                  end)).


%% @spec init_upgrade(Sock::port(), TimeoutSecs::integer(), Flags::integer()) ->
%%   ok | err()
%% @doc Start in-service upgrade
init_upgrade(Sock, TimeoutSecs, Flags) ->
    R = {TimeoutSecs, Flags},
    intcall(Sock, ?MAAPI_INIT_UPGRADE, -1, R).

%% @spec perform_upgrade(Sock::port(), [LoadPath::binary()]) ->
%%   ok | err()
%% @doc Do in-service upgrade
perform_upgrade(Sock, LoadPathList) ->
    intcall(Sock, ?MAAPI_PERFORM_UPGRADE, -1, LoadPathList).

%% @spec commit_upgrade(Sock::port()) ->
%%   ok | err()
%% @doc Commit in-service upgrade
commit_upgrade(Sock) ->
    intcall(Sock, ?MAAPI_COMMIT_UPGRADE, -1, <<>>).

%% @spec abort_upgrade(Sock::port()) ->
%%   ok | err()
%% @doc Abort in-service upgrade
abort_upgrade(Sock) ->
    intcall(Sock, ?MAAPI_ABORT_UPGRADE, -1, <<>>).


%%% Daemon control functions

%% @spec aaa_reload(Sock::port(), Synchronous::boolean()) -> ok | err()
%% @doc Tell AAA to reload external AAA data
aaa_reload(Sock, Synchronous) ->
    intcall(Sock, ?MAAPI_AAA_RELOAD, -1, bool2int(Synchronous)).

%% @spec snmpa_reload(Sock::port(), Synchronous::boolean()) -> ok | err()
%% @doc Tell ConfD to reload external SNMP Agent config data
snmpa_reload(Sock, Synchronous) ->
    intcall(Sock, ?MAAPI_SNMPA_RELOAD, -1, bool2int(Synchronous)).

%% @spec start_phase(Sock::port(), Phase::1|2, Synchronous::boolean()) ->
%%          ok | err()
%% @doc Tell ConfD to proceed to next start phase
start_phase(Sock, Phase, Synchronous) when Phase >= 1, Phase =< 2 ->
    intcall(Sock, ?MAAPI_START_PHASE, -1, {Phase, bool2int(Synchronous)}).

%% @spec wait_start(Sock::port()) -> ok | err()
%% @equiv wait_start(Sock, 2)
%% @doc Wait until ConfD daemon has completely started.
wait_start(Sock) ->
    wait_start(Sock, 2).

%% @spec wait_start(Sock::port(), Phase::0|1|2) -> ok | err()
%% @doc Wait until ConfD daemon has reached a certain start phase.
wait_start(Sock, Phase) when Phase >= 0, Phase =< 2 ->
    intcall(Sock, ?MAAPI_WAIT_START, -1, Phase).

%% @spec reload_config(Sock::port()) -> ok | err()
%% @doc Tell ConfD daemon to reload its configuration
reload_config(Sock) ->
    intcall(Sock, ?MAAPI_RELOAD_CONFIG, -1, <<>>).

%% @spec stop(Sock::port()) -> ok
%% @equiv stop(Sock, true)
%% @doc Tell ConfD daemon to stop, returns when daemon has exited
stop(Sock) ->
    stop(Sock, true).

%% @spec stop(Sock::port(), Synchronous::boolean()) -> ok
%% @doc Tell ConfD daemon to stop, if Synchronous is true won't return
%%      until daemon has come to a halt. Note that the socket will
%%      most certainly not be possible to use again, since ConfD will
%%      close its end when it exits.
stop(Sock, Synchronous) ->
    intcall(Sock, ?MAAPI_STOP, -1, bool2int(Synchronous)),
    ok.

%% @spec ncs_apply_template(Sock::port(), Tid::integer(),
%%                          TemplateName::binary(), RootIKP::econfd:ikeypath(),
%%                          Variables::term(), Documents::term(),
%%                          Shared::boolean(), Backpointer::boolean()) ->
%%     ok | err()
%% @doc Apply a template that has been loaded into NCS. The TemplateName
%%      parameter gives the name of the template. The Variables parameter
%%      is a list of variables and names for substitution into the template.
ncs_apply_template(Sock, Tid, TemplateName, RootIKP, Variables,
                   Documents, Shared, Backpointer) ->
    Term = {TemplateName, reverse(RootIKP), Variables,
            Documents, Shared, Backpointer},
    intcall(Sock, ?MAAPI_NCS_APPLY_TEMPLATE, Tid, Term).

%% @spec ncs_templates(Sock::port()) -> {ok, Res::binary()} | err()
%% @doc Retrieve a list of the templates currently loaded into NCS.
ncs_templates(Sock) ->
    intcall(Sock, ?MAAPI_NCS_TEMPLATES, -1, <<>>).

%% @spec ncs_template_variables(Sock::port(), Tid::integer(),
%%                              TemplateName::binary()) ->
%%     {ok, Res::binary()} | err()
%% @doc Retrieve the variables used in a template.
ncs_template_variables(Sock, Tid, TemplateName) ->
    intcall(Sock, ?MAAPI_NCS_TEMPLATE_VARIABLES, Tid, TemplateName).

%% @spec ncs_write_service_log_entry(Sock::port(), SIKP::econfd:ikeypath(),
%%                                   Msg::string(), Type::econfd:value(),
%%                                   Level::econfd:value()) -> ok | err()
%% @doc Write a service log entry.
ncs_write_service_log_entry(Sock, SIKP, Msg, Type, Level) ->
    Term = {lists:reverse(SIKP), 0, Msg, Type, Level},
    intcall(Sock, ?MAAPI_NCS_WRITE_SERVICE_LOG_ENTRY, -1, Term).

%%%--------------------------------------------------------------------
%%% Private functions
%%%--------------------------------------------------------------------

%% @spec ncs_commit_save_trans(Sock::port(), TidFrom::integer(),
%%                             TidTo::integer(), Usid::integer(),
%%                             IKP::econfd:ikeypath()) ->
%%            ok | err()
%% @private NCS-specific internal function.
ncs_commit_save_trans(Sock, TidFrom, TidTo, Usid, IKP) ->
    Term = {TidTo, Usid, reverse(IKP)},
    intcall(Sock, ?MAAPI_NCS_COMMIT_SAVE_TRANS, TidFrom, Term).

ncs_update_plan_history(Sock, TidFrom, Usid, Plan, PlanHistory, Nano, IKP) ->
    Term = {Usid, Plan, PlanHistory, Nano, reverse(IKP)},
    intcall(Sock, ?MAAPI_NCS_UPDATE_PLAN_HISTORY, TidFrom, Term).

ncs_commit_save_no_diffset(Sock, TidFrom, TidTo, Usid, IKP) ->
    Term = {TidTo, Usid, reverse(IKP)},
    intcall(Sock, ?MAAPI_NCS_COMMIT_SAVE_NO_DIFFSET, TidFrom, Term).

%% @spec ncs_apply_reverse_diffset(Sock::port(), Tid::integer(),
%%                                 SIKP::econfd:ikeypath() ) ->
%%            ok | err()
%% @private NCS-specific internal function.
ncs_apply_reverse_diffset(Sock, Tid, SIKP) ->
    intcall(Sock, ?MAAPI_NCS_APPLY_DIFF_SET, Tid, {SIKP}).


%%%--------------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------

%% @spec intcall(Socket::term(), Op::integer(),
%%               Tid::integer(), Bin::binary()) ->
%%  ok | err()
intcall(Sock, Op, -1, <<>>) ->
    econfd_internal:confd_call_bin(Sock, <<>>, Op);
intcall(Sock, Op, Tid, <<>>) ->
    econfd_internal:confd_call_bin(Sock, <<Tid:32>>, Op);
intcall(Sock, Op, Tid, Arg) ->
    econfd_internal:confd_call(Sock, Arg, Op, Tid).


ibool({ok, 1}) -> {ok, true};
ibool({ok, 0}) -> {ok, false};
ibool(X) -> X.

bool2int(true)  -> 1;
bool2int(false) -> 0.

reverse(X) -> lists:reverse(X).

mk_uident(UId) ->
    case UId of
        #confd_user_identification{vendor = V, product = P, version = Vsn,
                                   client_identity = CId} ->
            {V, P, Vsn, CId};
        _ ->
            {undefined, undefined, undefined, <<"econfd">>}
    end.
