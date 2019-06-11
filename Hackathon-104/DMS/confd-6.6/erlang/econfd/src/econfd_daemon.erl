%%%-------------------------------------------------------------------
%%% @copyright 2006 Tail-F Systems AB
%%% @version {$Id$}
%%% @private
%%% @doc This module implements a gen_server which acts as the equivalent
%%% of a 'daemon" in the C library.
%%%
%%% econfd:init_daemon() returns one of these gen servers and it is up
%%% to the application programmer to supervise the gen server the gen
%%% server may die for several reasons. Programming error and when the
%%% socket to confd dies.  When that happens, the server needs to be
%%% restarted e.g. we need to reconnect to ConfD.
%%%-------------------------------------------------------------------

-module(econfd_daemon).

-behaviour(gen_server).

-include("../include/econfd.hrl").
-include("../include/econfd_errors.hrl").
-include("econfd_internal.hrl").
-include("econfd_proto.hrl").

-import(econfd,
        [
         report_err/4,
         pp_kpath/1
        ]).

-import(econfd_internal,
        [
         confd_call_active/2,
         term_write/2
        ]).

%%% external exports
-export([start_link/6, stop/1, mk_error/1]).

%%% internal exports
-export([worker/1]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%--------------------------------------------------------------------
%%% External functions
%%%--------------------------------------------------------------------

%% @spec start_link(Name, DebugLevel, Estream, Dopaque, Ip, Port) -> {ok, pid()}
%% @equiv econfd:init_daemon(Name, DebugLevel, Estream, Dopaque, Ip, Port)
start_link(Name, DebugLevel, Estream, Dopaque, Ip, Port) ->
    gen_server:start_link(?MODULE,
                          [Name, DebugLevel,Estream, Dopaque, Ip, Port],[]).

%% @spec stop(pid()) -> void()
stop(Daemon) ->
    gen_server:call(Daemon, stop, infinity).

%%%--------------------------------------------------------------------
%%% gen_server callbacks
%%%--------------------------------------------------------------------

init([Name, DebugLevel, Estream, Dopaque, IP, Port]) ->
    put(sock_int, 1),
    Usess = ets:new(confd_user_sessions, []),
    Trans = ets:new(confd_transactions, [public]),
    Dx = #confd_daemon_ctx{name = Name, debug_level = DebugLevel,
                           daemon_pid = self(),
                           estream = Estream, d_opaque = Dopaque,
                           ip = IP, port = Port,
                           user_sessions = Usess, transactions = Trans,
                           flags = ?CONFD_DAEMON_FLAG_SEND_IKP
                          },
    case do_cnct(Dx, IP, Port) of
        {ok, Dx2} -> {ok, Dx2};
        {error, Reason} -> {stop, Reason}
    end.

handle_call({set_debug, DebugLevel, Estream}, _From, Dx) ->
    NewDx = Dx#confd_daemon_ctx{debug_level = DebugLevel, estream = Estream},
    NewDx#confd_daemon_ctx.worker_pid ! {new_dx, NewDx},
    {reply, ok, NewDx};

handle_call({set_flags, Flags}, _From, Dx) ->
    NewFlags = ?CONFD_DAEMON_FLAG_SEND_IKP bor Flags,
    {reply, ok, Dx#confd_daemon_ctx{flags = NewFlags}};

handle_call({register_data_cbs, DataCbs0}, _From, Dx) ->
    DataCbs = DataCbs0#confd_data_cbs{index = incr()},
    Mask =
        flg(DataCbs#confd_data_cbs.exists_optional,
            ?MASK_DATA_EXISTS_OPTIONAL) bor
        flg(DataCbs#confd_data_cbs.get_elem,?MASK_DATA_GET_ELEM) bor
        flg(DataCbs#confd_data_cbs.get_next,?MASK_DATA_GET_NEXT) bor
        flg(DataCbs#confd_data_cbs.set_elem,?MASK_DATA_SET_ELEM) bor
        flg(DataCbs#confd_data_cbs.create,?MASK_DATA_CREATE) bor
        flg(DataCbs#confd_data_cbs.remove,?MASK_DATA_REMOVE) bor
        flg(DataCbs#confd_data_cbs.find_next,?MASK_DATA_FIND_NEXT) bor
        flg(DataCbs#confd_data_cbs.num_instances,?MASK_DATA_NUM_INSTANCES) bor
        flg(DataCbs#confd_data_cbs.get_object,?MASK_DATA_GET_OBJECT) bor
        flg(DataCbs#confd_data_cbs.get_next_object,
            ?MASK_DATA_GET_NEXT_OBJECT) bor
        flg(DataCbs#confd_data_cbs.find_next_object,
            ?MASK_DATA_FIND_NEXT_OBJECT) bor
        flg(DataCbs#confd_data_cbs.get_case,?MASK_DATA_GET_CASE) bor
        flg(DataCbs#confd_data_cbs.set_case,?MASK_DATA_SET_CASE) bor
        flg(DataCbs#confd_data_cbs.get_attrs,?MASK_DATA_GET_ATTRS) bor
        flg(DataCbs#confd_data_cbs.set_attr,?MASK_DATA_SET_ATTR) bor
        flg(DataCbs#confd_data_cbs.move_after,?MASK_DATA_MOVE_AFTER) bor
        flg(DataCbs#confd_data_cbs.write_all,?MASK_DATA_WRITE_ALL),

    Msg = {?CONFD_PROTO_REGISTER, Dx#confd_daemon_ctx.daemon_id,
           {?CONFD_PROTO_DATA_CB,
            DataCbs#confd_data_cbs.callpoint, Mask,
            Dx#confd_daemon_ctx.flags,
            DataCbs#confd_data_cbs.index}},
    case term_write(Dx#confd_daemon_ctx.ctl, Msg) of
        ok ->
            {reply, ok, Dx#confd_daemon_ctx{
                          data_cbs =
                          [DataCbs | Dx#confd_daemon_ctx.data_cbs]}};
        Err ->
            {reply, Err, Dx}
    end;

handle_call({register_service_cb, ServiceCbs0}, _From, Dx) ->
    ServiceCbs = ServiceCbs0#ncs_service_cbs{index = incr()},
    Mask =
        flg(ServiceCbs#ncs_service_cbs.pre_modification,
            ?MASK_SERVICE_PRE_MODIFICATION) bor
        flg(ServiceCbs#ncs_service_cbs.post_modification,
            ?MASK_SERVICE_POST_MODIFICATION) bor
        flg(ServiceCbs#ncs_service_cbs.pre_lock_create,
            ?MASK_SERVICE_PRE_LOCK_CREATE) bor
        flg(ServiceCbs#ncs_service_cbs.create,
            ?MASK_SERVICE_CREATE),

    if (Mask band (?MASK_SERVICE_PRE_LOCK_CREATE bor ?MASK_SERVICE_CREATE)) =:=
       (?MASK_SERVICE_PRE_LOCK_CREATE bor ?MASK_SERVICE_CREATE) ->
            {reply,
             {error, {?CONFD_ERR_PROTOUSAGE,
                      <<"Cannot register both "
                        "pre_lock_create() and create() callbacks">>}},
             Dx};
       true ->
            Msg = {?CONFD_PROTO_REGISTER, Dx#confd_daemon_ctx.daemon_id,
                   {?CONFD_PROTO_SERVICE_CB,
                    ServiceCbs#ncs_service_cbs.servicepoint, Mask,
                    Dx#confd_daemon_ctx.flags,
                    ServiceCbs#ncs_service_cbs.index}},
            case term_write(Dx#confd_daemon_ctx.ctl, Msg) of
                ok ->
                    {reply,
                     ok,
                     Dx#confd_daemon_ctx{
                       service_cbs =
                           [ServiceCbs | Dx#confd_daemon_ctx.service_cbs]}};
                Err ->
                    {reply, Err, Dx}
            end
    end;


handle_call({register_nano_service_cb,
             ComponentType, State, NanoServiceCbs0}, _From, Dx) ->
    NanoServiceCbs = NanoServiceCbs0#ncs_nano_service_cbs{index = incr()},
    Mask =
        flg(NanoServiceCbs#ncs_nano_service_cbs.create,
            ?MASK_NANO_SERVICE_CREATE) bor
        flg(NanoServiceCbs#ncs_nano_service_cbs.delete,
            ?MASK_NANO_SERVICE_DELETE),

    Msg = {?CONFD_PROTO_REGISTER_NANO, Dx#confd_daemon_ctx.daemon_id,
           {?CONFD_PROTO_NANO_SERVICE_CB,
            {NanoServiceCbs#ncs_nano_service_cbs.servicepoint,
             ComponentType,
             State},
            Mask, Dx#confd_daemon_ctx.flags,
            NanoServiceCbs#ncs_nano_service_cbs.index}},
            case term_write(Dx#confd_daemon_ctx.ctl, Msg) of
                ok ->
                    {reply,
                     ok,
                     Dx#confd_daemon_ctx{
                       service_cbs =
                           [NanoServiceCbs | Dx#confd_daemon_ctx.service_cbs]}};
                Err ->
                    {reply, Err, Dx}
            end;

handle_call({register_valpoint_cbs, ValpointCb0}, _From, Dx) ->
    ValpointCb = ValpointCb0#confd_valpoint_cb{index = incr()},
    Msg = {?CONFD_PROTO_REGISTER, Dx#confd_daemon_ctx.daemon_id,
           {?CONFD_PROTO_VALIDATE_CB,
            ValpointCb#confd_valpoint_cb.valpoint, 0,
            Dx#confd_daemon_ctx.flags,
            ValpointCb#confd_valpoint_cb.index}},
    case term_write(Dx#confd_daemon_ctx.ctl, Msg) of
        ok ->
            {reply, ok, Dx#confd_daemon_ctx{
                          valp_cbs =
                          [ValpointCb | Dx#confd_daemon_ctx.valp_cbs]}};
        Err ->
            {reply, Err, Dx}
    end;

handle_call({register_action_cb, ActionCb0}, _From, Dx) ->
    ActionCb = ActionCb0#confd_action_cb{index = incr()},
    Mask0 = ?MASK_ACT_INIT,
    Mask1 = if ActionCb#confd_action_cb.action/= undefined ->
                    Mask0 bor ?MASK_ACT_ACTION bor ?MASK_ACT_COMMAND;
               true ->
                    Mask0
                        end,
    Mask2 = if ActionCb#confd_action_cb.completion /= undefined ->
                    Mask1 bor ?MASK_ACT_COMPLETION;
               true ->
                    Mask1
            end,
    Msg = {?CONFD_PROTO_REGISTER, Dx#confd_daemon_ctx.daemon_id,
           {?CONFD_PROTO_ACTION_CB,
            ActionCb#confd_action_cb.actionpoint, Mask2,
            Dx#confd_daemon_ctx.flags,
            ActionCb#confd_action_cb.index}},
    case term_write(Dx#confd_daemon_ctx.ctl, Msg) of
        ok ->
            {reply, ok, Dx#confd_daemon_ctx{
                          action_cbs =
                          [ActionCb | Dx#confd_daemon_ctx.action_cbs]}};
        Err ->
            {reply, Err, Dx}
    end;

handle_call({register_trans_cb, TransCbs}, _From, Dx) ->
    Mask =
        flg(TransCbs#confd_trans_cbs.init, ?MASK_TR_INIT) bor
        flg(TransCbs#confd_trans_cbs.trans_lock, ?MASK_TR_TRANS_LOCK) bor
        flg(TransCbs#confd_trans_cbs.trans_unlock, ?MASK_TR_TRANS_UNLOCK) bor
        flg(TransCbs#confd_trans_cbs.write_start, ?MASK_TR_WRITE_START) bor
        flg(TransCbs#confd_trans_cbs.prepare, ?MASK_TR_PREPARE) bor
        flg(TransCbs#confd_trans_cbs.abort, ?MASK_TR_ABORT) bor
        flg(TransCbs#confd_trans_cbs.commit, ?MASK_TR_COMMIT) bor
        flg(TransCbs#confd_trans_cbs.finish, ?MASK_TR_FINISH),
    Did = Dx#confd_daemon_ctx.daemon_id,
    Msg = {?CONFD_TRANS_CB_REGISTER, Did, Mask},
    case term_write(Dx#confd_daemon_ctx.ctl, Msg) of
        ok ->
            {reply, ok, Dx#confd_daemon_ctx{trans_cb = TransCbs}};
        Err ->
            {reply, Err, Dx}
    end;

handle_call({register_db_cb, DbCbs}, _From, Dx) ->
    Mask =
        flg(DbCbs#confd_db_cbs.candidate_commit,
            ?MASK_DB_CANDIDATE_COMMIT) bor
        flg(DbCbs#confd_db_cbs.candidate_confirming_commit,
            ?MASK_DB_CANDIDATE_CONFIRMING_COMMIT) bor
        flg(DbCbs#confd_db_cbs.candidate_reset,
            ?MASK_DB_CANDIDATE_RESET) bor
        flg(DbCbs#confd_db_cbs.candidate_chk_not_modified,
            ?MASK_DB_CANDIDATE_CHK_NOT_MODIFIED) bor
        flg(DbCbs#confd_db_cbs.candidate_rollback_running,
            ?MASK_DB_CANDIDATE_ROLLBACK_RUNNING) bor
        flg(DbCbs#confd_db_cbs.candidate_validate,
            ?MASK_DB_CANDIDATE_VALIDATE) bor
        flg(DbCbs#confd_db_cbs.add_checkpoint_running,
            ?MASK_DB_ADD_CHECKPOINT_RUNNING) bor
        flg(DbCbs#confd_db_cbs.del_checkpoint_running,
            ?MASK_DB_DEL_CHECKPOINT_RUNNING) bor
        flg(DbCbs#confd_db_cbs.activate_checkpoint_running,
            ?MASK_DB_ACTIVATE_CHECKPOINT_RUNNING) bor
        flg(DbCbs#confd_db_cbs.copy_running_to_startup,
            ?MASK_DB_COPY_RUNNING_TO_STARTUP) bor
        flg(DbCbs#confd_db_cbs.running_chk_not_modified,
            ?MASK_DB_RUNNING_CHK_NOT_MODIFIED) bor
        flg(DbCbs#confd_db_cbs.lock, ?MASK_DB_LOCK) bor
        flg(DbCbs#confd_db_cbs.unlock, ?MASK_DB_UNLOCK) bor
        flg(DbCbs#confd_db_cbs.lock_partial, ?MASK_DB_LOCK_PARTIAL) bor
        flg(DbCbs#confd_db_cbs.unlock_partial, ?MASK_DB_UNLOCK_PARTIAL) bor
        flg(DbCbs#confd_db_cbs.delete_config , ?MASK_DB_DELETE_CONFIG),
    Did = Dx#confd_daemon_ctx.daemon_id,
    Msg = {?CONFD_DB_CB_REGISTER, Did, Mask},
    case term_write(Dx#confd_daemon_ctx.ctl, Msg) of
        ok ->
            {reply, ok, Dx#confd_daemon_ctx{db_cb = DbCbs}};
        Err ->
            {reply, Err, Dx}
    end;

handle_call({register_authentication_cb, AuthenticationCb}, _From, Dx) ->
    Mask = 0,
    Msg = {?CONFD_PROTO_REGISTER, Dx#confd_daemon_ctx.daemon_id,
           {?CONFD_PROTO_AUTH_CB, auth, Mask,
            Dx#confd_daemon_ctx.flags, 0}},
    case term_write(Dx#confd_daemon_ctx.ctl, Msg) of
        ok ->
            {reply, ok, Dx#confd_daemon_ctx{
                          authentication_cb = AuthenticationCb}};
        Err ->
            {reply, Err, Dx}
    end;

handle_call({register_notification_stream, NotifCb}, _From, Dx) ->
    try
        case Dx#confd_daemon_ctx.notif_worker of
            undefined ->
                case worker_do_connect(Dx#confd_daemon_ctx.ip,
                                       Dx#confd_daemon_ctx.port,
                                       Dx#confd_daemon_ctx.daemon_id) of
                    {ok, Socket, Int} ->
                        Dx2 = Dx#confd_daemon_ctx{
                                notif_worker = Socket,
                                nwint = Int};
                    Err ->
                        error_logger:format("Failed to connect to ConfD: ~p:"
                                            ,[Err]),
                        Dx2 = throw(noconnect)
                end;
            _ ->
                Dx2 = Dx
        end,
        Ncbs = Dx#confd_daemon_ctx.notif_cbs,
        case lists:keysearch(NotifCb#confd_notification_stream_cbs.streamname,
                             #confd_notification_stream_cbs.streamname,
                             Ncbs) of
            false ->
                ok;
            _ ->
                error_logger:format("Duplicate registration\n",[]),
                throw(duplicate)
        end,
        %% Create a new context
        Nctx = #confd_notification_ctx{
          streamname = NotifCb#confd_notification_stream_cbs.streamname,
          notif_worker = Dx2#confd_daemon_ctx.notif_worker,
          opaque = NotifCb#confd_notification_stream_cbs.opaque,
          flags = Dx#confd_daemon_ctx.flags
         },
        Dx3 = Dx2#confd_daemon_ctx{notif_cbs = [NotifCb | Ncbs]},
        Mask = if
                   NotifCb#confd_notification_stream_cbs.replay == undefined ->
                       0;
                   true ->
                       ?MASK_NOTIF_GET_LOG_TIMES bor ?MASK_NOTIF_REPLAY
               end,
        Index = Dx3#confd_daemon_ctx.nwint,
        T = {?CONFD_PROTO_REGISTER, Dx#confd_daemon_ctx.daemon_id,
             {?CONFD_PROTO_NOTIF_STREAM_CB,
              NotifCb#confd_notification_stream_cbs.streamname,
              Mask, Dx#confd_daemon_ctx.flags, Index}},
        term_write(Dx3#confd_daemon_ctx.ctl, T),
        %%io:format("Reg: ~p~n", [Dx3#confd_daemon_ctx.notif_worker]),
        {reply, {ok, Nctx}, Dx3}
    catch
        _:Rsn ->
            ?cerror_msg("Failed to reg notif cbs: ~p~n", [Rsn]),
            {reply, {error, Rsn}, Dx}
    end;


handle_call({register_trans_validate_cbs, TVC}, _From, Dx) ->
    {reply, ok, Dx#confd_daemon_ctx{trans_validate_cb = TVC}};

handle_call(register_done, _From, #confd_daemon_ctx{worker_pid = Pid} = Dx)
  when Pid /= undefined ->
    {reply,
     {error, {?CONFD_ERR_PROTOUSAGE, <<"Not allowed after register_done">>}},
     Dx};
handle_call(register_done, _From, Dx0) ->
    Worker = proc_lib:spawn_link(?MODULE, worker, [Dx0]),
    Dx = Dx0#confd_daemon_ctx{worker_pid = Worker},
    econfd:controlling_process(Dx#confd_daemon_ctx.worker, Worker),
    Ret = confd_call_active(Dx#confd_daemon_ctx.ctl,
                            {?CONFD_PROTO_REGISTER_DONE,
                             Dx#confd_daemon_ctx.daemon_id}),
    case Ret of
        {ok, {?CONFD_PROTO_OLD_USESS, Users}} ->
            lists:foreach(fun(Utuple) ->
                                  Uinfo = econfd:mk_uinfo(Utuple),
                                  Usid = Uinfo#confd_user_info.usid,
                                  ets:insert(Dx#confd_daemon_ctx.user_sessions,
                                             {Usid, Uinfo})
                          end, Users),
            {reply, ok, Dx};
        Other ->
            {reply, Other, Dx}
    end;

handle_call(stop, _From, Dx) ->
    {stop, normal, ok, Dx}.

handle_cast({set_d_opaque, DO}, Dx) ->
    NewDx = Dx#confd_daemon_ctx{d_opaque = DO},
    NewDx#confd_daemon_ctx.worker_pid ! {new_dx, NewDx},
    {noreply, NewDx};
handle_cast(_Msg, Dx) ->
    {noreply, Dx}.

handle_info({tcp, _Socket, Data}, Dx) ->
    confd_fd_ready(Dx, econfd_internal:term_get(Data)),
    {noreply, Dx};
handle_info({tcp_closed, _Socket}, Dx) ->
    {stop, shutdown, Dx};
handle_info({tcp_error, _Socket, Reason}, Dx) ->
    error_logger:format("Confd/NCS socket died: ~p\n",[Reason]),
    {stop, {error, Reason}, Dx}.

terminate(_Reason, _Dx) ->
    ok.

code_change(_OldVsn, Dx, _Extra) ->
    {ok, Dx}.

%%%--------------------------------------------------------------------
%%% External functions
%%%--------------------------------------------------------------------

incr() -> X = get(sock_int),
          put(sock_int, X+1),
          X.

trans_reply_ok(Tctx)
  when ((Tctx#confd_trans_ctx.lastop == ?CONFD_PROTO_NEW_TRANS) or
        (Tctx#confd_trans_ctx.lastop == ?CONFD_PROTO_NEW_VALIDATE)) ->
    Dx = Tctx#confd_trans_ctx.dx,
    if Tctx#confd_trans_ctx.lastop == ?CONFD_PROTO_NEW_TRANS ->
            econfd:trans_put(Dx, th, Tctx);
       true ->
            econfd:trans_put(Dx, thvalidate, Tctx)
    end,
    Reply = {Tctx#confd_trans_ctx.lastop, Tctx#confd_trans_ctx.query_ref,
             Dx#confd_daemon_ctx.daemon_id,
             Tctx#confd_trans_ctx.thandle,
             Dx#confd_daemon_ctx.wint},
    term_write(Dx#confd_daemon_ctx.ctl, Reply);
trans_reply_ok(Tctx) ->
    Dx = Tctx#confd_trans_ctx.dx,
    if ((Tctx#confd_trans_ctx.lastop == ?CONFD_PROTO_CLOSE_TRANS) or
        (Tctx#confd_trans_ctx.lastop == ?CONFD_PROTO_CLOSE_VALIDATE)) ->
            ok;
       true ->
            econfd:trans_put(Dx, th, Tctx)
    end,
    Reply = {Tctx#confd_trans_ctx.lastop, Tctx#confd_trans_ctx.query_ref,
             Dx#confd_daemon_ctx.daemon_id,
             Tctx#confd_trans_ctx.thandle},
    term_write(Dx#confd_daemon_ctx.worker, Reply).


is_key(K) when is_tuple(K) -> true;
is_key(false) -> true;
is_key(_) -> false.

%% FIXME do this but better and only when debug
chk_reply(?CONFD_VALIDATE_VALUE, Val) ->
    Val;
chk_reply(?CONFD_DATA_CB_GET_NEXT, {Key, Next}) ->
    case is_key(Key) of
        true ->
            {Key, Next};
        false ->
            throw({badret, key, Key})
    end;
chk_reply(?CONFD_DATA_CB_NUM_INSTANCES, Int) when is_integer(Int) ->
    Int;
chk_reply(?CONFD_DATA_CB_GET_OBJECT, L) when is_list(L) ->
    L;
chk_reply(_Op, Val) ->
    Val.

-define(is_cs_error(E), element(1, E) == cs_error).

proto_reply(Op, Tctx0, _ExpectData, RetVal0) ->
    case RetVal0 of
        {ok, #confd_trans_ctx{} = Tctx} ->
            RetVal = ok;
        {ok, R, #confd_trans_ctx{} = Tctx} ->
            RetVal = {ok, R};
        {ok, R, T, #confd_trans_ctx{} = Tctx} ->
            RetVal = {ok, R, T};
        _ ->
            Tctx = Tctx0,
            RetVal = RetVal0
    end,
    Dx = Tctx#confd_trans_ctx.dx,
    if
        (Dx#confd_daemon_ctx.debug_level >= ?CONFD_TRACE) and
        (Dx#confd_daemon_ctx.estream /= undefined) ->
            case RetVal of
                ok ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> OK\n",[]);
                {ok, Reply} ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> OK: ~p\n",[chk_reply(Op,Reply)]);
                {ok, Reply, _Timeout} ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> OK: ~p\n",[chk_reply(Op,Reply)]);
                {error, #confd_error{code = Code, str = Str}} ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_ERR: ~s:~s\n",[?a2l(Code),?s2l(Str)]);
                {error, CE} when ?is_cs_error(CE) ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_ERR: <internal #cs_error{}>\n",[]);
                {error, ErrStr} ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_ERR: ~s\n", [?s2l(ErrStr)]);
                {validation_warn, Warning} ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_VALIDATION_WARN: ~p\n",
                              [?s2l(Warning)]);
                delayed_response ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_DELAYED_RESPONSE",[]);
                _ ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> BADRETVAL\n",[])
            end;
        true ->
            ok
    end,
    %% FIXME seen reply stuff ???
    case RetVal of
        ok ->
            econfd:data_reply_ok(Tctx);
        {ok, {Key, Next}} when Op == ?CONFD_DATA_CB_GET_NEXT;
                               Op == ?CONFD_DATA_CB_FIND_NEXT ->
            econfd:data_reply_next_key(Tctx, Key, Next);
        {ok, Value} when Op == ?CONFD_DATA_CB_GET_ELEM;
                         Op == ?CONFD_DATA_CB_NUM_INSTANCES;
                         Op == ?CONFD_DATA_CB_EXISTS_OPTIONAL;
                         Op == ?CONFD_DATA_CB_GET_CASE;
                         Op == ?CONFD_DATA_CB_GET_ATTRS ->
            econfd:data_reply_value(Tctx, Value);
        {ok, ValueList} when Op == ?CONFD_DATA_CB_GET_OBJECT ->
            econfd:data_reply_value_array(Tctx, ValueList);
        {ok, {Values, Next}} when Op == ?CONFD_DATA_CB_GET_NEXT_OBJECT;
                                  Op == ?CONFD_DATA_CB_FIND_NEXT_OBJECT ->
            econfd:data_reply_next_object_value_array(Tctx, Values, Next);
        {ok, Objects, Timeout} when Op == ?CONFD_DATA_CB_GET_NEXT_OBJECT;
                                    Op == ?CONFD_DATA_CB_FIND_NEXT_OBJECT ->
            econfd:data_reply_next_object_value_arrays(Tctx, Objects, Timeout);
        {error, Reason} ->
            reply_error(Tctx, Reason);
        {validation_warn, Warn} ->
            reply_warning(Tctx, Warn);
        delayed_response ->
            ok;
        _ ->
            reply_error(Tctx, #confd_error{code = proto_usage,
                                           str = <<"Bad return value">>})
    end.

reply_warning(Tctx, Str) ->
    Dx = Tctx#confd_trans_ctx.dx,
    R = {?CONFD_PROTO_CALLBACK,
         Tctx#confd_trans_ctx.query_ref,
         Dx#confd_daemon_ctx.daemon_id,
         {warning, ?s2b(Str)}},
    term_write(?wsock(Tctx), R).

reply_error(Tctx, Error) ->
    econfd:data_reply_error(Tctx, Error).

mk_error(#confd_error{str = Str} = Error) when is_list(Str) ->
    mk_error(Error#confd_error{str = ?l2b(Str)});
mk_error(#confd_error{code = Code, apptag = Tag, str = Str,info = undefined}) ->
    {Code, Tag, Str};
mk_error(#confd_error{code = Code, apptag = Tag, str = Str, info = Info0}) ->
    Info = {{exml, Info0}, 1},
    {Code, Tag, Str, Info};
mk_error(CE) when ?is_cs_error(CE) ->
    CE;
mk_error(ErrStr) ->
    mk_error(#confd_error{code = application, str = ErrStr}).

trans_reply(Tctx0, RetVal0) ->
    case RetVal0 of
        {ok, #confd_trans_ctx{} = Tctx} ->
            RetVal = ok;
        _ ->
            Tctx = Tctx0,
            RetVal = RetVal0
    end,
    Dx = Tctx#confd_trans_ctx.dx,
    if
        (Dx#confd_daemon_ctx.debug_level >= ?CONFD_TRACE) and
        (Dx#confd_daemon_ctx.estream /= undefined) ->
            case RetVal of
                ok ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> OK\n",[]);
                {error, #confd_error{code = Code, str = Str}} ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_ERR: ~s:~s\n",[?a2l(Code),?s2l(Str)]);
                {error, CE} when ?is_cs_error(CE) ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_ERR: <internal #cs_error{}>\n",[]);
                {error, ErrStr} ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_ERR: ~s\n", [?s2l(ErrStr)]);
                confd_already_locked ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_ALREADY_LOCKED\n",[]);
                confd_in_use ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_IN_USE\n",[]);
                _ ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> BADRETVAL\n",[])
            end;
        true ->
            ok
    end,
    case RetVal of
        ok ->
            trans_reply_ok(Tctx);
        {error, Error}  ->
            reply_error(Tctx, Error);
        confd_already_locked
        when Tctx#confd_trans_ctx.lastop == ?CONFD_PROTO_TRANS_LOCK ->
            reply_error(Tctx, #confd_error{code = in_use});
        confd_in_use
        when Tctx#confd_trans_ctx.lastop == ?CONFD_PROTO_WRITE_START;
             Tctx#confd_trans_ctx.lastop == ?CONFD_PROTO_PREPARE ->
            reply_error(Tctx, #confd_error{code = in_use});
        _ ->
            report_err(Dx, ?CONFD_DEBUG,
                             "Bad application return value: ~p thandle=~p\n",
                             [RetVal, Tctx#confd_trans_ctx.thandle]),
            reply_error(Tctx, #confd_error{code = proto_usage,
                                           str = <<"Bad return value">>}),
            {error, <<"bad retval">>}
    end.

db_reply(Dx, Dbx, RetVal) ->
    if
        (Dx#confd_daemon_ctx.debug_level >= ?CONFD_TRACE) and
        (Dx#confd_daemon_ctx.estream /= undefined) ->
            case RetVal of
                ok ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> OK\n",[]);
                {error, #confd_error{code = Code, str = Str}} ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_ERR: ~s:~s\n",[?a2l(Code),?s2l(Str)]);
                {error, CE} when ?is_cs_error(CE) ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_ERR: <internal #cs_error{}>\n",[]);
                {error, ErrStr} ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_ERR: ~s\n", [?s2l(ErrStr)]);
                _ ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> BADRETVAL\n",[])
            end;
        true ->
            ok
    end,
    Send = case RetVal of
               ok ->
                   {?CONFD_PROTO_DB_REPLY, Dbx#confd_db_ctx.qref,
                    Dbx#confd_db_ctx.did, ok};
               {error, Error} ->
                   {?CONFD_PROTO_DB_REPLY, Dbx#confd_db_ctx.qref,
                    Dbx#confd_db_ctx.did, error, mk_error(Error)};
               _ ->
                   report_err(Dx, ?CONFD_DEBUG,
                                    "Bad retval ~p~n", [RetVal]),
                   {?CONFD_PROTO_DB_REPLY, Dbx#confd_db_ctx.qref,
                    Dbx#confd_db_ctx.did, error,
                    mk_error(#confd_error{code = proto_usage,
                                          str = <<"Bad return value">>})}
           end,
    term_write(Dx#confd_daemon_ctx.ctl, Send).

init_db_ctx(Dx, Et, Op, Usid) ->
    U=case ets:lookup(Dx#confd_daemon_ctx.user_sessions, Usid) of
        [] ->
              #confd_user_info{};
        [{_,Uinfo}] ->
              Uinfo
    end,
    DbCtx = init_db_ctx(Dx, Et, Op),
    DbCtx#confd_db_ctx{uinfo = U}.

init_db_ctx(Dx, Et, Op) ->
    #confd_db_ctx{dx = Dx, lastop = Op, did = element(3, Et),
                  qref = element(2, Et)}.


%% control socket callbacks
confd_fd_ready(Dx, Et={Op=?CONFD_PROTO_NEW_USESS,_Qref,_Did,Users})->
    %% NOTE: This may include updates of sessions received earlier
    UsessTab = Dx#confd_daemon_ctx.user_sessions,
    lists:foreach(fun(Utuple) ->
                          Uinfo0 = econfd:mk_uinfo(Utuple),
                          Usid = Uinfo0#confd_user_info.usid,
                          Name = Uinfo0#confd_user_info.username,
                          case ets:lookup(UsessTab, Usid) of
                              [{_,OldUinfo}] ->
                                  %% update
                                  Uinfo = econfd:mk_uinfo(Utuple, OldUinfo);
                              _ ->
                                  ?confd_trace(Dx, "New user session ~p "
                                               "for user ~p\n", [Usid, Name]),
                                  Uinfo = Uinfo0
                          end,
                          ets:insert(UsessTab, {Usid, Uinfo})
                  end, Users),
    Dbx = init_db_ctx(Dx, Et, Op),
    db_reply(Dx, Dbx, ok);
confd_fd_ready(Dx, Et={Op=?CONFD_PROTO_CLOSE_USESS, _Qref, _Did,Usid}) ->
    ?confd_trace(Dx, "Close user session ~p", [Usid]),
    Dbx = init_db_ctx(Dx, Et, Op, Usid),
    ets:delete(Dx#confd_daemon_ctx.user_sessions, Usid),
    db_reply(Dx, Dbx, ok);
confd_fd_ready(Dx,{Op=?CONFD_PROTO_NEW_TRANS, Qref, _Did, _, Dbname, Usid,
                   Mode, TH, RequestData, HideInactive})->
    ?confd_trace(Dx, "CALL trans init(Thandle=~p)", [TH]),
    [{_,Uinfo}] = ets:lookup(Dx#confd_daemon_ctx.user_sessions, Usid),
    F = (Dx#confd_daemon_ctx.trans_cb)#confd_trans_cbs.init,
    Tctx = #confd_trans_ctx{dx = Dx, mode =Mode, dbname= Dbname, thandle=TH,
                            request_data = RequestData, uinfo = Uinfo,
                            hide_inactive = HideInactive /= 0,
                            lastop = Op, query_ref = Qref},
    try F(Tctx) of
        Result ->
            trans_reply(Tctx, Result)
    catch
        _:Rsn ->
            ?cerror_msg("new_trans failed: ~p~n", [Rsn]),
            trans_reply(Tctx, callback_failed())
    end;
confd_fd_ready(Dx, {Op=?CONFD_PROTO_NEW_VALIDATE, Qref, _Did, Dbname, Usid,
                    TH, _TestP, RequestData})->
    ?confd_trace(Dx, "CALL validate init(Thandle=~p)", [TH]),
    [{_,Uinfo}] = ets:lookup(Dx#confd_daemon_ctx.user_sessions, Usid),
    F = case Dx#confd_daemon_ctx.trans_validate_cb of
            undefined -> fun(_) -> ok end;
            Vcb -> null_1(Vcb#confd_trans_validate_cbs.init)
        end,
    Tctx = case econfd:trans_get(Dx, th, TH) of
               undefined ->
                   %% The case with a validator inside a daemon
                   %% that is not also a daataprovider
                   #confd_trans_ctx{dx = Dx, mode = ?CONFD_READ,
                                    dbname= Dbname, thandle=TH,
                                    uinfo = Uinfo,
                                    request_data = RequestData,
                                    hide_inactive = true,
                                    lastop = Op, query_ref = Qref};
               Old ->
                   %% we allready know about this TH since we're also
                   %% a dataprovider
                   Old#confd_trans_ctx{dx = Dx, reused = 1,
                                       request_data = RequestData,
                                       hide_inactive = true,
                                       lastop = Op, query_ref = Qref}
           end,
    econfd:trans_put(Dx, thvalidate, Tctx),
    try F(Tctx) of
        Result ->
            trans_reply(Tctx, Result)
    catch
        _:Rsn ->
            ?cerror_msg("new_validate failed: ~p~n", [Rsn]),
            trans_reply(Tctx, callback_failed())
    end;
confd_fd_ready(Dx, {Op=?CONFD_PROTO_NEW_ACTION,
                    Qref, Did, Usid, _CallPoint, _Index, TH})->
    ?confd_trace(Dx, "CALL action init(Usid=~p) --> OK\n", [Usid]),
    [{_,Uinfo}] = ets:lookup(Dx#confd_daemon_ctx.user_sessions, Usid),
    Actx = #confd_action_ctx{thandle = TH},
    ets:insert(Dx#confd_daemon_ctx.user_sessions,
               {Usid, Uinfo#confd_user_info{actx = Actx}}),
    Reply = {Op, Qref, Did, usid2th(Usid), Dx#confd_daemon_ctx.wint},
    term_write(Dx#confd_daemon_ctx.ctl, Reply);
confd_fd_ready(Dx, {Op=?CONFD_PROTO_ABORT_ACTION,
                    Qref, Did, TH})->
    _Usid = th2usid(TH),
    %% NYI for erlang
    Term = {Op, Qref, Did},
    term_write(Dx#confd_daemon_ctx.ctl, Term);

confd_fd_ready(Dx, {Op=?CONFD_PROTO_AUTH_CB,
                    Qref, Did, Utuple, Method, Result})->
    try
        #confd_authentication_cb{auth = Fun} =
            Dx#confd_daemon_ctx.authentication_cb,
        Uinfo = econfd:mk_uinfo(Utuple),
        Actx = case Result of
                   {true, Groups} ->
                       #confd_authentication_ctx{uinfo = Uinfo,
                                                 method = Method,
                                                 success = true,
                                                 ainfo = Groups};
                   {false, Logno, Reason} ->
                       #confd_authentication_ctx{uinfo = Uinfo,
                                                 method = Method,
                                                 success = false,
                                                 ainfo = {Logno, Reason}}
               end,
        ?confd_trace(Dx, "CALL auth(user=~p, ctx=~p, success=~p)",
                     [Uinfo#confd_user_info.username,
                      Uinfo#confd_user_info.context,
                      Actx#confd_authentication_ctx.success]),
        T = case Fun(Actx) of
                ok ->
                    ?confd_trace_ret(Dx, " --> OK\n", []),
                    {Op, Qref, Did, true};
                error ->
                    ?confd_trace_ret(Dx, " --> CONFD_ERR\n", []),
                    {Op, Qref, Did, {false, <<"">>}};
                {error, Msg} ->
                    ?confd_trace_ret(Dx, " --> CONFD_ERR: ~s\n", [?s2l(Msg)]),
                    {Op, Qref, Did, {false, Msg}};
                _ ->
                    ?confd_trace_ret(Dx, " --> BADRETVAL\n", []),
                    {Op, Qref, Did, {false, <<"">>}}
            end,
        term_write(Dx#confd_daemon_ctx.ctl,T)
    catch
        _:Rsn ->
            ?cerror_msg("econfd: ~p",[Rsn]),
            T2 = {Op, Qref, Did, error, mk_error(#confd_error{code=internal})},
            term_write(Dx#confd_daemon_ctx.ctl,T2)
    end;

confd_fd_ready(Dx, {Op=?CONFD_PROTO_NOTIF_GET_LOG_TIMES,
                    Qref,Did,Streamname,Index}) ->
    try
        {value, Ncb} = find_notif_cb(Streamname, Dx, Index),
        Nctx = #confd_notification_ctx{
          streamname = Streamname,
          notif_worker = Dx#confd_daemon_ctx.notif_worker,
          query_ref = Qref,
          opaque = Ncb#confd_notification_stream_cbs.opaque},
        Fun = Ncb#confd_notification_stream_cbs.get_log_times,
        T = case Fun(Nctx) of
                {ok, {Created, Aged}} ->
                    {Op, Qref, Did, Created, Aged};
                {error, Reason} ->
                    {Op, Qref, Did, error, mk_error(Reason)};
                _ ->
                    {Op, Qref, Did, error,
                     mk_error(#confd_error{code = proto_usage,
                                           str= <<"Bad return value">>})}
            end,
        term_write(Dx#confd_daemon_ctx.ctl,T)
    catch
        _:Rsn ->
            ?cerror_msg("econfd: ~p",[Rsn]),
            T2 = {Op, Qref, Did, error, mk_error(#confd_error{code=internal})},
            term_write(Dx#confd_daemon_ctx.ctl,T2)
    end;
confd_fd_ready(Dx, {Op=?CONFD_PROTO_NOTIF_REPLAY,
                    Qref,Did,Streamname,Index, SubId,
                    Estart, Estop}) ->
    try
        {value, Ncb} = find_notif_cb(Streamname, Dx, Index),
        Nctx = #confd_notification_ctx{
          streamname = Streamname,
          notif_worker = Dx#confd_daemon_ctx.notif_worker,
          query_ref = Qref,
          opaque = Ncb#confd_notification_stream_cbs.opaque,
          subid = SubId,
          flags = Dx#confd_daemon_ctx.flags
         },

        Fun = Ncb#confd_notification_stream_cbs.replay,
        T = case Fun(Nctx, Estart, Estop) of
                ok ->
                    {Op, Qref, Did, ok};
                {error, Reason} ->
                    {Op, Qref, Did, error, mk_error(Reason)};
                _ ->
                    {Op, Qref, Did, error,
                     mk_error(#confd_error{code = proto_usage,
                                           str = <<"Bad return value">>})}
            end,
        term_write(Dx#confd_daemon_ctx.ctl, T)
    catch
        _:Rsn ->
            ?cerror_msg("econfd: ~p\n",[Rsn]),
            T2 = {Op, Qref, Did, error, mk_error(#confd_error{code=internal})},
            term_write(Dx#confd_daemon_ctx.ctl, T2)
    end;

%% transaction callbacks
confd_fd_ready(Dx, {Op=?CONFD_PROTO_TRANS_LOCK, Qref,_Did,TH,
                    RequestData,RealUsid,HideInactive})->
    ?confd_trace(Dx, "CALL trans_lock()",[]),
    {TransCb, Tctx} = null_trans_handler(Dx,TH,Qref,Op,RequestData,
                                         RealUsid,HideInactive),
    trans_reply(Tctx,cc1(TransCb#confd_trans_cbs.trans_lock, Tctx));
confd_fd_ready(Dx, {Op=?CONFD_PROTO_TRANS_UNLOCK, Qref,_Did,TH,
                    RequestData,RealUsid,HideInactive})->
    ?confd_trace(Dx, "CALL trans_unlock()",[]),
    {TransCb, Tctx} = null_trans_handler(Dx,TH,Qref,Op,RequestData,
                                         RealUsid,HideInactive),
    trans_reply(Tctx,cc1(TransCb#confd_trans_cbs.trans_unlock,Tctx));
confd_fd_ready(Dx, {Op=?CONFD_PROTO_WRITE_START, Qref,_Did,TH,
                    RequestData,RealUsid,HideInactive})->
    ?confd_trace(Dx,"CALL write_start()",[]),
    {TransCb, Tctx} = null_trans_handler(Dx,TH,Qref,Op,RequestData,
                                         RealUsid,HideInactive),
    trans_reply(Tctx,cc1(TransCb#confd_trans_cbs.write_start,Tctx));
confd_fd_ready(Dx, {Op=?CONFD_PROTO_PREPARE, Qref,_Did,TH,
                    RequestData,RealUsid,HideInactive})->
    ?confd_trace(Dx, "CALL prepare()",[]),
    {TransCb, Tctx} = null_trans_handler(Dx,TH,Qref,Op,RequestData,
                                         RealUsid,HideInactive),
    trans_reply(Tctx,cc1(TransCb#confd_trans_cbs.prepare,Tctx));
confd_fd_ready(Dx, {Op=?CONFD_PROTO_ABORT, _Qref,_Did,TH,
                    RequestData,RealUsid,HideInactive})->
    ?confd_trace(Dx, "CALL abort()",[]),
    {TransCb, Tctx} = null_trans_handler(Dx,TH,_Qref,Op,RequestData,
                                         RealUsid,HideInactive),
    trans_reply(Tctx,cc1(TransCb#confd_trans_cbs.abort,Tctx));
confd_fd_ready(Dx, {Op=?CONFD_PROTO_COMMIT, Qref,_Did,TH,
                    RequestData,RealUsid,HideInactive})->
    ?confd_trace(Dx, "CALL commit()",[]),
    {TransCb, Tctx} = null_trans_handler(Dx,TH,Qref,Op,RequestData,
                                         RealUsid,HideInactive),
    trans_reply(Tctx,cc1(TransCb#confd_trans_cbs.commit,Tctx));
confd_fd_ready(Dx, {Op=?CONFD_PROTO_CLOSE_TRANS, Qref,_Did,TH,
                    RequestData,RealUsid,HideInactive})->
    ?confd_trace(Dx, "CALL finish()",[]),
    ets:match_delete(confd_next_map_from_int, {{TH, '_'}, '_', '_'}),
    {TransCb, Tctx} = null_trans_handler(Dx,TH,Qref,Op,RequestData,
                                         RealUsid,HideInactive),
    econfd:trans_erase(Dx, th, TH),
    trans_reply(Tctx,cc1(TransCb#confd_trans_cbs.finish,Tctx));
confd_fd_ready(Dx, {Op=?CONFD_PROTO_CLOSE_VALIDATE, Qref,_Did,TH,_CommitP,
                    RequestData,RealUsid,_HideInactive})->
    ?confd_trace(Dx, "CALL validate stop()",[]),
    {ValidateCb, Tctx} = null_validate_handler(Dx,TH,Qref,Op,
                                               RequestData,RealUsid),
    econfd:trans_erase(Dx, thvalidate, TH),
    trans_reply(Tctx,cc1(ValidateCb#confd_trans_validate_cbs.stop,Tctx));

%% validate callback
confd_fd_ready(Dx,ET) when element(1, ET) == ?CONFD_PROTO_CALLBACK,
                           element(7,ET) == ?CONFD_VALIDATE_VALUE ->
    Qref = element(2, ET),
    case element(5,ET) of
        {Point, Opaque} -> ok;
        Point           -> Opaque = undefined
    end,
    TH=element(4,ET),
    case econfd:trans_get(Dx, thvalidate, TH) of
        undefined ->
            report_err(Dx, ?CONFD_DEBUG, "No transaction found",[]);
        Tctx0->
            RequestData = element(8, ET),
            Usid = element(9, ET),
            Uinfo = update_uinfo(Dx, Tctx0#confd_trans_ctx.uinfo, Usid),
            Tctx1=Tctx0#confd_trans_ctx{dx=Dx, query_ref=Qref, uinfo=Uinfo,
                                        lastop=?CONFD_PROTO_CALLBACK,
                                        last_proto_op = ?CONFD_VALIDATE_VALUE,
                                        request_data = RequestData},
            econfd:trans_put(Dx, thvalidate, Tctx1),
            Tctx = Tctx1#confd_trans_ctx{callpoint_opaque = Opaque},
            Index = element(6, ET),
            case tag_to_valcbs(Dx,Point,Index) of
                false ->
                    report_err(Dx, ?CONFD_DEBUG, "No valpoint found",[]);
                {value, ValpointCb} ->
                    ArgL = element(11,ET),
                    [Kp, NewVal] = ArgL,
                    case ValpointCb#confd_valpoint_cb.validate of
                        undefined ->
                            no_cb_err(?CONFD_VALIDATE_VALUE, Tctx, "validate");
                        F ->
                            try F(Tctx,Kp,NewVal) of
                                Ret ->
                                    proto_reply(?CONFD_VALIDATE_VALUE,
                                                Tctx,2,Ret)
                            catch
                                _:Rsn ->
                                    ?cerror_msg("callback failed: ~p~n",[Rsn]),
                                    proto_reply(?CONFD_VALIDATE_VALUE,Tctx, 2,
                                                callback_failed())
                            end

                    end
            end
    end;

%% action callbacks
confd_fd_ready(Dx,ET) when element(1, ET) == ?CONFD_PROTO_CALLBACK,
                           element(7,ET) == ?CONFD_CALL_ACTION;
                           element(7,ET) == ?CONFD_CALL_ACTION_COMMAND;
                           element(7, ET) == ?CONFD_CALL_ACTION_COMPLETION ->
    Qref = element(2, ET),
    case element(5,ET) of
        {Point, Opaque} -> ok;
        Point           -> Opaque = undefined
    end,
    TH=element(4,ET),
    Usid = th2usid(TH),
    [{_,Uinfo0}] = ets:lookup(Dx#confd_daemon_ctx.user_sessions, Usid),
    Index = element(6, ET),
    case tag_to_actcbs(Dx,Point,Index) of
        false ->
            report_err(Dx, ?CONFD_DEBUG,
                       "No action callback for ~p",[Point]);
        {value, ActionCb} ->
            Actx0 = Uinfo0#confd_user_info.actx,
            Actx = Actx0#confd_action_ctx{dx = Dx,
                                          actionpoint_opaque = Opaque,
                                          query_ref = Qref},
            Uinfo = Uinfo0#confd_user_info{actx = Actx},
            Type = element(7, ET),
            ArgL = element(11, ET),
            action_callback(Dx, Qref, Uinfo, ActionCb, Type, ArgL)
    end;

%% data callbacks and NCS service callbacks
confd_fd_ready(Dx,ET) when element(1, ET) == ?CONFD_PROTO_CALLBACK ->
    Qref = element(2, ET),
    case element(5,ET) of
        {Point, Opaque} -> ok;
        Point           -> Opaque = undefined
    end,
    TH=element(4,ET),
    case econfd:trans_get(Dx, th, TH) of
        undefined ->
            report_err(Dx, ?CONFD_DEBUG, "No transaction found",[]);
        Tctx0->
            RequestData = element(8, ET),
            Usid = element(9, ET),
            Uinfo = update_uinfo(Dx, Tctx0#confd_trans_ctx.uinfo, Usid),
            HideInactive = element(10, ET),
            Cb = element(7, ET),
            Tctx1=Tctx0#confd_trans_ctx{dx=Dx, query_ref=Qref, uinfo=Uinfo,
                                        lastop=?CONFD_PROTO_CALLBACK,
                                        last_proto_op = Cb,
                                        request_data = RequestData,
                                        hide_inactive = HideInactive /= 0},
            econfd:trans_put(Dx, th, Tctx1),
            Tctx = Tctx1#confd_trans_ctx{callpoint_opaque = Opaque},
            Index = element(6, ET),
            ArgL = element(11, ET),
            if Cb == ?CONFD_SERVICE_CB_PRE_MODIFICATION;
               Cb == ?CONFD_SERVICE_CB_POST_MODIFICATION;
               Cb == ?CONFD_SERVICE_CB_PRE_LOCK_CREATE;
               Cb == ?CONFD_SERVICE_CB_CREATE ->
                    %% service callback
                    case tag_to_srvcbs(Dx,Point,Index) of
                        false ->
                            report_err(Dx, ?CONFD_DEBUG,
                                       "No service callbacks for ~p", [Point]);
                        {value, SrvCb} ->
                            try service_callback(Dx, Cb, Tctx, SrvCb, ArgL) of
                                Ret -> Ret
                            catch
                                _:Rsn ->
                                    report_err(Tctx#confd_trans_ctx.dx,
                                               ?CONFD_DEBUG,
                                               "Sevice callback for Op=~p "
                                               "failed:~p ~p",
                                               [Cb, Rsn,
                                                erlang:get_stacktrace()]),
                                    service_reply(Cb, Tctx, callback_failed())
                            end
                    end;
               Cb == ?CONFD_NANO_SERVICE_CB_CREATE;
               Cb == ?CONFD_NANO_SERVICE_CB_DELETE ->
                    %% nano service callback
                    case tag_to_nanosrvcbs(Dx,Point,Index) of
                        false ->
                            report_err(Dx, ?CONFD_DEBUG,
                                       "No nano service callbacks for ~p",
                                       [Point]);
                        {value, NanoSrvCb} ->
                            try nano_service_callback(Dx, Cb, Tctx,
                                                      NanoSrvCb, ArgL) of
                                Ret -> Ret
                            catch
                                _:Rsn ->
                                    report_err(Tctx#confd_trans_ctx.dx,
                                               ?CONFD_DEBUG,
                                               "NanoService callback for Op=~p "
                                               "failed:~p ~p",
                                               [Cb, Rsn,
                                                erlang:get_stacktrace()]),
                                    service_reply(Cb, Tctx, callback_failed())
                            end
                    end;
                true ->
                    %% data callback
                    case tag_to_datacbs(Dx,Point,Index) of
                        false ->
                            report_err(Dx, ?CONFD_DEBUG,
                                       "No data callbacks for ~p", [Point]);
                        {value, DataCb} ->
                            try proto_callback(Dx, Cb, Tctx, DataCb, ArgL) of
                                Ret -> Ret
                            catch _:Rsn ->
                                    report_err(Tctx#confd_trans_ctx.dx,
                                               ?CONFD_DEBUG,
                                               "Data callback for Op=~p "
                                               "failed:~p ~p",
                                               [Cb, Rsn,
                                                erlang:get_stacktrace()]),
                                    proto_reply(Cb, Tctx, 0,
                                                callback_failed())
                            end
                    end
            end
    end;

%% db callbacks (on control socket)
confd_fd_ready(Dx, Et={Op=?CONFD_PROTO_CANDIDATE_COMMIT,
                       _Qref,_Did, Usid, Timeout})->
    Dbx = init_db_ctx(Dx, Et, Op, Usid),
    DbCb = Dx#confd_daemon_ctx.db_cb,
    ?confd_trace(Dx, "CALL db candidate_commit(~p)",[Timeout]),
    if
        DbCb == undefined ->
            db_reply(Dx, Dbx, ok);
        DbCb#confd_db_cbs.candidate_commit == undefined ->
            db_reply(Dx, Dbx, ok);
        true ->
            F = DbCb#confd_db_cbs.candidate_commit,
            db_reply(Dx, Dbx, F(Dbx, Timeout))
    end;
confd_fd_ready(Dx, Et={Op=?CONFD_PROTO_CANDIDATE_CONFIRMING_COMMIT,
                       _Qref,_Did, Usid})->
    Dbx = init_db_ctx(Dx, Et, Op, Usid),
    DbCb = Dx#confd_daemon_ctx.db_cb,
    ?confd_trace(Dx, "CALL db candidate_confirming_commit()",[]),
    if
        DbCb == undefined ->
            db_reply(Dx, Dbx, ok);
        DbCb#confd_db_cbs.candidate_confirming_commit == undefined ->
            db_reply(Dx, Dbx, ok);
        true ->
            F = DbCb#confd_db_cbs.candidate_confirming_commit,
            db_reply(Dx, Dbx, F(Dbx))
    end;
confd_fd_ready(Dx, Et={Op=?CONFD_PROTO_CANDIDATE_RESET,
                       _Qref,_Did, Usid})->
    Dbx = init_db_ctx(Dx, Et, Op, Usid),
    DbCb = Dx#confd_daemon_ctx.db_cb,
    ?confd_trace(Dx, "CALL db candidate_reset()",[]),
    if
        DbCb == undefined ->
            db_reply(Dx, Dbx, ok);
        DbCb#confd_db_cbs.candidate_reset == undefined ->
            db_reply(Dx, Dbx, ok);
        true ->
            F = DbCb#confd_db_cbs.candidate_reset,
            db_reply(Dx, Dbx, F(Dbx))
    end;
confd_fd_ready(Dx, Et={Op=?CONFD_PROTO_CANDIDATE_CHK_NOT_MODIFIED,
                       _Qref,_Did, Usid})->
    Dbx = init_db_ctx(Dx, Et, Op, Usid),
    DbCb = Dx#confd_daemon_ctx.db_cb,
    ?confd_trace(Dx, "CALL db candidate_chk_not_modified()",[]),
    if
        DbCb == undefined ->
            db_reply(Dx, Dbx, ok);
        DbCb#confd_db_cbs.candidate_chk_not_modified == undefined ->
            db_reply(Dx, Dbx, ok);
        true ->
            F = DbCb#confd_db_cbs.candidate_chk_not_modified,
            db_reply(Dx, Dbx, F(Dbx))
    end;
confd_fd_ready(Dx, Et={Op=?CONFD_PROTO_CANDIDATE_ROLLBACK_RUNNING,
                       _Qref,_Did, Usid})->
    Dbx = init_db_ctx(Dx, Et, Op, Usid),
    DbCb = Dx#confd_daemon_ctx.db_cb,
    ?confd_trace(Dx, "CALL db candidate_rollback_running()",[]),
    if
        DbCb == undefined ->
            db_reply(Dx, Dbx, ok);
        DbCb#confd_db_cbs.candidate_rollback_running == undefined ->
            db_reply(Dx, Dbx, ok);
        true ->
            F = DbCb#confd_db_cbs.candidate_rollback_running,
            db_reply(Dx, Dbx, F(Dbx))
    end;
confd_fd_ready(Dx, Et={Op=?CONFD_PROTO_CANDIDATE_VALIDATE,
                       _Qref,_Did, Usid})->
    Dbx = init_db_ctx(Dx, Et, Op, Usid),
    DbCb = Dx#confd_daemon_ctx.db_cb,
    ?confd_trace(Dx, "CALL db candidate_validate()",[]),
    if
        DbCb == undefined ->
            db_reply(Dx, Dbx, ok);
        DbCb#confd_db_cbs.candidate_validate == undefined ->
            db_reply(Dx, Dbx, ok);
        true ->
            F = DbCb#confd_db_cbs.candidate_validate,
            db_reply(Dx, Dbx, F(Dbx))
    end;
confd_fd_ready(Dx, Et={Op=?CONFD_PROTO_ADD_CHECKPOINT_RUNNING,
                       _Qref,_Did, Usid})->
    Dbx = init_db_ctx(Dx, Et, Op, Usid),
    DbCb = Dx#confd_daemon_ctx.db_cb,
    ?confd_trace(Dx, "CALL db add_checkpoint_running()",[]),
    if
        DbCb == undefined ->
            db_reply(Dx, Dbx, ok);
        DbCb#confd_db_cbs.add_checkpoint_running == undefined ->
            db_reply(Dx, Dbx, ok);
        true ->
            F = DbCb#confd_db_cbs.add_checkpoint_running,
            db_reply(Dx, Dbx, F(Dbx))
    end;
confd_fd_ready(Dx, Et={Op=?CONFD_PROTO_DEL_CHECKPOINT_RUNNING,
                       _Qref,_Did, Usid})->
    Dbx = init_db_ctx(Dx, Et, Op, Usid),
    DbCb = Dx#confd_daemon_ctx.db_cb,
    ?confd_trace(Dx, "CALL db del_checkpoint_running()",[]),
    if
        DbCb == undefined ->
            db_reply(Dx, Dbx, ok);
        DbCb#confd_db_cbs.del_checkpoint_running == undefined ->
            db_reply(Dx, Dbx, ok);
        true ->
            F = DbCb#confd_db_cbs.del_checkpoint_running,
            db_reply(Dx, Dbx, F(Dbx))
    end;
confd_fd_ready(Dx, Et={Op=?CONFD_PROTO_ACTIVATE_CHECKPOINT_RUNNING,
                       _Qref,_Did, Usid})->
    Dbx = init_db_ctx(Dx, Et, Op, Usid),
    DbCb = Dx#confd_daemon_ctx.db_cb,
    ?confd_trace(Dx, "CALL db activate_checkpoint_running()",[]),
    if
        DbCb == undefined ->
            db_reply(Dx, Dbx, ok);
        DbCb#confd_db_cbs.activate_checkpoint_running == undefined ->
            db_reply(Dx, Dbx, ok);
        true ->
            F = DbCb#confd_db_cbs.activate_checkpoint_running,
            db_reply(Dx, Dbx, F(Dbx))
    end;
confd_fd_ready(Dx, Et={Op=?CONFD_PROTO_COPY_RUNNING_TO_STARTUP,
                       _Qref,_Did, Usid})->
    Dbx = init_db_ctx(Dx, Et, Op, Usid),
    DbCb = Dx#confd_daemon_ctx.db_cb,
    ?confd_trace(Dx, "CALL db copy_running_to_startup()",[]),
    if
        DbCb == undefined ->
            db_reply(Dx, Dbx, ok);
        DbCb#confd_db_cbs.copy_running_to_startup == undefined ->
            db_reply(Dx, Dbx, ok);
        true ->
            F = DbCb#confd_db_cbs.copy_running_to_startup,
            db_reply(Dx, Dbx, F(Dbx))
    end;
confd_fd_ready(Dx, Et={Op=?CONFD_PROTO_RUNNING_CHK_NOT_MODIFIED,
                       _Qref,_Did, Usid})->
    Dbx = init_db_ctx(Dx, Et, Op, Usid),
    DbCb = Dx#confd_daemon_ctx.db_cb,
    ?confd_trace(Dx, "CALL db running_chk_not_modified()",[]),
    if
        DbCb == undefined ->
            db_reply(Dx, Dbx, ok);
        DbCb#confd_db_cbs.running_chk_not_modified == undefined ->
            db_reply(Dx, Dbx, ok);
        true ->
            F = DbCb#confd_db_cbs.running_chk_not_modified,
            db_reply(Dx, Dbx, F(Dbx))
    end;
confd_fd_ready(Dx, Et={Op=?CONFD_PROTO_LOCK, _Qref,_Did, Usid, DbName})->
    Dbx = init_db_ctx(Dx, Et, Op, Usid),
    DbCb = Dx#confd_daemon_ctx.db_cb,
    ?confd_trace(Dx, "CALL db lock(~p)",[DbName]),
    if
        DbCb == undefined ->
            db_reply(Dx, Dbx, ok);
        DbCb#confd_db_cbs.lock == undefined ->
            db_reply(Dx, Dbx, ok);
        true ->
            F = DbCb#confd_db_cbs.lock,
            db_reply(Dx, Dbx, F(Dbx, DbName))
    end;
confd_fd_ready(Dx, Et={Op=?CONFD_PROTO_UNLOCK, _Qref,_Did, Usid, DbName})->
    Dbx = init_db_ctx(Dx, Et, Op, Usid),
    DbCb = Dx#confd_daemon_ctx.db_cb,
    ?confd_trace(Dx, "CALL db unlock(~p)",[DbName]),
    if
        DbCb == undefined ->
            db_reply(Dx, Dbx, ok);
        DbCb#confd_db_cbs.unlock == undefined ->
            db_reply(Dx, Dbx, ok);
        true ->
            F = DbCb#confd_db_cbs.unlock,
            db_reply(Dx, Dbx, F(Dbx, DbName))
    end;
confd_fd_ready(Dx, Et={Op=?CONFD_PROTO_LOCK_PARTIAL,
                       _Qref, _Did, Usid, DbName, LockId, PathList})->
    Dbx = init_db_ctx(Dx, Et, Op, Usid),
    DbCb = Dx#confd_daemon_ctx.db_cb,
    ?confd_trace(Dx, "CALL db lock_partial(~p)",[DbName]),
    if
        DbCb == undefined ->
            db_reply(Dx, Dbx, ok);
        DbCb#confd_db_cbs.lock_partial == undefined ->
            db_reply(Dx, Dbx, ok);
        true ->
            F = DbCb#confd_db_cbs.lock_partial,
            db_reply(Dx, Dbx, F(Dbx, DbName, LockId, PathList))
    end;
confd_fd_ready(Dx, Et={Op=?CONFD_PROTO_UNLOCK_PARTIAL,
                       _Qref, _Did, Usid, DbName, LockId})->
    Dbx = init_db_ctx(Dx, Et, Op, Usid),
    DbCb = Dx#confd_daemon_ctx.db_cb,
    ?confd_trace(Dx, "CALL db unlock_partial(~p)",[DbName]),
    if
        DbCb == undefined ->
            db_reply(Dx, Dbx, ok);
        DbCb#confd_db_cbs.unlock_partial == undefined ->
            db_reply(Dx, Dbx, ok);
        true ->
            F = DbCb#confd_db_cbs.unlock_partial,
            db_reply(Dx, Dbx, F(Dbx, DbName, LockId))
    end;
confd_fd_ready(Dx, Et={Op=?CONFD_PROTO_DELETE_CONFIG,
                       _Qref,_Did, Usid, DbName})->
    Dbx = init_db_ctx(Dx, Et, Op, Usid),
    DbCb = Dx#confd_daemon_ctx.db_cb,
    ?confd_trace(Dx, "CALL db delete_config(~p)",[DbName]),
    if
        DbCb == undefined ->
            db_reply(Dx, Dbx, ok);
        DbCb#confd_db_cbs.delete_config == undefined ->
            db_reply(Dx, Dbx, ok);
        true ->
            F = DbCb#confd_db_cbs.delete_config,
            db_reply(Dx, Dbx, F(Dbx, DbName))
    end.


-define(get_data_cb(Tctx,DC,Item),
               if DC#confd_data_cbs.Item == undefined ->
                       undefined;
                  true ->
                       DC#confd_data_cbs.Item
               end).

proto_callback(Dx, Op=?CONFD_DATA_CB_GET_NEXT, Tctx0, DataCb, ArgL) ->
    [KP, Prev, Index] = ArgL,
    Tctx = Tctx0#confd_trans_ctx{secondary_index = Index},
    TH = Tctx#confd_trans_ctx.thandle,
    Ekey = {TH, Prev},
    RealKey = if Prev /= -1 ->
                      [{_, RK, Ints}]=ets:lookup(confd_next_map_from_int, Ekey),
                      [ets:delete(confd_next_map_from_int, {TH, Int}) ||
                          Int <- Ints],
                      RK;
                 true ->
                      -1
              end,
    ?confd_trace(Dx, "CALL get_next(~s, ~p)", [pp_kpath(KP), RealKey]),
    case ?get_data_cb(Tctx, DataCb, get_next) of
        undefined ->
            no_cb_err(Op, Tctx, "get_next", KP);
        F ->
            proto_reply(Op, Tctx, 1, F(Tctx, KP, RealKey))
    end;
proto_callback(Dx, Op=?CONFD_DATA_CB_FIND_NEXT, Tctx0, DataCb, ArgL) ->
    [KP, {Type, Key}, Index] = ArgL,
    Tctx = Tctx0#confd_trans_ctx{secondary_index = Index},
    ?confd_trace(Dx, "CALL find_next(~s, ~p)", [pp_kpath(KP), Key]),
    case ?get_data_cb(Tctx, DataCb, find_next) of
        undefined ->
            no_cb_err(Op, Tctx, "find_next", KP);
        F ->
            proto_reply(Op, Tctx, 1, F(Tctx, KP, Type, Key))
    end;
proto_callback(Dx, Op=?CONFD_DATA_CB_NUM_INSTANCES, Tctx, DataCb, ArgL) ->
    [KP] = ArgL,
    ?confd_trace(Dx, "CALL get_num_instances(~s)", [pp_kpath(KP)]),
    case ?get_data_cb(Tctx, DataCb, num_instances) of
        undefined ->
            %% try get_next
            case ?get_data_cb(Tctx, DataCb, get_next) of
                undefined ->
                    no_cb_err(Op, Tctx, "get_next", KP);
                F ->
                    N = count_instances(Tctx, KP, F, 0, -1),
                    proto_reply(Op, Tctx, 1, N)
            end;
        F2 ->
            proto_reply(Op, Tctx, 1, F2(Tctx, KP))
    end;
proto_callback(Dx, Op=?CONFD_DATA_CB_GET_ELEM, Tctx, DataCb, ArgL) ->
    [KP] = ArgL,
    ?confd_trace(Dx, "CALL get_elem(~s)", [pp_kpath(KP)]),
    case ?get_data_cb(Tctx, DataCb, get_elem) of
        undefined ->
            no_cb_err(Op, Tctx, "get_elem", KP);
        F ->
            proto_reply(Op, Tctx, 1, F(Tctx, KP))
    end;
proto_callback(Dx, Op=?CONFD_DATA_CB_GET_OBJECT, Tctx, DataCb, ArgL) ->
    [KP] = ArgL,
    ?confd_trace(Dx, "CALL get_object(~s)", [pp_kpath(KP)]),
    case ?get_data_cb(Tctx, DataCb, get_object) of
        undefined ->
            econfd:data_reply_not_found(Tctx);
        F ->
            proto_reply(Op, Tctx, 1, F(Tctx, KP))
    end;
proto_callback(Dx, Op=?CONFD_DATA_CB_GET_NEXT_OBJECT, Tctx0, DataCb, ArgL) ->
    [KP, Prev, Index] = ArgL,
    Tctx = Tctx0#confd_trans_ctx{secondary_index = Index},
    TH = Tctx#confd_trans_ctx.thandle,
    Ekey = {TH, Prev},
    RealKey = if Prev /= -1 ->
                      [{_, RK, Ints}]=ets:lookup(confd_next_map_from_int, Ekey),
                      [ets:delete(confd_next_map_from_int, {TH, Int}) ||
                          Int <- Ints],
                      RK;
                 true ->
                      -1
              end,
    ?confd_trace(Dx, "CALL get_next_object(~s, ~p)", [pp_kpath(KP), RealKey]),
    case ?get_data_cb(Tctx, DataCb, get_next_object) of
        undefined ->
            no_cb_err(Op, Tctx, "get_next_object", KP);
        F ->
            proto_reply(Op, Tctx, 1, F(Tctx, KP, RealKey))
    end;
proto_callback(Dx, Op=?CONFD_DATA_CB_FIND_NEXT_OBJECT, Tctx0, DataCb, ArgL) ->
    [KP, {Type, Key}, Index] = ArgL,
    Tctx = Tctx0#confd_trans_ctx{secondary_index = Index},
    ?confd_trace(Dx, "CALL find_next_object(~s, ~p)", [pp_kpath(KP), Key]),
    case ?get_data_cb(Tctx, DataCb, find_next_object) of
        undefined ->
            no_cb_err(Op, Tctx, "find_next_object", KP);
        F ->
            proto_reply(Op, Tctx, 1, F(Tctx, KP, Type, Key))
    end;
proto_callback(Dx, Op=?CONFD_DATA_CB_SET_ELEM, Tctx, DataCb, ArgL) ->
    [KP, Val|_] = ArgL,
    ?confd_trace(Dx, "CALL set_elem(~s, ~s)", [pp_kpath(KP),
                                           econfd:pp_value(Val)]),
    case ?get_data_cb(Tctx, DataCb, set_elem) of
        undefined ->
            no_cb_err(Op, Tctx, "set_elem", KP);
        F ->
            proto_reply(Op, Tctx, 1, F(Tctx, KP, Val))
    end;

proto_callback(Dx, Op=?CONFD_DATA_CB_CREATE, Tctx, DataCb, ArgL) ->
    [KP|_] = ArgL,
    ?confd_trace(Dx, "CALL create(~s)", [pp_kpath(KP)]),
    case ?get_data_cb(Tctx, DataCb, create) of
        undefined ->
            no_cb_err(Op, Tctx, "create", KP);
        F ->
            proto_reply(Op, Tctx, 1, F(Tctx, KP))
    end;
proto_callback(Dx, Op=?CONFD_DATA_CB_EXISTS_OPTIONAL, Tctx, DataCb, ArgL) ->
    [KP|_] = ArgL,
    ?confd_trace(Dx, "CALL exists_optional(~s)", [pp_kpath(KP)]),
    case ?get_data_cb(Tctx, DataCb, exists_optional) of
        undefined ->
            no_cb_err(Op, Tctx, "exists_optional", KP);
        F ->
            proto_reply(Op, Tctx, 1, F(Tctx, KP))
    end;
proto_callback(Dx, Op=?CONFD_DATA_CB_DELETE, Tctx, DataCb, ArgL) ->
    [KP|_] = ArgL,
    ?confd_trace(Dx, "CALL delete(~s)", [pp_kpath(KP)]),
    case ?get_data_cb(Tctx, DataCb, remove) of
        undefined ->
            no_cb_err(Op,Tctx, "delete", KP);
        F ->
            proto_reply(Op, Tctx, 1, F(Tctx, KP))
    end;
proto_callback(Dx, Op=?CONFD_DATA_CB_GET_CASE, Tctx, DataCb, ArgL) ->
    [KP, Choice|_] = ArgL,
    ?confd_trace(Dx, "CALL get_case(~s, ~s)", [pp_kpath(KP),
                                               pp_choice(Choice)]),
    case ?get_data_cb(Tctx, DataCb, get_case) of
        undefined ->
            no_cb_err(Op, Tctx, "get_case", KP);
        F ->
            proto_reply(Op, Tctx, 1, F(Tctx, KP, Choice))
    end;
proto_callback(Dx, Op=?CONFD_DATA_CB_SET_CASE, Tctx, DataCb, ArgL) ->
    [KP, Choice, Case|_] = ArgL,
    ?confd_trace(Dx, "CALL set_case(~s, ~s, ~s)", [pp_kpath(KP),
                                                   pp_choice(Choice),
                                                   econfd:pp_value(Case)]),
    case ?get_data_cb(Tctx, DataCb, set_case) of
        undefined ->
            no_cb_err(Op, Tctx, "set_case", KP);
        F ->
            proto_reply(Op, Tctx, 0, F(Tctx, KP, Choice, Case))
    end;
proto_callback(Dx, Op=?CONFD_DATA_CB_GET_ATTRS, Tctx, DataCb, ArgL) ->
    [KP, AttrL|_] = ArgL,
    ?confd_trace(Dx, "CALL get_attrs(~s, ~p)", [pp_kpath(KP), AttrL]),
    case ?get_data_cb(Tctx, DataCb, get_attrs) of
        undefined ->
            no_cb_err(Op, Tctx, "get_attrs", KP);
        F ->
            proto_reply(Op, Tctx, 1, F(Tctx, KP, AttrL))
    end;
proto_callback(Dx, Op=?CONFD_DATA_CB_SET_ATTR, Tctx, DataCb, ArgL) ->
    [KP, Attr, V|_] = ArgL,
    ?confd_trace(Dx, "CALL set_attr(~s, ~w, ~s)", [pp_kpath(KP), Attr,
                                                   econfd:pp_value(V)]),
    case ?get_data_cb(Tctx, DataCb, set_attr) of
        undefined ->
            no_cb_err(Op, Tctx, "set_attr", KP);
        F ->
            proto_reply(Op, Tctx, 0, F(Tctx, KP, Attr, V))
    end;
proto_callback(Dx, Op=?CONFD_DATA_CB_MOVE_AFTER, Tctx, DataCb, ArgL) ->
    [KP, PrevKeys|_] = ArgL,
    ?confd_trace(Dx, "CALL move_after(~s)", [pp_kpath(KP)]),
    case ?get_data_cb(Tctx, DataCb, move_after) of
        undefined ->
            no_cb_err(Op, Tctx, "move_after", KP);
        F ->
            proto_reply(Op, Tctx, 0, F(Tctx, KP, PrevKeys))
    end;
proto_callback(Dx, Op=?CONFD_DATA_CB_WRITE_ALL, Tctx, DataCb, _ArgL) ->
    %% no args
    ?confd_trace(Dx, "CALL write_all()", []),
    case ?get_data_cb(Tctx, DataCb, write_all) of
        undefined ->
            no_cb_err(Op, Tctx, "write_all");
        F ->
            proto_reply(Op, Tctx, 0, F(Tctx, []))
    end.


service_callback(Dx, Op=?CONFD_SERVICE_CB_PRE_MODIFICATION,
                 Tctx, SrvCb, ArgL) ->
    [Type, KP, Proplist|_] = ArgL,
    ?confd_trace(Dx, "CALL service pre_call(~p, ~s)", [Type, pp_kpath(KP)]),
    case SrvCb#ncs_service_cbs.pre_modification of
        undefined ->
            no_cb_err(Op, Tctx, "service pre_modification");
        F ->
            service_reply(Op, Tctx, F(Tctx, Type, KP, Proplist))
    end;
service_callback(Dx, Op=?CONFD_SERVICE_CB_POST_MODIFICATION,
                 Tctx, SrvCb, ArgL) ->
    [Type, KP, Proplist|_] = ArgL,
    ?confd_trace(Dx, "CALL service post_modification(~p, ~s)",
                 [Type, pp_kpath(KP)]),
    case SrvCb#ncs_service_cbs.post_modification of
        undefined ->
            no_cb_err(Op, Tctx, "service post_modification");
        F ->
            service_reply(Op, Tctx, F(Tctx, Type, KP, Proplist))
    end;
service_callback(Dx, Op=?CONFD_SERVICE_CB_PRE_LOCK_CREATE, Tctx, SrvCb, ArgL) ->
    [KP, Proplist, TransInTrans|_] = ArgL,
    ?confd_trace(Dx, "CALL service pre_lock_create(~s)", [pp_kpath(KP)]),
    case SrvCb#ncs_service_cbs.pre_lock_create of
        undefined ->
            no_cb_err(Op, Tctx, "service pre_lock_create");
        F ->
            service_reply(Op, Tctx, F(Tctx, KP, Proplist, TransInTrans))
    end;
service_callback(Dx, Op=?CONFD_SERVICE_CB_CREATE, Tctx, SrvCb, ArgL) ->
    [KP, Proplist, TransInTrans|_] = ArgL,
    ?confd_trace(Dx, "CALL service create(~s)", [pp_kpath(KP)]),
    case SrvCb#ncs_service_cbs.create of
        undefined ->
            no_cb_err(Op, Tctx, "service create");
        F ->
            service_reply(Op, Tctx, F(Tctx, KP, Proplist, TransInTrans))
    end.

nano_service_callback(Dx, Op=?CONFD_NANO_SERVICE_CB_CREATE,
                      Tctx, NanoSrvCb, ArgL) ->
    [KP, Proplist, TransInTrans|_] = ArgL,
    [State, state, Component, component, plan | KP2] = KP,
    ?confd_trace(Dx, "CALL nano service create(~s)", [pp_kpath(KP)]),
    case NanoSrvCb#ncs_nano_service_cbs.create of
        undefined ->
            no_cb_err(Op, Tctx, "nano service create");
        F ->
            service_reply(Op, Tctx, F(Tctx, KP2, Component, State,
                                      Proplist, TransInTrans))
    end;
nano_service_callback(Dx, Op=?CONFD_NANO_SERVICE_CB_DELETE,
                      Tctx, NanoSrvCb, ArgL) ->
    [KP, Proplist, TransInTrans|_] = ArgL,
    [State, state, Component, component, plan | KP2] = KP,
    ?confd_trace(Dx, "CALL nano service delete(~s)", [pp_kpath(KP)]),
    case NanoSrvCb#ncs_nano_service_cbs.delete of
        undefined ->
            no_cb_err(Op, Tctx, "nano service delete");
        F ->
            service_reply(Op, Tctx, F(Tctx, KP2, Component, State,
                                      Proplist, TransInTrans))
    end.

service_reply(_Op, Tctx0, RetVal0) ->
    case RetVal0 of
        {ok, R, #confd_trans_ctx{} = Tctx} ->
            RetVal = {ok, R};
        _ ->
            Tctx = Tctx0,
            RetVal = RetVal0
    end,
    Dx = Tctx#confd_trans_ctx.dx,
    if
        (Dx#confd_daemon_ctx.debug_level >= ?CONFD_TRACE) and
        (Dx#confd_daemon_ctx.estream /= undefined) ->
            case RetVal of
                {ok, _Reply} ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> OK\n", []);
                {error, #confd_error{code = Code, str = Str}} ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_ERR: ~s:~s\n",[?a2l(Code),?s2l(Str)]);
                {error, CE} when ?is_cs_error(CE) ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_ERR: <internal #cs_error{}>\n",[]);
                {error, ErrStr} ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_ERR: ~s\n", [?s2l(ErrStr)]);
                _ ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> BADRETVAL\n",[])
            end;
        true ->
            ok
    end,
    case RetVal of
        {ok, Value} ->
            econfd:data_reply_value(Tctx, Value);
        {error, Reason} ->
            reply_error(Tctx, Reason);
        _ ->
            reply_error(Tctx, #confd_error{code = proto_usage,
                                           str = <<"Bad return value">>})
    end.


action_callback(Dx, Qref, Uinfo, ActionCb, Type, ArgL) ->
    Op = ?CONFD_PROTO_CALLBACK,
    Did = Dx#confd_daemon_ctx.daemon_id,
    Usid = Uinfo#confd_user_info.usid,
    RetVal = try
                 case {ActionCb#confd_action_cb.action,
                       ActionCb#confd_action_cb.completion} of
                     {undefined, undefined} ->
                         report_err(Dx, ?CONFD_DEBUG,
                                    "No action callback installed",[]),
                         {error, #confd_error{
                            code = internal,
                            str = <<"no callback installed">> }};

                     {F, undefined} ->
                         [Name, KP, Params] = ArgL,

                         ?confd_trace(Dx, "CALL action(usid=~p,~p,~p)",
                                      [Usid, Name, KP]),
                         F(Uinfo, Name, KP, Params);
                     {undefined, F} ->
                         [Style, Token, CompletionChar, KP,
                          CmdPath, Id, SimpleType] = ArgL,

                         ?confd_trace(Dx, "CALL completion(~p)",
                                      [[Usid|ArgL]]),
                         F(Uinfo, Style, Token, CompletionChar, KP,
                                CmdPath, Id, SimpleType, [])

                 end
             catch
                 _:Rsn ->
                     report_err(Dx, ?CONFD_DEBUG,
                                "action cb failed ~p: ~p",
                                [Rsn, erlang:get_stacktrace()]),
                     callback_failed()
             end,
    if
        (Dx#confd_daemon_ctx.debug_level >= ?CONFD_TRACE) and
        (Dx#confd_daemon_ctx.estream /= undefined) ->
            case RetVal of
                ok ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> OK\n",[]);
                {ok, Values} ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> OK: ~p\n",[Values]);
                {error, #confd_error{code = Code, str = Str}} ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_ERR: ~s:~s\n",[?a2l(Code),?s2l(Str)]);
                {error, CE} when ?is_cs_error(CE) ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_ERR: <internal #cs_error{}>\n",[]);
                {error, ErrStr} ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> CONFD_ERR: ~s\n", [?s2l(ErrStr)]);
                _ ->
                    io:format(Dx#confd_daemon_ctx.estream,
                              " --> BADRETVAL\n",[])
            end;
        true ->
            ok
    end,
    Reply = case RetVal of
                ok ->
                    {Op, Qref, Did, {}};
                {ok, ValuesList} when Type == ?CONFD_CALL_ACTION_COMPLETION ->
                    {Op, Qref, Did, ValuesList};
                {ok, ValueList} when Type == ?CONFD_CALL_ACTION ->
                    {Op, Qref, Did, {exml, ValueList}};
                {ok, ValueList} ->
                    {Op, Qref, Did, ValueList};
                {error, Reason} ->
                    {Op, Qref, Did, error, mk_error(Reason)};
                _ ->
                    {Op, Qref, Did, error,
                     mk_error(#confd_error{code = proto_usage,
                                           str = <<"Bad return value">>})}
            end,
    term_write(Dx#confd_daemon_ctx.worker, Reply).

no_cb_err(Op, Tctx, CbStr) ->
    report_err(Tctx#confd_trans_ctx.dx, ?CONFD_DEBUG,
               "No ~p callback installed",[CbStr]),
    proto_reply(Op, Tctx, 0,
                {error, #confd_error{ code = internal,
                                      str = <<"no callback installed">> }}).

no_cb_err(Op, Tctx, CbStr, KP) ->
    report_err(Tctx#confd_trans_ctx.dx, ?CONFD_DEBUG,
               "No ~p callback installed for ~p~n",[CbStr, KP]),
    proto_reply(Op, Tctx, 0,
                {error, #confd_error{ code = internal,
                                      str = <<"no callback installed">> }}).

null_trans_handler(Dx, TH, Qref, Op, RequestData, RealUsid, HideInactive) ->
    Tctx = econfd:trans_get(Dx, th, TH),
    Uinfo = update_uinfo(Dx, Tctx#confd_trans_ctx.uinfo, RealUsid),
    Tctx2 = Tctx#confd_trans_ctx{dx=Dx, query_ref = Qref, lastop=Op,
                                 uinfo = Uinfo, request_data = RequestData,
                                 hide_inactive = HideInactive /= 0},
    econfd:trans_put(Dx, th, Tctx2),
    TransCb = Dx#confd_daemon_ctx.trans_cb,
    if
        TransCb == undefined ->
            {#confd_trans_cbs{}, Tctx2};
        true ->
            {TransCb, Tctx2}
    end.

null_validate_handler(Dx, TH,Qref,Op,RequestData,RealUsid) ->
    Tctx = econfd:trans_get(Dx, thvalidate, TH),
    Uinfo = update_uinfo(Dx, Tctx#confd_trans_ctx.uinfo, RealUsid),
    Tctx2 = Tctx#confd_trans_ctx{dx=Dx, query_ref = Qref, lastop=Op,
                                 uinfo = Uinfo, request_data = RequestData},
    econfd:trans_put(Dx, thvalidate, Tctx2),
    ValidateCb = Dx#confd_daemon_ctx.trans_validate_cb,
    if
        ValidateCb == undefined ->
            {#confd_trans_validate_cbs{}, Tctx2};
        true ->
            {ValidateCb, Tctx2}
    end.

update_uinfo(_Dx, #confd_user_info{usid = Usid} = Uinfo, Usid) ->
    Uinfo;
update_uinfo(Dx, _OldUinfo, Usid) ->
    [{_,Uinfo}] = ets:lookup(Dx#confd_daemon_ctx.user_sessions, Usid),
    Uinfo.

tag_to_valcbs(Dx,Point,Index) ->
    case [X || X <- Dx#confd_daemon_ctx.valp_cbs,
               X#confd_valpoint_cb.valpoint == Point,
               X#confd_valpoint_cb.index == Index] of
        [Item] ->
            {value, Item};
        [] ->
            false
    end.

tag_to_datacbs(Dx,Point,Index) ->
    case [X || X <- Dx#confd_daemon_ctx.data_cbs,
               X#confd_data_cbs.callpoint == Point,
               X#confd_data_cbs.index == Index] of
        [Item] ->
            {value, Item};
        [] ->
            false
    end.

tag_to_srvcbs(Dx,Point,Index) ->
    case [X || X <- Dx#confd_daemon_ctx.service_cbs,
               X#ncs_service_cbs.servicepoint == Point,
               X#ncs_service_cbs.index == Index] of
        [Item] ->
            {value, Item};
        [] ->
            false
    end.

tag_to_nanosrvcbs(Dx,Point,Index) ->
    case [X || X <- Dx#confd_daemon_ctx.service_cbs,
               X#ncs_nano_service_cbs.servicepoint == Point,
               X#ncs_nano_service_cbs.index == Index] of
        [Item] ->
            {value, Item};
        [] ->
            false
    end.

tag_to_actcbs(Dx,Point,Index) ->
    case [X || X <- Dx#confd_daemon_ctx.action_cbs,
               X#confd_action_cb.actionpoint == Point,
               X#confd_action_cb.index == Index] of
        [Item] ->
            {value, Item};
        [] ->
            false
    end.

pp_choice(ChoicePath) ->
    F = fun ([_Ns|Tag]) -> Tag; (Tag) -> Tag end,
    string:join([?a2l(F(E)) || E <- lists:reverse(ChoicePath)], "/").

cc1(undefined,_A) ->
    ok;
cc1(F,A) ->
    try F(A) of
        Ret -> Ret
    catch _:Rsn ->
            ?cerror_msg("Callback failed: ~p:",[Rsn]),
            callback_failed()
    end.

callback_failed() ->
    {error, #confd_error{code = proto_usage,
                         str = <<"callback invocation failed">>}}.


%% Actions don't have transaction handles
usid2th(Usid) -> -Usid - 10.
th2usid(TH) -> -TH - 10.

count_instances(Tctx, KP, F, Num, Key) ->
    case F(Tctx, KP, Key) of
        {ok, {false, _}} ->
             {ok,Num};
        {ok, {_, Next}} ->
            count_instances(Tctx, KP, F, Num+1, Next)
    end.

do_cnct(Dx, Ip, Port) ->
    case ctl_cnct(Dx, Ip, Port) of
        {ok, Dx2} ->
            worker_cnct(Dx2, Ip, Port);
        Err ->
            Err
    end.

ctl_cnct(Dx, Ip, Port) ->
    case econfd_internal:connect(Ip, Port, ?CLIENT_CAPI, [{active, true}]) of
        {ok, Socket} ->
            ?confd_trace(Dx, "Connected to Confd/NCS\n",[]),
            Ret = confd_call_active(Socket, {?CONFD_PROTO_DAEMON,
                                             Dx#confd_daemon_ctx.name, 1}),
            case Ret of
                {ok, {_IdConst, Did}} ->
                    {ok, Dx#confd_daemon_ctx{
                           daemon_id = Did,
                           ctl = Socket}};
                Error ->
                    Error
            end;
        Error ->
            ?confd_trace(Dx, "Failed to connect to ConfD\n",[]),
            Error
    end.

worker_cnct(Dx, Ip, Port) ->
    case worker_do_connect(Ip, Port, Dx#confd_daemon_ctx.daemon_id) of
        {ok, Socket, Int} ->
            {ok, Dx#confd_daemon_ctx{wint = Int, worker = Socket}};
        Error ->
            Error
    end.

worker_do_connect(Ip, Port, DaemonId) ->
    case econfd_internal:connect(Ip, Port, ?CLIENT_CAPI, [{active, true}]) of
        {ok, Socket} ->
            Int = incr(),
            case term_write(Socket, {?CONFD_PROTO_WORKER, DaemonId, Int}) of
                ok ->
                    {ok, Socket, Int};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.


flg(undefined, _) -> 0;
flg(_, Mask) -> Mask.


null_1(undefined) ->
    fun(_) ->
             ok
    end;
null_1(F) ->
    F.

find_notif_cb(Streamname, Dx, _Index) ->
    lists:keysearch(Streamname, #confd_notification_stream_cbs.streamname,
                    Dx#confd_daemon_ctx.notif_cbs).


%% worker process - handles all worker socket requests
worker(Dx) ->
    receive
        {new_dx, NewDx} ->
            worker(NewDx);
        {tcp, _Socket, Data} ->
            confd_fd_ready(Dx, econfd_internal:term_get(Data)),
            worker(Dx);
        {tcp_closed, _Socket} ->
            ok;
        {tcp_error, _Socket, Reason} ->
            error_logger:format("Confd/NCS socket died: ~p\n",[Reason]),
            ok
    end.
