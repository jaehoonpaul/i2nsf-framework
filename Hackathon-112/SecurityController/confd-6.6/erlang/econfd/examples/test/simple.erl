-module(simple).
-compile(export_all).

-include("../../include/econfd.hrl").
-include("simple.hrl").
-record(server, {name_number, ip, port, obj, refs, attrs = []}).

start() ->
    application:start(econfd),
    init_db(),
    Trans = #confd_trans_cbs{init = fun s_init/1,
                             trans_lock = fun s_trans_lock/1,
                             trans_unlock = fun s_trans_unlock/1,
                             write_start = fun s_write_start/1,
                             prepare = fun s_prepare/1},
    Data = #confd_data_cbs{get_elem = fun get_elem/2,
                           exists_optional = fun exists_optional/2,
                           get_next = fun get_next/3,
                           set_elem = fun set_elem/3,
                           create =   fun create/2,
                           remove =   fun doremove/2,
                           get_attrs = fun get_attrs/3,
                           set_attr = fun set_attr/4,
                           callpoint = simplecp},
    Db = #confd_db_cbs{add_checkpoint_running = fun add_checkpoint_running/1,
                       del_checkpoint_running = fun del_checkpoint_running/1,
                       activate_checkpoint_running =
                           fun activate_checkpoint_running/1,
                       lock = fun lock/2,
                       unlock = fun unlock/2},

    {ok,Daemon} = econfd:init_daemon(simple, ?CONFD_DEBUG, user, none,
                                    {127,0,0,1}, ?CONFD_PORT),
    ok = econfd:set_daemon_flags(Daemon, ?CONFD_DAEMON_FLAG_LEAF_LIST_AS_LEAF),
    ok = econfd:register_trans_cb(Daemon, Trans),
    ok = econfd:register_data_cb(Daemon, Data),
    ok = econfd:register_db_cbs(Daemon, Db),
    ok = econfd:register_done(Daemon).

make_ets() ->
    spawn(fun() ->
                  (catch ets:delete(servers)),
                  ets:new(servers, [public, {keypos, 2}, named_table]),
                  timer:sleep(infinity)
          end).


init_db() ->
    make_ets(),
    case restore("running.DB") of
        ok ->
            ok;
        _ ->
            io:format("setting servers to default values \n",[]),
            ets:insert(servers, #server{name_number={<<"ssh">>, 1},
                                        ip={192,168,128,1},
                                        port=22,
                                        obj=objref(<<"www">>, 2)}),
            ets:insert(servers, #server{name_number={<<"www">>, 2},
                                        ip={192,168,128,11},
                                        port=80,
                                        obj=objref(<<"smtp">>, 3)}),
            ets:insert(servers, #server{name_number={<<"smtp">>, 3},
                                        ip={192,168,128,31},
                                        port=25,
                                        obj=objref(<<"ssh">>, 1)}),
            ok
    end.
restore(File) ->
    case file:read_file(File) of
        {ok, B} ->
            L = binary_to_term(B),
            ets:match_delete(servers, '_'),
            lists:foreach(fun(S) -> ets:insert(servers, S) end, L),
            ok;
        Err ->
            Err
    end.


find_server({Name, ?CONFD_INT64(Number)}) ->
    ets:lookup(servers, {Name, Number}).


get_next(_Tctx, _Ikeypath, -1) ->
    case ets:first(servers) of
        '$end_of_table' ->
            {ok, {false, undefined}};
        Key = {Name, Number} ->
            Next = ets:next(servers, Key),
            {ok, {{Name, ?CONFD_INT64(Number)}, Next}}
    end;
get_next(_Tctx, _Ikeypath, Prev) ->
    case Prev of
        '$end_of_table' ->
            {ok, {false, undefined}};
        {Name, Number} ->
            Next = ets:next(servers, Prev),
            {ok, {{Name, ?CONFD_INT64(Number)}, Next}}
    end.

exists_optional(_Tctx, [dataModelVersion2|_]) ->
    {ok, true}.

get_elem(_Tctx, [updateMode| _Tail]) ->
    {ok, <<"MYUPDATEMODE">>};
get_elem(_Tctx, [dataModelVersion|_]) ->
    {ok, not_found};
get_elem(_Tctx, [ElemTag, Key | _Tail]) ->
    case find_server(Key) of
        [] ->
            {ok, not_found};
        [Srv] ->
            if
                ElemTag == name ->
                    {Name, _Number} = Srv#server.name_number,
                    {ok, Name};
                ElemTag == ip ->
                    {ok, Srv#server.ip};
                ElemTag == port ->
                    {ok,  ?CONFD_UINT16(Srv#server.port)};
                ElemTag == obj, Srv#server.obj == undefined ->
                    {ok, not_found};
                ElemTag == obj ->
                    {ok, ?CONFD_OBJECTREF(Srv#server.obj)};
                ElemTag == refs, Srv#server.refs == undefined ->
                    {ok, not_found};
                ElemTag == refs ->
                    {ok, [?CONFD_OBJECTREF(Ref) || Ref <- Srv#server.refs]};
                true ->
                    {error, <<"Bad tag received">>}
            end
    end.

set_elem(_Tctx, [ElemTag, Key | _Ikeypath], Value) ->
    case find_server(Key) of
        [] ->
            {error, <<"No such server">>};
        [Srv] when ElemTag == ip ->
            ets:insert(servers, Srv#server{ip = Value}), ok;
        [Srv] when ElemTag == port ->
            ?CONFD_UINT16(Port) = Value,
            ets:insert(servers, Srv#server{port = Port}), ok;
        [Srv] when ElemTag == obj ->
            ?CONFD_OBJECTREF(Obj) = Value,
            ets:insert(servers, Srv#server{obj = Obj}), ok;
        [Srv] when ElemTag == refs ->
            Refs = [Ref || ?CONFD_OBJECTREF(Ref) <- Value],
            ets:insert(servers, Srv#server{refs = Refs}), ok
    end.

create(_Tctx, [{Srvname,?CONFD_INT64(Number)} | _Ikeypath]) ->
    ets:insert(servers, #server{name_number = {Srvname, Number}}),
    ok.

doremove(_Tctx,  [{Srvname, ?CONFD_INT64(Number)} | _Ikeypath]) ->
    ets:delete(servers, {Srvname, Number}),
    ok;
doremove(Tctx,  [obj, Key | _Ikeypath]) ->
    case errchk(data, Tctx#confd_trans_ctx.uinfo) of
        ok ->
            case find_server(Key) of
                [] ->
                    {error, <<"No such server">>};
                [Srv] ->
                    ets:insert(servers, Srv#server{obj = undefined}),
                    ok
            end;
        Err ->
            Err
    end;
doremove(_Tctx,  [refs, Key | _Ikeypath]) ->
    case find_server(Key) of
        [] ->
            {error, <<"No such server">>};
        [Srv] ->
            ets:insert(servers, Srv#server{refs = undefined}),
            ok
    end.

%% We only support attrs on the server instance itself
get_attrs(_Tctx, [Key | _Ikeypath], AttrL) when is_tuple(Key) ->
    case find_server(Key) of
        [] -> {ok, not_found};
        [Srv] -> {ok, get_attrs(AttrL, Srv#server.attrs)}
    end;
get_attrs(_Tctx, [_Elem, Key | _Ikeypath], _AttrL) when is_tuple(Key) ->
    case find_server(Key) of
        [] -> {ok, not_found};
        [_Srv] -> {ok, []}
    end;
get_attrs(_Tctx, _Ikeypath, _AttrL) ->
    {ok, []}.

get_attrs([], Attrs) ->
    Attrs;
get_attrs(L, Attrs) ->
    lists:zf(fun(Attr) ->
                     case lists:keysearch(Attr, 1, Attrs) of
                         {value, V} -> {true, V};
                         false      -> false
                     end
             end, L).

set_attr(_Tctx, [Key | _Ikeypath], Attr, Value) when is_tuple(Key) ->
    case find_server(Key) of
        [] ->
            {error, <<"No such server">>};
        [Srv] ->
            Attrs = [{Attr, Value}|lists:keydelete(Attr, 1, Srv#server.attrs)],
            ets:insert(servers, Srv#server{attrs = Attrs}),
            ok
    end;
set_attr(_Tctx, [_Elem, Key | _Ikeypath], _Attr, _Value) when is_tuple(Key) ->
    case find_server(Key) of
        [] ->
            {error, <<"No such server">>};
        [_Srv] ->
            {error, <<"No attrs on leafs">>}
    end;
set_attr(_Tctx, _Ikeypath, _Attr, _Value) ->
    {error, <<"No attrs on containers">>}.


objref(Name, Number) ->
    [obj, {Name, ?CONFD_INT64(Number)}, server, servers, [?simple__ns_uri|dp]].

%% Pass transaction state in #confd_trans_ctx.opaque
%% to verify that returned Tctx is propagated
s_init(Tctx) ->
    {ok, Tctx#confd_trans_ctx{opaque = read}}.

s_trans_lock(#confd_trans_ctx{opaque = read} = Tctx) ->
    trans_ret(trans_lock, Tctx).

s_trans_unlock(#confd_trans_ctx{opaque = validate} = Tctx) ->
    trans_ret(trans_unlock, Tctx).

s_write_start(#confd_trans_ctx{opaque = validate} = Tctx) ->
    trans_ret(write_start, Tctx).

s_prepare(#confd_trans_ctx{opaque = write} = Tctx) ->
    trans_ret(prepare, Tctx).

trans_ret(Cb, Tctx) ->
    case errchk(Cb, Tctx#confd_trans_ctx.uinfo) of
        ok when Cb == trans_lock ->
            {ok, Tctx#confd_trans_ctx{opaque = validate}};
        ok when Cb == trans_unlock ->
            {ok, Tctx#confd_trans_ctx{opaque = read}};
        ok when Cb == write_start ->
            {ok, Tctx#confd_trans_ctx{opaque = write}};
        ok when Cb == prepare ->
            {ok, Tctx#confd_trans_ctx{opaque = prepared}};
        Res ->
            Res
    end.

add_checkpoint_running(Dbx) ->
    errchk(add_checkpoint_running, Dbx#confd_db_ctx.uinfo).

del_checkpoint_running(Dbx) ->
    errchk(del_checkpoint_running, Dbx#confd_db_ctx.uinfo).

activate_checkpoint_running(Dbx) ->
    errchk(activate_checkpoint_running, Dbx#confd_db_ctx.uinfo).

lock(Dbx, DbName) ->
    errchk(list_to_atom("lock_" ++ db_name(DbName)), Dbx#confd_db_ctx.uinfo).

unlock(Dbx, DbName) ->
    errchk(list_to_atom("unlock_" ++ db_name(DbName)), Dbx#confd_db_ctx.uinfo).

db_name(?CONFD_RUNNING) -> "running";
db_name(?CONFD_STARTUP) -> "startup";
db_name(?CONFD_CANDIDATE) -> "candidate".

errchk(Func, Uinfo) ->
    FName = atom_to_list(Func),
    User = Uinfo#confd_user_info.username,
    case binary_to_list(User) of
        "errstr_" ++ FName ->
            {error, list_to_binary("error for " ++ FName)};
        "in_use_" ++ FName ->
            confd_in_use;
        "already_locked_" ++ FName ->
            confd_already_locked;
        "cs_in_use_" ++ FName ->
            {error, #confd_error{code = in_use}};
        "cs_in_use_str_" ++ FName ->
            {error, #confd_error{code = in_use,
                                 str=list_to_binary("cs_error for " ++ FName)}};
        "cs_apptag_" ++ FName ->
            {error, #confd_error{code = application,
                                 apptag = [?simple__ns_uri|servers]}};
        "cs_str_" ++ FName ->
            {error, #confd_error{code = application,
                                 str=list_to_binary("cs_error for " ++ FName)}};
        _ ->
            ok
    end.
