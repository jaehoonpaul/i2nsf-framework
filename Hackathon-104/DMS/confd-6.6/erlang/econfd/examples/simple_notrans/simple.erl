-module(simple).
-compile(export_all).

-include("../../include/econfd.hrl").
-include("smp.hrl").
-record(server, {name_number, ip, port, obj, refs, attrs = []}).

start() ->
    application:start(econfd),
    timer:sleep(1000),
    proc_lib:spawn(fun go/0).


go() ->
    init_db(),
    Trans = #confd_trans_cbs{init = fun s_init/1},
    Data = #confd_data_cbs{get_elem = fun get_elem/2,
                           get_next = fun get_next/3,
                           set_elem = fun set_elem/3,
                           create =   fun create/2,
                           remove =   fun doremove/2,
                           get_attrs = fun get_attrs/3,
                           set_attr = fun set_attr/4,
                           callpoint = simplecp},

    process_flag(trap_exit, true),
    {ok,Daemon} = econfd:init_daemon(simple, ?CONFD_TRACE, user, none,
                                    {127,0,0,1}, ?CONFD_PORT),
    ok = econfd:set_daemon_flags(Daemon, ?CONFD_DAEMON_FLAG_LEAF_LIST_AS_LEAF),
    register(daemon, Daemon),
    ok = econfd:register_trans_cb(Daemon, Trans),
    ok = econfd:register_data_cb(Daemon, Data),
    ok = econfd:register_done(Daemon),
    receive
        {'EXIT', _From, _Reason} ->
            init:stop()
    end.


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
    ok.

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
    [obj, {Name, ?CONFD_INT64(Number)}, server, [?smp__ns_uri|servers]].

s_init(Tctx) ->
    {ok, Tctx}.
