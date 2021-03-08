-module(iid).
-export([start/0, objref/1]).

-include("../../include/econfd.hrl").
-include("iid.hrl").
-record(iid, {key, value, attrs = []}).

start() ->
    application:start(econfd),
    init_db(),
    Trans = #confd_trans_cbs{init = fun s_init/1},
    Data = #confd_data_cbs{get_elem = fun get_elem/2,
                           get_next = fun get_next/3,
                           set_elem = fun set_elem/3,
                           create =   fun create/2,
                           remove =   fun remove/2,
                           get_attrs = fun get_attrs/3,
                           set_attr = fun set_attr/4,
                           callpoint = ?iid__callpointid_iidcp},
    Db = #confd_db_cbs{add_checkpoint_running = fun add_checkpoint_running/1,
                       del_checkpoint_running = fun del_checkpoint_running/1,
                       activate_checkpoint_running =
                           fun activate_checkpoint_running/1},

    {ok,Daemon} = econfd:init_daemon(iid, ?CONFD_DEBUG, user, none,
                                    {127,0,0,1}, ?CONFD_PORT),
    ok = econfd:register_trans_cb(Daemon, Trans),
    ok = econfd:register_data_cb(Daemon, Data),
    ok = econfd:register_db_cbs(Daemon, Db),
    ok = econfd:register_done(Daemon).

make_ets() ->
    spawn(fun() ->
                  (catch ets:delete(iids)),
                  ets:new(iids, [public, {keypos, 2}, named_table]),
                  timer:sleep(infinity)
          end).


init_db() ->
    make_ets(),
    io:format("setting iids to default values \n",[]),
    ets:insert(iids,
               #iid{key = {<<"ssh">>, objref(<<"ssh">>)},
                    value = <<"foo">>}),
    ets:insert(iids,
               #iid{key = {<<"www">>, objref(<<"www">>)},
                    value = <<"bar">>}),
    ets:insert(iids,
               #iid{key = {<<"smtp">>, objref(<<"smtp">>)},
                    value = <<"foobar">>}),
    ok.


find_iid(Key) ->
    ets:lookup(iids, Key).


get_next(_Tctx, _Ikeypath, Prev) ->
    case get_next(Prev) of
        '$end_of_table' ->
            {ok, {false, undefined}};
        Key ->
            {ok, {Key, Key}}
    end.

get_next(-1) ->
    ets:first(iids);
get_next(Prev) ->
    ets:next(iids, Prev).


get_elem(_Tctx, [ElemTag, Key | _Tail]) ->
    case find_iid(Key) of
        [] ->
            {ok, not_found};
        [#iid{key = {Name, _Obj}}] when ElemTag == name ->
            {ok, Name};
        [#iid{value = Value}] when ElemTag == value ->
            {ok, Value}
    end.


set_elem(_Tctx, [ElemTag, Key | _Ikeypath], Value) ->
    case find_iid(Key) of
        [] ->
            {error, <<"No such iid">>};
        [IID] when ElemTag == value ->
            ets:insert(iids, IID#iid{value = Value}),
            ok
    end.

create(_Tctx, [Key | _Ikeypath]) ->
    ets:insert(iids, #iid{key = Key}),
    ok.

remove(_Tctx,  [Key | _Ikeypath]) ->
    ets:delete(iids, Key),
    ok.

%% We only support attrs on the iid instance itself
get_attrs(_Tctx, [Key | _Ikeypath], AttrL) when is_tuple(Key) ->
    case find_iid(Key) of
        [] -> {ok, not_found};
        [IID] -> {ok, get_attrs(AttrL, IID#iid.attrs)}
    end;
get_attrs(_Tctx, [_Elem, Key | _Ikeypath], _AttrL) when is_tuple(Key) ->
    case find_iid(Key) of
        [] -> {ok, not_found};
        [_IID] -> {ok, []}
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
    case find_iid(Key) of
        [] ->
            {error, <<"No such iid">>};
        [IID] ->
            Attrs = [{Attr, Value}|lists:keydelete(Attr, 1, IID#iid.attrs)],
            ets:insert(iids, IID#iid{attrs = Attrs}),
            ok
    end;
set_attr(_Tctx, [_Elem, Key | _Ikeypath], _Attr, _Value) when is_tuple(Key) ->
    case find_iid(Key) of
        [] ->
            {error, <<"No such iid">>};
        [_IID] ->
            {error, <<"No attrs on leafs">>}
    end;
set_attr(_Tctx, _Ikeypath, _Attr, _Value) ->
    {error, <<"No attrs on containers">>}.


s_init(_Tctx) ->
    ok.

add_checkpoint_running(_Dbx) ->
    ok.

del_checkpoint_running(_Dbx) ->
    ok.

activate_checkpoint_running(_Dbx) ->
    ok.

objref(<<"ssh">>) ->
    ?CONFD_OBJECTREF(top());
objref(<<"www">>) ->
    ?CONFD_OBJECTREF([iid | top()]);
objref(<<"smtp">>) ->
    ?CONFD_OBJECTREF([v, {?CONFD_INT32(42)}, [?iid__ns_uri|ref]]).

top() ->
    [[?iid__ns_uri|ext]].
