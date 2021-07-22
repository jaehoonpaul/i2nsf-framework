-module(procs).
-compile(export_all).

-include("../../include/econfd.hrl").


start() ->
    application:start(econfd),
    Trans = #confd_trans_cbs{init = fun s_init/1,
                             finish = fun s_finish/1},

    Data = #confd_data_cbs{get_elem = fun get_elem/2,
                           get_next = fun get_next/3,
                           find_next = fun find_next/4,
                           get_attrs = fun dummy_get_attrs/3,
                           callpoint = proc_cp},

    Obj = #confd_data_cbs{get_object = fun get_obj/2,
                          get_next =   fun o_get_next/3,
                          get_attrs = fun dummy_get_attrs/3,
                          callpoint = obj_cp},

    NObj = #confd_data_cbs{get_next_object = fun get_nobj/3,
                           find_next_object = fun find_nobj/4,
                           num_instances = fun nobj_num_instances/2,
                           get_attrs = fun dummy_get_attrs/3,
                           callpoint = nobj_cp},

    {ok,Daemon} = econfd:init_daemon('procs.erl', ?CONFD_DEBUG, user, none,
                                   {127,0,0,1}, ?CONFD_PORT),
    ok = econfd:set_daemon_flags(Daemon, ?CONFD_DAEMON_FLAG_LEAF_LIST_AS_LEAF),
    ok = econfd:register_trans_cb(Daemon, Trans),
    ok = econfd:register_data_cb(Daemon, Data),
    ok = econfd:register_data_cb(Daemon, Obj),
    ok = econfd:register_data_cb(Daemon, NObj),
    ok = econfd:register_done(Daemon).


dummy_get_attrs(_T, _K, _L) -> {ok, []}.

get_next(Tctx, _IKP, -1) ->
    L = lists:sort(lists:map(fun(P) ->  list_to_binary(pid_to_list(P)) end,
                             [list_to_pid("<0.1999.0>") | processes()])),
    Tctx2 = Tctx#confd_trans_ctx{opaque = L},
    {ok, {{hd(L)}, -1}, Tctx2};     % switch to find_next
get_next(Tctx, _HKP, Prev) ->
    L  = Tctx#confd_trans_ctx.opaque,
    case catch lists:nth(Prev, L) of
        {'EXIT',_} ->
            {ok, {false, undefined}};
        Item when Prev rem 3 == 0 ->
            {ok, {{Item}, -1}};     % switch to find_next
        Item ->
            {ok, {{Item}, Prev+1}}
    end.

find_next(Tctx, _HKP, _Type, {}) ->
    L  = Tctx#confd_trans_ctx.opaque,
    {ok, {{hd(L)}, 1}};             % switch to get_next
find_next(Tctx, _HKP, Type, {PrevKey}) ->
    L  = Tctx#confd_trans_ctx.opaque,
    case find_next_key(PrevKey, L, Type, 1) of
        {Item, Next} ->
            {ok, {{Item}, Next}};   % switch to get_next
        false ->
            {ok, {false, undefined}}
    end.

find_next_key(Key, [NextKey|_], ?CONFD_FIND_NEXT, Next)
  when NextKey > Key ->
    {NextKey, Next};
find_next_key(Key, [NextKey|_], ?CONFD_FIND_SAME_OR_NEXT, Next)
  when NextKey >= Key ->
    {NextKey, Next};
find_next_key(Key, [_|Rest], Type, Next) ->
    find_next_key(Key, Rest, Type, Next+1);
find_next_key(_, _, _, _) ->
    false.

get_elem(_Tctx, [ElemTag, {PidStr} | _Tail]) ->
    case process_info(list_to_pid(binary_to_list(PidStr))) of
        undefined ->
            {ok, not_found};
        _L when ElemTag == pid ->
            {ok, PidStr};
        L when ElemTag == heap_size ->
            {value, {_, Sz}} = lists:keysearch(heap_size,1,L),
            {ok, Sz};
        L when ElemTag == stack_size ->
            {value, {_, Sz}} = lists:keysearch(stack_size,1,L),
            {ok, Sz};
        L when ElemTag == registered_name ->
            case lists:keysearch(registered_name, 1,L) of
                false ->
                    {ok, not_found};
                {value, {_, Name}} ->
                    {ok, list_to_binary(atom_to_list(Name))}
            end;
        L when ElemTag == trap_exit ->
            {value, {_, Bool}} = lists:keysearch(trap_exit,1,L),
            {ok, Bool}
    end.

o_get_next(_Tctx, _IKP, -1) ->
    {ok, {{1001}, 1}};
o_get_next(_Tctx, _IKP, 1) ->
    {ok, {{1002}, 2}};
o_get_next(_Tctx, _IKP, 2) ->
    {ok, {{1003}, 3}};
o_get_next(_Tctx, _IKP, 3) ->
    {ok, {{1011}, 4}};
o_get_next(_Tctx, _IKP, 4) ->
    {ok, {{1012}, 5}};
o_get_next(_Tctx, _IKP, 5) ->
    {ok, {{1013}, 6}};
o_get_next(_Tctx, _IKP, 6) ->
    {ok, {false, undefined}}.


get_obj(_Tctx, [{1001}| _]) ->
    io:format("getobj ~p~n", [1001]),
    Time = ?CONFD_TIME({2,2,2,0,[],0}),
    Date = ?CONFD_DATE({1999,2,22,2,30}),
    Refs = [],
    Dur = ?CONFD_DURATION({0,0,0,2,3,8,8888}),
    {ok,[1001, <<"s1">>, Time, Date, Refs, ?CONFD_XMLTAG(container),Dur]};

get_obj(_Tctx, [{1002}|T]) ->
    io:format("getobj ~p~n", [1002]),
    Time = ?CONFD_TIME({2,2,2,0,[],0}),
    Date = ?CONFD_DATE({1999,2,22,2,30}),
    Refs = [?CONFD_OBJECTREF([s1, {1001}|T]),
            ?CONFD_OBJECTREF([time, {1002}|T])],
    _Dur = ?CONFD_DURATION({0,0,0,2,3,8,8888}),
    %% Incorrect return value (includes element for child of non-existent
    %% container) - will be logged in devel-log and agent gets external error
    {ok,[1002, <<"s1">>, Time, Date, Refs, not_found, not_found]};

get_obj(_Tctx, [{1003}|T]) ->
    io:format("getobj ~p~n", [1003]),
    Time = ?CONFD_TIME({2,2,2,0,[],0}),
    Date = ?CONFD_DATE({1999,2,22,2,30}),
    Refs = [?CONFD_OBJECTREF([s1, {1001}|T]),
            ?CONFD_OBJECTREF([time, {1002}|T])],
    _Dur = ?CONFD_DURATION({0,0,0,2,3,8,8888}),
    %% This is the correct return value
    {ok,[1003, <<"s1">>, Time, Date, Refs, not_found]};

%% now some [tagval()] replies

get_obj(_Tctx, [{1011}| _]) ->
    io:format("getobj ~p~n", [1011]),
    Time = ?CONFD_TIME({2,2,2,0,[],0}),
    Date = ?CONFD_DATE({1999,2,22,2,30}),
    Dur = ?CONFD_DURATION({0,0,0,2,3,8,8888}),
    {ok, {exml, [{s1, <<"s1">>}, {time, Time}, {date, Date},
                 {container, start}, {duration, Dur}, {container, stop}]}};

get_obj(_Tctx, [{1012}|T]) ->
    io:format("getobj ~p~n", [1012]),
    Time = ?CONFD_TIME({2,2,2,0,[],0}),
    Date = ?CONFD_DATE({1999,2,22,2,30}),
    Refs = [?CONFD_OBJECTREF([s1, {1011}|T]),
            ?CONFD_OBJECTREF([time, {1012}|T])],
    Dur = ?CONFD_DURATION({0,0,0,2,3,8,8888}),
    %% Incorrect return value - includes child of container but omits container
    %% start - will be logged in devel-log and agent gets external error
    {ok, {exml, [{s1, <<"s1">>}, {time, Time}, {date, Date}, {refs, Refs},
                 {duration, Dur}]}};

get_obj(_Tctx, [{1013}|T]) ->
    io:format("getobj ~p~n", [1013]),
    Time = ?CONFD_TIME({2,2,2,0,[],0}),
    Date = ?CONFD_DATE({1999,2,22,2,30}),
    Refs = [?CONFD_OBJECTREF([s1, {1011}|T]),
            ?CONFD_OBJECTREF([time, {1012}|T])],
    _Dur = ?CONFD_DURATION({0,0,0,2,3,8,8888}),
    %% This is the correct return value for non-existing container
    {ok, {exml, [{s1, <<"s1">>}, {time, Time}, {date, Date}, {refs, Refs}]}}.


get_nobj(_Tctx, [nobj, nobjs, [_NS|test]], -1) ->
    {ok, {[1, <<"foo">>, <<"1">>], a}};
get_nobj(_Tctx, [nobj, nobjs, [_NS|test]], a) ->
    {ok, {[2, <<"bar">>, <<"2">>], -1}};        % switch to find_next_object()
%get_nobj(_Tctx, [nobj, nobjs, [_NS|test]], b) ->
%    {ok, {{exml, [{key1, 3}, {key2, <<"foobar">>}, {s1, <<"3">>}]}, c}};
get_nobj(_Tctx, [nobj, nobjs, [_NS|test]], c) ->
    Objs = [{[4, <<"xxx">>, <<"4">>], d}, {[5, <<"yyy">>, <<"5">>], e}],
    {ok, Objs, 0};
get_nobj(_Tctx, [nobj, nobjs, [_NS|test]], e) ->
    TObjs = [{{exml, [{key1, 6}, {key2, <<"aaa">>}, {s1, <<"6">>}]}, f},
             {{exml, [{key1, 7}, {key2, <<"bbb">>}, {s1, <<"7">>}]}, -1},
             false],
    {ok, TObjs, 0};
get_nobj(_Tctx, [nobj, nobjs, [_NS|test]], g) ->
    {ok, {false, undefined}}.

%find_nobj(_Tctx, [nobj, nobjs, [_NS|test]], _Type, -1) ->
%    {ok, {[1, <<"foo">>, <<"1">>], a}};
%find_nobj(_Tctx, [nobj, nobjs, [_NS|test]], _Type, {1, <<"foo">>}) ->
%    {ok, {[2, <<"bar">>, <<"2">>], b}};
find_nobj(_Tctx, [nobj, nobjs, [_NS|test]], _Type, {2, <<"bar">>}) ->
    {ok, {{exml, [{key1, 3}, {key2, <<"foobar">>}, {s1, <<"3">>}]}, c}};
%find_nobj(_Tctx, [nobj, nobjs, [_NS|test]], _Type, {3, <<"foobar">>}) ->
%    Objs = [{[4, <<"xxx">>, <<"4">>], d}, {[5, <<"yyy">>, <<"5">>], e}],
%    {ok, Objs, 0};
%find_nobj(_Tctx, [nobj, nobjs, [_NS|test]], _Type, {5, <<"yyy">>}) ->
%    TObjs = [{{exml, [{key1, 6}, {key2, <<"aaa">>}, {s1, <<"6">>}]}, f},
%             {{exml, [{key1, 7}, {key2, <<"bbb">>}, {s1, <<"7">>}]}, g},
%             false],
%    {ok, TObjs, 0};
find_nobj(_Tctx, [nobj, nobjs, [_NS|test]], _Type, {7, <<"bbb">>}) ->
    {ok, {false, undefined}}.

nobj_num_instances(_Tctx, [nobj, nobjs, [_NS|test]]) ->
    {ok, 7}.


s_init(Tctx) ->
    {ok, Tctx#confd_trans_ctx{opaque = []}}.

s_finish(_Tctx) ->
    ok.




