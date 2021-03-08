-module(procs).
-compile(export_all).

-include("../../include/econfd.hrl").
-include("procs.hrl").

start() ->
    application:start(econfd),
    Trans = #confd_trans_cbs{init = fun s_init/1,
                             finish = fun s_finish/1},

    Data = #confd_data_cbs{get_elem = fun get_elem/2,
                           get_next = fun get_next/3,
                           callpoint = proc_cp},

    {ok,Daemon} = econfd:init_daemon(procs_name, ?CONFD_TRACE, user, none,
                                   {127,0,0,1}, ?CONFD_PORT),
    ok = econfd:register_trans_cb(Daemon, Trans),
    ok = econfd:register_data_cb(Daemon, Data),
    ok = econfd:register_done(Daemon).


get_next(Tctx, _IKP, -1) ->
    L  = Tctx#confd_trans_ctx.opaque,
    {ok, {{hd(L)}, 2}};
get_next(Tctx, _HKP, Prev) ->
    L  = Tctx#confd_trans_ctx.opaque,
    case catch lists:nth(Prev, L) of
        {'EXIT',_} ->
            {ok, {false, undefined}};
        Item ->
            {ok, {{Item}, Prev+1}}
    end.

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
            {ok, Bool};
        L when ElemTag == priority ->
            {value, {_, Prio}} = lists:keysearch(priority,1,L),
            {ok, priority(Prio)}
    end.

priority(low)    -> ?CONFD_ENUM_VALUE(?procs_low);
priority(normal) -> ?CONFD_ENUM_VALUE(?procs_normal);
priority(high)   -> ?CONFD_ENUM_VALUE(?procs_high);
priority(max)    -> ?CONFD_ENUM_VALUE(?procs_max).

%% add a non existing proc to the list .. just for testing
s_init(Tctx) ->
    Opaque = lists:sort(lists:map(fun(P) ->  list_to_binary(pid_to_list(P)) end,
                                  [list_to_pid("<0.1999.0>") | processes()])),
    {ok, Tctx#confd_trans_ctx{opaque = Opaque}}.


s_finish(_Tctx) ->
    ok.




