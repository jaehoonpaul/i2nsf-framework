%%% File    : test.erl
%%% Author  :  <klacke@tail-f.com>
%%% Description :
%%% Created : 23 Oct 2006 by  <klacke@tail-f.com>

-module(test).
-compile(export_all).
-include("../../include/econfd.hrl").
-include("../../include/econfd_errors.hrl").
-define(line, put('$line',{?MODULE,?LINE}),).

-include("smp.hrl").
-include("cdbwrite.hrl").
-include("types.hrl").
-include("iid.hrl").
-include("notif.hrl").

start() ->
    proc_lib:spawn(fun() -> test(true, false) end).

istart() ->
    proc_lib:spawn(fun() -> test(true, true) end).

dstart() ->
    proc_lib:spawn(fun() -> test(false, false) end).

pstart() ->
    proc_lib:spawn(fun() -> port_test() end).

setup() ->
    proc_lib:spawn(fun() -> setup0() end).


setup0() ->
    application:start(econfd),
    ?line ok = procs:start(),  %% a stats generator
    ?line ok = simple:start(), %% a read/write data provider
    ?line ok = iid:start(),    %% another read/write data provider

    ?line {ok, M} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),

    %% install a validator in a new daemon
    Trans = #confd_trans_validate_cbs{init = fun vinit/1, stop = fun vstop/1},
    ?line V = #confd_valpoint_cb{valpoint = vp1,
                                 validate = fun validate/3},


    ?line V2 = #confd_valpoint_cb{valpoint = vp2,
                                  validate = fun validate2/3},

    ?line Action2 = #confd_action_cb{actionpoint = math_cs,
                                     action = fun math_cs/4},
    ?line Action3 = #confd_action_cb{actionpoint = reboot,
                                     action = fun reboot/4},

    ?line Authen = #confd_authentication_cb{auth = fun auth_check/1},

    ?line Notif = #confd_notification_stream_cbs{streamname = linkUp},

    ?line {ok,Daemon} = econfd:init_daemon('test.erl', ?CONFD_TRACE,
                                           user, M,
                                           {127,0,0,1}, ?CONFD_PORT),
    econfd:controlling_process(M, Daemon),
    register(daemon, Daemon),
    ?line ok = econfd:register_trans_validate_cb(Daemon, Trans),
    ?line ok = econfd:register_valpoint_cb(Daemon, V),
    ?line ok = econfd:register_valpoint_cb(Daemon, V2),
    ?line ok = econfd:register_action_cb(Daemon, Action2),
    ?line ok = econfd:register_action_cb(Daemon, Action3),
    ?line ok = econfd:register_authentication_cb(Daemon, Authen),
    ?line {ok, Nctx} = econfd:register_notification_stream(Daemon, Notif),
    ?line ok = econfd:register_done(Daemon),
    ?line {error, {?CONFD_ERR_PROTOUSAGE, _}} = econfd:register_done(Daemon),

    %% Makefile only started phase0, we must register
    %% the validation handler before we go into phase1/2

    io:format("Setting phases 1,2 ~n",[]),
    ?line {ok, M2} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    ?line ok = econfd_maapi:start_phase(M2, 1, true),
    ?line ok = econfd_maapi:start_phase(M2, 2, true),
    econfd_maapi:close(M2),
    Nctx.


test(DoHalt, IsInternal) ->
    try
        SELF = self(),
        Pid = spawn(fun() ->
                            Nctx = setup0(),
                            SELF ! {self(), ok, Nctx}
                    end),
        receive {Pid, ok, Nctx} -> ok end,
        Notif = proc_lib:spawn_link(fun() -> notif(SELF) end),
        receive {Notif, ok} -> ok end,
        timer:sleep(1000),
        bad_nc_notif(Nctx),
        myproc(),
        maapi(),
        cdb(),
        action(),
        cdbwrite(),
        types(),
        dp(),
        schema(IsInternal),
        xpath(),
        iid_keys(),
        iterate_crash(),   % trac #14192
        log(IsInternal),
        % chk_notif(Notif, IsInternal), % trac #14109
        chk_notif(Notif, false),

        io:format("all testcases ok \n",[]),
        timer:sleep(500),
        ehalt(DoHalt, 0)
    catch
        _:Err ->
            erep(Err),
            ehalt(DoHalt, 1)
    end.

ehalt(true, Code) -> erlang:halt(Code);
ehalt(false, 0) -> ok;
ehalt(false, 1) -> exit(error).



erep(Error) ->
    {ok, Wd} = file:get_cwd(),
    {M,Line} = case get('$line') of
                   undefined -> {undefined, undefined};
                   ML -> ML
               end,
    io:format("**************\n"
              "** test failed at ~p:~p\n"
              "   in ~p\n"
              "** reason: \n"
              "   ~p\n~p~n"
              "**************\n",
              [M, Line, Wd, Error, erlang:get_stacktrace()]),
    %% allow time to flush output
    timer:sleep(1000).


%% verify that a malformed nc_notif array doesn't crash ConfD
%% (used to happen with "internal" econfd since it bypassed a try/catch)
bad_nc_notif(Nctx) ->
    econfd:notification_send(Nctx, atime(), bad_exml()).

%% Just any time
atime() ->
    Y = 2007,
    Month = 3,
    D = 31,
    H = 23,
    Min = 23,
    S = 7,
    Mcr = 0,
    TZ = [],
    TZM = 0,
    {?C_DATETIME, {Y,Month,D,H,Min,S,Mcr,TZ,TZM}}.

bad_exml() ->
    [{[?notif__ns_uri|linkDown],start},
     {?CONFD_IPV4({2,3,4,5}),?CONFD_UINT32(1)},
     {[?notif__ns_uri|linkDown],stop}].


%% read procs stats and check that we are there
myproc() ->
    ?line {ok, M} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    NS = 'http://tail-f.com/ns/example/procs',
    ?line ok = econfd_maapi:start_user_session(M, <<"admin">>, <<"maapi">>,
                                               [<<"admin">>],
                                               {127,0,0,1}, ?CONFD_PROTO_TCP),

    ?line {ok,TH} = econfd_maapi:start_trans(M, ?CONFD_RUNNING,
                                             ?CONFD_READ),
    C = econfd_maapi:init_cursor(M, TH, [proc,procs,[NS|test]]),
    {PrevKey, Key} = find_proc(M, TH, econfd_maapi:get_next(C), self(), {}),
    CC1 = econfd_maapi:init_cursor(M, TH, [proc,procs,[NS|test]]),
    ?line {ok, Key, _} =
        econfd_maapi:find_next(CC1, ?CONFD_FIND_NEXT, PrevKey),
    CC2 = econfd_maapi:init_cursor(M, TH, [proc,procs,[NS|test]]),
    ?line {ok, Key, _} =
        econfd_maapi:find_next(CC2, ?CONFD_FIND_SAME_OR_NEXT, Key),
    CC3 = econfd_maapi:init_cursor(M, TH, [proc,procs,[NS|test]]),
    ?line {ok, FirstKey, _} =
        econfd_maapi:find_next(CC3, ?CONFD_FIND_SAME_OR_NEXT, {}),
    T = [FirstKey,proc,procs,[NS|test]],
    ?line {FirstPidBin} = FirstKey,
    ?line {ok, FirstPidBin} = econfd_maapi:get_elem(M,TH,[pid|T]),

    C2 = econfd_maapi:init_cursor(M, TH, [obj,objs,[NS|test]]),
    loop_objs(M, TH, econfd_maapi:get_next(C2), []),
    Num = econfd_maapi:num_instances(M, TH, [obj,objs,[NS|test]]),
    ?line {ok, 6} = Num,

    C3 = econfd_maapi:init_cursor(M, TH, [nobj,nobjs,[NS|test]]),
    NN = loop_nobjs(M, TH, econfd_maapi:get_next(C3), []),
    ?line {ok, NN} = econfd_maapi:num_instances(M, TH, [nobj,nobjs,[NS|test]]),
    ?line ok = econfd_maapi:finish_trans(M, TH).


loop_objs(_M, _TH, done, Ack) ->
    io:format("Accumulated ~p objects~n", [Ack]),
    ?line 4 = length(Ack);
loop_objs(M, TH, {ok, Key, C2}, Ack) ->
    NS = 'http://tail-f.com/ns/example/procs',
    T = [obj,objs,[NS|test]],
    Ret = econfd_maapi:get_elem(M,TH,[date, Key|T]),
    NAck = case Key of
               _ when Key == {1002}; Key == {1012} ->
                   %% Bad get_object() retval for this key - see procs.erl
                   ?line {error, {?CONFD_ERR_EXTERNAL, _}} = Ret,
                   Ack;
               _ ->
                   ?line {ok, Date} = Ret,
                   ?line Refs = case econfd_maapi:get_elem(M,TH,[refs,Key|T]) of
                                    %% empty leaf-list can not exist
                                    %% with new representation
                                    %% {ok, []}
                                    {error, {?CONFD_ERR_NOEXISTS, _}}
                                      when Key == {1001} ->
                                        [];
                                    {error, {?CONFD_ERR_NOEXISTS, _}}
                                      when Key == {1011} ->
                                        [];
                                    {ok, R = [?CONFD_OBJECTREF(R1), _R2]} ->
                                        ?line [s1,_Key|T] = R1,
                                        R
                                end,
                   [{Date, Refs}|Ack]
           end,
    loop_objs(M,TH,econfd_maapi:get_next(C2),NAck).


loop_nobjs(_M, _TH, done, Ack) ->
    N = length(Ack),
    ?line 7 = N,
    N;
loop_nobjs(M, TH, {ok, Key, C}, Ack) ->
    NS = 'http://tail-f.com/ns/example/procs',
    T = [nobj, nobjs, [NS|test]],
    ?line {ok, Str} = econfd_maapi:get_elem(M, TH, [s1, Key|T]),
    ?line {K1, _} = Key,
    io:format("Str=~p Key=~p~n",[Str,Key]),
    ?line Str = list_to_binary(integer_to_list(K1)),
    NAck = [Str|Ack],
    loop_nobjs(M, TH, econfd_maapi:get_next(C), NAck).


%% crash here
find_proc(_M,_TH, done, _, _) ->
    ?line exit(done_error);
find_proc(M,TH, {ok, Key, C}, Self, PrevKey) ->
    ?line {PidBin} = Key,
    Pid = list_to_pid(binary_to_list(PidBin)),
    if Pid == Self ->
            NS = 'http://tail-f.com/ns/example/procs',
            T = [Key,proc,procs,[NS|test]],
            ?line {ok, PidBin} = econfd_maapi:get_elem(M,TH,[pid|T]),
            {PrevKey, Key};
       true ->
            find_proc(M,TH, econfd_maapi:get_next(C), Self, Key)
    end.


cdbwrite() ->
    ?line {ok, S} = econfd_cdb:connect({127,0,0,1}, ?CONFD_PORT),
    ?line {ok, CDB} = econfd_cdb:new_session(S, ?CDB_OPERATIONAL),
    NS = ?cdbwrite__ns_uri,
    ItemsPath = [items, [NS|cdbw]],
    ItemPath = [item | ItemsPath],

    %% Create a single list entry with create() + set_elem()
    T = [{?CONFD_BUF(<<"str1">>)} | ItemPath],
    ?line ok = econfd_cdb:create(CDB, T),
    ?line ok = econfd_cdb:set_elem(CDB, ?CONFD_INT32(444), [val1|T]),
    ?line ok = econfd_cdb:set_elem(CDB,
                                   ?CONFD_ENUM_VALUE(?cdbwrite_complete),[e|T]),
    ?line ok = econfd_cdb:set_elem2(CDB, <<"complete">>,[e|T]),

    %% Create an additional server with set_object()
    ?line ok = econfd_cdb:set_object(CDB,
                                     [?CONFD_BUF(<<"strobj">>),
                                      ?CONFD_INT32(555),
                                      ?CONFD_ENUM_VALUE(?cdbwrite_complete)],
                                     ItemPath),

    %% And create one with set_values()
    TV = [{name, ?CONFD_BUF(<<"obj2">>)},
          {val1, ?CONFD_INT32(333)},
          {e, ?CONFD_ENUM_VALUE(?cdbwrite_complete)}],
    ?line ok = econfd_cdb:set_values(CDB, TV, ItemPath),

    %% Read with multi-value functions
    ?line {ok, [?CONFD_BUF(<<"str1">>), ?CONFD_INT32(444),
                ?CONFD_ENUM_VALUE(?cdbwrite_complete)]} =
        econfd_cdb:get_object(CDB, T),
    ?line {ok, [
                [?CONFD_BUF(<<"str1">>),
                 ?CONFD_INT32(444),
                 ?CONFD_ENUM_VALUE(?cdbwrite_complete)],
                [?CONFD_BUF(<<"strobj">>),
                 ?CONFD_INT32(555),
                 ?CONFD_ENUM_VALUE(?cdbwrite_complete)]
                ]} =
        econfd_cdb:get_objects(CDB, ItemPath, 1, 2),
    %% selection by key value
    GTV1 = [{item, start}, {name, ?CONFD_BUF(<<"obj2">>)}, {val1, not_found},
            {item, stop},
            {item, start}, {name, ?CONFD_BUF(<<"str1">>)}, {val1, not_found},
            {item, stop},
            {item, start}, {name, ?CONFD_BUF(<<"strobj">>)}, {val1, not_found},
            {item, stop}],
    %% selection by index
    GTV2 = [{item, {start, 0}}, {val1, not_found}, {item, stop},
            {item, {start, 1}}, {val1, not_found}, {item, stop},
            {item, {start, 2}}, {val1, not_found}, {item, stop}],
    ?line {ok, RTV1} = econfd_cdb:get_values(CDB, ItemsPath, GTV1),
    ?line [_, _, {[NS|val1], ?CONFD_INT32(333)}, _,
           _, _, {[NS|val1], ?CONFD_INT32(444)}, _,
           _, _, {[NS|val1], ?CONFD_INT32(555)}, _] = RTV1,
    RTV2 = [RTV || RTV <- RTV1, element(1, RTV) /= [NS|name]],
    ?line {ok, RTV2} = econfd_cdb:get_values(CDB, ItemsPath, GTV2),

    %% instance-identifier tests
    Objref = ?CONFD_OBJECTREF([val1|T]),
    ?line ok = econfd_cdb:set_elem(CDB, Objref, [objref,[NS|cdbw]]),
    ?line {ok, Objref} = econfd_cdb:get_elem(CDB, [objref,[NS|cdbw]]),

    %% error test - bad set_values() data (invalid tag 'e555')
    TV2 = [{name, ?CONFD_BUF(<<"obj2">>)},
           {val1, ?CONFD_INT32(333)},
           {e555, ?CONFD_ENUM_VALUE(?cdbwrite_complete)}],
    ?line {error, {?CONFD_ERR_BADTYPE, Err}} = econfd_cdb:set_values(
                                                 CDB, TV2,
                                                 [item, items,[NS|cdbw]]),
    io:format("Good err = '~s'~n", [binary_to_list(Err)]),

    %% set_case()/get_case()
    ?line ok = econfd_cdb:set_elem(CDB, ?CONFD_INT32(42), [a,[NS|cdbw]]),
    ?line ok = econfd_cdb:set_case(CDB, [[NS|cdbw]], ch, one),
    ?line {ok, ?CONFD_INT32(42)} = econfd_cdb:get_elem(CDB, [a,[NS|cdbw]]),
    ?line {ok, one} = econfd_cdb:get_case(CDB, [[NS|cdbw]], ch),

    ?line {ok, S} = econfd_cdb:end_session(CDB),
    ?line ok = econfd_cdb:close(CDB),

    cdb_write_sub().

cdb_write_sub() ->
    NS = ?cdbwrite__ns_uri,
    Self = self(),
    %% spawn an operational subscriber
    Pid =
        proc_lib:spawn_link(
          fun() ->
                  try
                      ?line {ok, S1} = econfd_cdb:connect({127,0,0,1}),
                      ?line {ok, Sub} = econfd_cdb:subscribe_session(S1),
                      ?line {ok, _Point1} =
                          econfd_cdb:subscribe(Sub, ?CDB_SUB_OPERATIONAL, 0,
                                               ?cdbwrite__ns_uri,
                                               "/cdbw/encrypted"),
                      ?line ok = econfd_cdb:subscribe_done(Sub),
                      Self ! {self(), sub_done},
                      F = fun(Update) ->
                                  io:format("Received oper update: ~p~n",
                                            [Update]),
                                  ?line {ok, S2} =
                                      econfd_cdb:connect({127,0,0,1}),
                                  ?line {ok, CDB} =
                                      econfd_cdb:new_session(
                                        S2, ?CDB_OPERATIONAL),
                                  ?line {ok, Des3Crypt} =
                                      econfd_cdb:get_elem(
                                        CDB, [des3, encrypted, [NS|cdbw]]),
                                  ?line {ok, AesCrypt} =
                                      econfd_cdb:get_elem(
                                        CDB, [aes, encrypted, [NS|cdbw]]),
                                  ?line {ok, Des3} = econfd:decrypt(Des3Crypt),
                                  ?line {ok, Aes} = econfd:decrypt(AesCrypt),
                                  Self ! {self(), Des3, Aes},
                                  ?line ok = econfd_cdb:close(CDB),
                                  close
                          end,
                      ?line ok = econfd_cdb:wait(Sub, 20000, F)
                  catch
                      _:Err ->
                          erep(Err),
                          exit(error)
                  end
          end),
    ?line ok = receive
                   {Pid, sub_done} ->
                       ok
               after 10000 ->
                       timeout
               end,

    %% write without and with subscription notification
    ?line {ok, S} = econfd_cdb:connect({127,0,0,1}),
    ?line {ok, CDB1} = econfd_cdb:new_session(S, ?CDB_OPERATIONAL, 0),
    ?line ok = econfd_cdb:set_elem2(CDB1, <<"foo">>,
                                    [des3, encrypted, [NS|cdbw]]),
    ?line {ok, S} = econfd_cdb:end_session(CDB1),
    ?line {ok, CDB2} =
        econfd_cdb:new_session(S, ?CDB_OPERATIONAL,
                               ?CDB_LOCK_REQUEST bor ?CDB_LOCK_WAIT),
    ?line ok = econfd_cdb:set_elem2(CDB2, <<"bar">>,
                                    [aes, encrypted, [NS|cdbw]]),
    ?line {<<"foo">>, <<"bar">>} = receive
                                       {Pid, V1, V2} ->
                                           {V1, V2}
                                   after 20000 ->
                                           timeout
                                   end,
    ?line ok = econfd_cdb:close(CDB2),

    %% trigger
    TPid =
        proc_lib:spawn_link(
          fun() ->
                  try
                      ?line {ok, S1} = econfd_cdb:connect({127,0,0,1}),
                      ?line {ok, Sub} = econfd_cdb:subscribe_session(S1),
                      ?line {ok, Point} =
                          econfd_cdb:subscribe(Sub, ?CDB_SUB_OPERATIONAL, 0,
                                               ?cdbwrite__ns_uri,
                                               "/cdbw/encrypted"),
                      ?line ok = econfd_cdb:subscribe_done(Sub),
                      Self ! {self(), Point},
                      F = fun(Update) ->
                                  io:format("Received oper update: ~p~n",
                                            [Update]),
                                  Self ! {self(), triggered},
                                  ?CDB_DONE_PRIORITY
                          end,
                      ?line ok = econfd_cdb:wait(Sub, 20000, F)
                  catch
                      _:Err ->
                          erep(Err),
                          exit(error)
                  end
          end),
    ?line {ok, Point} = receive
                            {TPid, P} ->
                                {ok, P}
                        after 10000 ->
                                timeout
                        end,
    ?line {ok, TS} = econfd_cdb:connect({127,0,0,1}),
    ?line [ok, ok, ok] =
        lists:map(fun (F) ->
                          TTPid = proc_lib:spawn_link(
                                    fun () ->
                                            ?line ok = F(),
                                            Self ! {self(), trig_done}
                                    end),
                          receive
                              {TPid, triggered} ->
                                  receive
                                      {TTPid, trig_done} ->
                                          ok
                                  after 10000 ->
                                          timeout
                                  end
                          after 10000 ->
                                  timeout
                          end
                  end,
                  [fun () ->
                           econfd_cdb:trigger_oper_subscriptions(TS)
                   end,
                   fun () ->
                           econfd_cdb:trigger_oper_subscriptions(TS, [Point])
                   end,
                   fun () ->
                           econfd_cdb:trigger_oper_subscriptions(TS, all, 0)
                   end]),
    ?line ok = econfd_cdb:close(TS).


cdb() ->
    ?line {ok, S} = econfd_cdb:connect({127,0,0,1}, ?CONFD_PORT),
    NS = ?smp__ns_uri,
    %% Now read out the single server
    %% make sure its there
    ?line {ok, CDB} = econfd_cdb:new_session(S, ?CDB_RUNNING),
    ?line {ok, ?CONFD_UINT16(77)} = econfd_cdb:get_elem(
                                      CDB,
                                      [port,{<<"smtp">>},server,[NS|servers]]),
    ?line {ok, true} = econfd_cdb:exists(
                                      CDB,
                                      [{<<"smtp">>},server,[NS|servers]]),
    ?line {ok, false} = econfd_cdb:exists(
                                      CDB,
                                      [{<<"noserver">>},server,[NS|servers]]),
    X = econfd_cdb:get_elem(CDB, [e,{<<"smtp">>},server,[NS|servers]]),
    ?line {ok, ?CONFD_ENUM_VALUE(?smp_complete)} = X,
    ?line {ok, mailserver = Case} =
        econfd_cdb:get_case(CDB, [{<<"smtp">>},server,[NS|servers]], type),
    ?line {ok, <<"gmail">>} =
        econfd_cdb:get_elem(CDB, [Case, {<<"smtp">>},server,[NS|servers]]),
    ?line {ok, 4} = econfd_cdb:num_instances(CDB, [server,[NS|servers]]),
    ?line {ok, 0} = econfd_cdb:next_index(CDB, [{'*'},server,[NS|servers]]),
    ?line {ok, 2} = econfd_cdb:next_index(CDB, [{<<"s">>},server,[NS|servers]]),
    ?line {ok, 3} = econfd_cdb:next_index(CDB,
                                          [{<<"smtp">>},server,[NS|servers]]),
    ?line {ok, S} = econfd_cdb:end_session(CDB),


    %% Fine setup a subscription point
    ?line {ok, S2} = econfd_cdb:connect({127,0,0,1}),
    ?line {ok, Sub} = econfd_cdb:subscribe_session(S2),
    ?line {ok, _Point1} = econfd_cdb:subscribe(Sub, 1, NS, "/servers/server"),
    ?line ok = econfd_cdb:subscribe_done(Sub),
    F = fun(Update) ->
                io:format("Received update: ~p~n", [Update]),

                %% Iterate over the changes
                Iter = fun(IKP, Op, OldValue, Value, State) ->
                               io:format("CDB_ITER:\n IKP=~p\n Op=~p "
                                         " value = ~p -> ~p~n",
                                         [IKP, Op, OldValue, Value]),
                               case IKP of
                                   [refs|T] ->
                                       IpRef = ?CONFD_OBJECTREF([ip|T]),
                                       ?line true = lists:member(IpRef, Value);
                                   _ ->
                                       ok
                               end,
                               {ok, ?ITER_RECURSE, State}
                       end,
                lists:foreach(
                  fun (Point) ->
                          econfd_cdb:diff_iterate(
                            Sub, Point, Iter, ?CDB_ITER_WANT_PREV, [])
                  end, Update),

                %% Now make sure that we can read the newly written val
                ?line {ok, CDB2} = econfd_cdb:new_session(S, ?CDB_RUNNING),
                ?line {ok, ?CONFD_UINT16(3)} =
                    econfd_cdb:get_elem(
                      CDB2,
                      [port,  {<<"aantp">>}, server,[NS|servers]]),
                ?line {ok, S} = econfd_cdb:end_session(CDB2),
                close
        end,

    %% spawn a maapi setter which creates another server
    proc_lib:spawn_link(
      fun() ->
              try
                  timer:sleep(1000),
                  ?line {ok, M} = econfd_maapi:connect({127,0,0,1},
                                                       ?CONFD_PORT),
                  NS = ?smp__ns_uri,
                  ?line ok = econfd_maapi:start_user_session(M, <<"admin">>,
                                                             <<"maapi">>,
                                                             [<<"admin">>],
                                                             {127,0,0,1},
                                                             ?CONFD_PROTO_TCP),
                  ?line {ok,TH} = econfd_maapi:start_trans(M, ?CONFD_RUNNING,
                                                           ?CONFD_READ_WRITE),
                  T = [{?CONFD_BUF(<<"aantp">>)}, server, [NS|servers]],
                  ?line ok = econfd_maapi:create(M, TH, T),
                  ?line ok = econfd_maapi:set_elem(M,TH, [ip|T],
                                                   ?CONFD_IPV4({3,5,6,7})),
                  ?line ok = econfd_maapi:set_elem2(M,TH, [port|T],<<"3">>),
                  Ref1 = ?CONFD_OBJECTREF([ip|T]),
                  Ref2 = ?CONFD_OBJECTREF([port|T]),
                  ?line ok = econfd_maapi:set_elem(M, TH, [refs|T],
                                                   [Ref1, Ref2]),
                  ?line ok = econfd_maapi:apply_trans(M, TH, false),
                  ?line ok = econfd_maapi:finish_trans(M, TH)
              catch
                  _:Err ->
                      erep(Err),
                      exit(error)
              end
      end),

    io:format("Enter wait \n",[]),
    ?line ok = econfd_cdb:wait(Sub, 20000, F),

    cdb_sub2(),
    cdb_sub3(),

    ok.


cdb_sub2() ->
    ?line {ok, S} = econfd_cdb:connect({127,0,0,1}, ?CONFD_PORT),
    NS = ?smp__ns_uri,
    T = [{?CONFD_BUF(<<"aantp">>)}, server, [NS|servers]],

    %% Fine setup a subscription point
    ?line {ok, S2} = econfd_cdb:connect({127,0,0,1}),
    ?line {ok, Sub} = econfd_cdb:subscribe_session(S2),
    ?line {ok, _Point1} = econfd_cdb:subscribe(Sub, 1, NS, "/servers/server"),
    ?line ok = econfd_cdb:subscribe_done(Sub),
    F = fun(Update) ->
                io:format("Received update: ~p~n", [Update]),

                %% Iterate over the changes
                Iter = fun(IKP, ?MOP_MODIFIED, undefined, undefined, State)
                             when IKP == T ->
                               {ok, ?ITER_RECURSE, State};
                          (IKP, ?MOP_VALUE_SET,
                           ?CONFD_IPV4({3,5,6,7}), ?CONFD_IPV4({1,2,3,4}),
                           State)
                             when IKP == [ip|T] ->
                               {ok, ?ITER_RECURSE, State}
                       end,
                lists:foreach(
                  fun (Point) ->
                          econfd_cdb:diff_iterate(
                            Sub, Point, Iter, ?CDB_ITER_WANT_PREV, [])
                  end, Update),

                %% CLI-iterate over the changes
                CIter = fun(IKP, ?MOP_MODIFIED, undefined, undefined,
                            <<"servers server aantp\n">>,
                            [{[NS1|servers], <<"servers">>},
                             {server, <<"server">>},
                             {name, <<>>},
                             {?CONFD_BUF(<<"aantp">>), <<"aantp">>}],
                            first)
                              when IKP == T, NS1 == NS ->
                               {ok, ?ITER_RECURSE, next};
                          (IKP, ?MOP_VALUE_SET,
                           ?CONFD_IPV4({3,5,6,7}), ?CONFD_IPV4({1,2,3,4}),
                           <<"ip 1.2.3.4\n">>,
                           [{ip, <<"ip">>},
                            {?CONFD_IPV4({1,2,3,4}), <<"1.2.3.4">>}],
                           next)
                              when IKP == [ip|T] ->
                                {ok, ?ITER_RECURSE, done}
                       end,
                lists:foreach(
                  fun (Point) ->
                          {ok, done} = econfd_cdb:cli_diff_iterate(
                            Sub, Point, CIter, ?CDB_ITER_WANT_PREV, first)
                  end, Update),

                %% Now make sure that we can read the newly written val
                ?line {ok, CDB2} = econfd_cdb:new_session(S, ?CDB_RUNNING),
                ?line {ok, ?CONFD_UINT16(3)} =
                    econfd_cdb:get_elem(
                      CDB2,
                      [port,  {<<"aantp">>}, server,[NS|servers]]),
                ?line {ok, S} = econfd_cdb:end_session(CDB2),
                close
        end,

    %% a maapi setter which modifies a value in the newly created server
    proc_lib:spawn_link(
      fun() ->
              try
                  timer:sleep(500),
                  ?line {ok, M} = econfd_maapi:connect({127,0,0,1},
                                                       ?CONFD_PORT),
                  NS = ?smp__ns_uri,
                  ?line ok = econfd_maapi:start_user_session(M, <<"admin">>,
                                                             <<"maapi">>,
                                                             [<<"admin">>],
                                                             {127,0,0,1},
                                                             ?CONFD_PROTO_TCP),
                  ?line {ok,TH} = econfd_maapi:start_trans(M, ?CONFD_RUNNING,
                                                           ?CONFD_READ_WRITE),
                  ?line ok = econfd_maapi:set_elem(M,TH, [ip|T],
                                                   ?CONFD_IPV4({1,2,3,4})),
                  ?line ok = econfd_maapi:apply_trans(M, TH, false),
                  ?line ok = econfd_maapi:finish_trans(M, TH)
              catch
                  _:Err ->
                      erep(Err),
                      exit(error)
              end
      end),

    io:format("Enter wait2 \n",[]),
    ?line ok = econfd_cdb:wait(Sub, 20000, F),

    ok.

%% multi-sub with non-string keys, Ns == ''
cdb_sub3() ->
    ?line {ok, S} = econfd_cdb:connect({127,0,0,1}, ?CONFD_PORT),
    NS = ?smp__ns_uri,
    T = [{?CONFD_IPV4({1,2,3,4}), ?CONFD_UINT16(80)}, [NS|service]],

    %% Fine setup a subscription point
    ?line {ok, S2} = econfd_cdb:connect({127,0,0,1}),
    ?line {ok, Sub} = econfd_cdb:subscribe_session(S2),
    %% This subscription should not trigger
    ?line {ok, _Point0} = econfd_cdb:subscribe(Sub, 1, NS, "/servers"),
    Path = "/smp:service{1.2.3.4 *}/data/int",
    ?line {ok, Point} = econfd_cdb:subscribe(Sub, 2, '', Path),
    ?line ok = econfd_cdb:subscribe_done(Sub),
    F = fun(Update) ->
                io:format("Received update: ~p~n", [Update]),
                ?line [Point] = Update,
                %% Now make sure that we can read the newly written val
                ?line {ok, CDB2} = econfd_cdb:new_session(S, ?CDB_RUNNING),
                ?line {ok, ?CONFD_INT32(3)} =
                    econfd_cdb:get_elem(CDB2, [int, data|T]),
                ?line {ok, S} = econfd_cdb:end_session(CDB2),
                close
        end,

    %% a maapi setter which creates another service (and sets the values)
    proc_lib:spawn_link(
      fun() ->
              try
                  timer:sleep(500),
                  ?line {ok, M} = econfd_maapi:connect({127,0,0,1},
                                                       ?CONFD_PORT),
                  ?line ok = econfd_maapi:start_user_session(M, <<"admin">>,
                                                             <<"maapi">>,
                                                             [<<"admin">>],
                                                             {127,0,0,1},
                                                             ?CONFD_PROTO_TCP),
                  ?line {ok,TH} = econfd_maapi:start_trans(M, ?CONFD_RUNNING,
                                                           ?CONFD_READ_WRITE),
                  ?line ok = econfd_maapi:create(M, TH, T),
                  ?line ok = econfd_maapi:set_elem(M,TH,
                                                   [int, data|T],
                                                   ?CONFD_INT32(3)),
                  ?line ok = econfd_maapi:set_elem(M,TH,
                                                   [string, data|T],
                                                   ?CONFD_BUF(<<"foo">>)),
                  ?line ok = econfd_maapi:apply_trans(M, TH, false),
                  ?line ok = econfd_maapi:finish_trans(M, TH)
              catch
                  _:Err ->
                      erep(Err),
                      exit(error)
              end
      end),

    io:format("Enter wait2 \n",[]),
    ?line ok = econfd_cdb:wait(Sub, 20000, F),

    ok.


maapi() ->
    io:format("MAAPI()\n",[]),
    ?line {ok, M} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    ?line ok = econfd_maapi:install_crypto_keys(M),
    NS = ?smp__ns_uri,
    ?line ok = econfd_maapi:start_user_session(M, <<"admin">>, <<"maapi">>,
                                               [<<"admin">>], {127,0,0,1},
                                                ?CONFD_PROTO_TCP),
    ?line {ok,TH} = econfd_maapi:start_trans(M, ?CONFD_RUNNING,
                                             ?CONFD_READ_WRITE),

    %% cleanup
    T = [server, [NS|servers]],
    ?line econfd_maapi:delete(M,TH, T),
    ?line econfd_maapi:delete(M,TH, [[NS|service]]),
    NST = 'http://tail-f.com/ns/example/types',
    ?line econfd_maapi:delete(M, TH, [x, [NST|root]]),

    %% create a server entry
    ?line {ok, false} =
        econfd_maapi:exists(M, TH, [{?CONFD_BUF(<<"smtp">>)} |T]),
    ?line ok = econfd_maapi:create(M, TH, [{?CONFD_BUF(<<"smtp">>)} |T]),
    ?line {ok, true} =
        econfd_maapi:exists(M, TH, [{?CONFD_BUF(<<"smtp">>)} |T]),
    ?line ok = econfd_maapi:set_elem2(M,TH, [ip,{?CONFD_BUF(<<"smtp">>)} |T],
                                      <<"3.4.5.6">>),
    ?line ok = econfd_maapi:set_elem2(M,TH,[port, {?CONFD_BUF(<<"smtp">>)}|T],
                                      <<"77">>),
    ?line ok = econfd_maapi:set_elem2(M,TH,
                                      [mailserver, {?CONFD_BUF(<<"smtp">>)}|T],
                                      <<"gmail">>),

    ?line ok = econfd_maapi:apply_trans(M, TH, false),
    ?line ok = econfd_maapi:finish_trans(M, TH),

    %% create another server entry
    ?line {ok,TH2} = econfd_maapi:start_trans(M, ?CONFD_RUNNING,
                                              ?CONFD_READ_WRITE),
    ?line ok = econfd_maapi:create(M, TH2, [{?CONFD_BUF(<<"www">>)}|T]),
    ?line ok = econfd_maapi:set_elem2(M,TH2, [ip,{?CONFD_BUF(<<"www">>)}|T],
                                      <<"3.4.5.6">>),
    ?line ok = econfd_maapi:set_elem2(M,TH2, [port,{?CONFD_BUF(<<"www">>)} |T],
                                      <<"34">>),
    ?line ok = econfd_maapi:set_elem2(M,TH2,
                                      [webserver, {?CONFD_BUF(<<"www">>)}|T],
                                      <<"google">>),
    X = econfd_maapi:get_elem(M, TH2, [e, {?CONFD_BUF(<<"www">>)} |T]),
    ?line {ok, ?CONFD_ENUM_VALUE(?smp_complete)} = X,

    %% one more with set_object()
    VL = [
          not_found,                            % name (key)
          ?CONFD_IPV4({1,2,3,4}),               % ip
          ?CONFD_UINT16(21),                    % port
          ?CONFD_ENUM_VALUE(?smp_invalid),      % e
          not_found,                            % refs
          ?CONFD_XMLTAG(NS, foo),               % foo (container)
          not_found,                            % ee (list)
          not_found,                            % webserver
          not_found                             % mailserver
         ],
    ?line ok = econfd_maapi:set_object(M, TH2, [{?CONFD_BUF(<<"ftp">>)}|T], VL),

    % and one with set_values(), including sub-list entry
    TVL = [{ip, ?CONFD_IPV4({1,2,3,4})},
           {port, ?CONFD_UINT16(2)},
           {foo, start},
           {ee, start},
           {e, ?CONFD_ENUM_VALUE(?smp_unavailable)},
           {myref, ?CONFD_OBJECTREF([[NS|servers]])},
           {ee, stop},
           {foo, stop}],
    ?line ok = econfd_maapi:set_values(M, TH2,[{?CONFD_BUF(<<"http">>)}|T],TVL),

    %% copy the sub-container/list to "ftp" entry with copy_tree
    ?line ok = econfd_maapi:copy_tree(M, TH2, [foo, {?CONFD_BUF(<<"http">>)}|T],
                                       [foo, {?CONFD_BUF(<<"ftp">>)}|T]),

    ?line ok = econfd_maapi:set_label(M, TH2, <<"test_label">>),
    ?line ok = econfd_maapi:set_comment(M, TH2, <<"this is a comment">>),

    ?line ok = econfd_maapi:apply_trans(M, TH2, false),
    ?line ok = econfd_maapi:finish_trans(M, TH2),


    %% read the server entries with multi-value functions
    ?line {ok,TH3} = econfd_maapi:start_trans(M, ?CONFD_RUNNING, ?CONFD_READ),
    ObjSMTP = [?CONFD_BUF(<<"smtp">>), ?CONFD_IPV4({3,4,5,6}),
               ?CONFD_UINT16(77), ?CONFD_ENUM_VALUE(?smp_complete),
               not_found,                       % leaf-list refs
               ?CONFD_XMLTAG(NS, foo),
               not_found,                       % list ee
               not_found, ?CONFD_BUF(<<"gmail">>)],
    ?line {ok, ObjSMTP} =
        econfd_maapi:get_object(M, TH3, [{?CONFD_BUF(<<"smtp">>)} |T]),

    ObjHTTP = [?CONFD_BUF(<<"http">>), ?CONFD_IPV4({1,2,3,4}),
               ?CONFD_UINT16(2), ?CONFD_ENUM_VALUE(?smp_complete),
               not_found,                       % leaf-list refs
               ?CONFD_XMLTAG(NS, foo),
               not_found,                       % list ee
               not_found, not_found],
    ObjWWW = [?CONFD_BUF(<<"www">>), ?CONFD_IPV4({3,4,5,6}),
              ?CONFD_UINT16(34), ?CONFD_ENUM_VALUE(?smp_complete),
              not_found,                       % leaf-list refs
              ?CONFD_XMLTAG(NS, foo),
              not_found,                       % list ee
              ?CONFD_BUF(<<"google">>), not_found],

    C0 = econfd_maapi:init_cursor(M, TH3, T),
    ?line {ok, {<<"ftp">>}, C1} = econfd_maapi:get_next(C0),       % skip "ftp"
    ?line {ok, C2, [ObjHTTP, ObjSMTP]} = econfd_maapi:get_objects(C1, 2),
    ?line {done, [ObjWWW]} = econfd_maapi:get_objects(C2, 2),
    C00 = econfd_maapi:init_cursor(M, TH3, T),
    ?line {ok, {<<"ftp">>}, C01} = econfd_maapi:get_next(C00),     % skip "ftp"
    ?line {ok, C02, [ObjHTTP, ObjSMTP, ObjWWW]} =
        econfd_maapi:get_objects(C01, 3),
    ?line done = econfd_maapi:get_next(C02),

    TVL2 = [{ip, not_found},
            {port, not_found},
            {foo, start},
            {ee, start},
            {e, ?CONFD_ENUM_VALUE(?smp_unavailable)},
            {myref, not_found},
            {ee, stop},
            {foo, stop}],
    ?line {ok, TVL3} =
        econfd_maapi:get_values(M, TH3, [{?CONFD_BUF(<<"http">>)}|T], TVL2),
    ?line TVL = [{Tag, Val} || {[_Ns|Tag], Val} <- TVL3],

    %%  verify copy_tree
    ?line {ok, ?CONFD_OBJECTREF([[NS|servers]])} =
        econfd_maapi:get_elem(M, TH3,
                              [myref, {?CONFD_ENUM_VALUE(?smp_unavailable)},
                               ee, foo, {?CONFD_BUF(<<"ftp">>)}|T]),

    ?line ok = econfd_maapi:finish_trans(M, TH3),


    %% trans-in-trans
    ?line {ok,TH4} = econfd_maapi:start_trans(M, ?CONFD_RUNNING,
                                              ?CONFD_READ_WRITE),
    ?line {ok,TH5} = econfd_maapi:start_trans_in_trans(M, ?CONFD_READ_WRITE,
                                                       0, TH4),
    ?line ok = econfd_maapi:set_elem2(M,TH5, [port,{?CONFD_BUF(<<"www">>)} |T],
                                      <<"88">>),
    ?line {ok, ?CONFD_UINT16(34)} =
        econfd_maapi:get_elem(M, TH4, [port,{?CONFD_BUF(<<"www">>)} |T]),
    ?line {ok, ?CONFD_UINT16(88)} =
        econfd_maapi:get_elem(M, TH5, [port,{?CONFD_BUF(<<"www">>)} |T]),
    ?line ok = econfd_maapi:apply_trans(M, TH5, false),
    ?line {ok, ?CONFD_UINT16(88)} =
        econfd_maapi:get_elem(M, TH4, [port,{?CONFD_BUF(<<"www">>)} |T]),
    ?line ok = econfd_maapi:finish_trans(M, TH5),
    ?line ok = econfd_maapi:finish_trans(M, TH4),


    ?line ok = m_create(M),
    ?line ok = m_test_validate(M),
    ?line ok = m_test_ordered(M),
    ?line ok = m_test_auth_cb(M),

    ?line ok = econfd_maapi:end_user_session(M),
    ?line econfd_maapi:close(M).



%% unused...
m_create_server(Name) ->
    io:format("M_CREATE_SERVER(~p)~n", [Name]),
    ?line {ok, M} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    NS = ?smp__ns_uri,
    ?line ok = econfd_maapi:start_user_session(M, <<"admin">>, <<"maapi">>,
                                               [<<"admin">>], {127,0,0,1},
                                                ?CONFD_PROTO_TCP),
    ?line {ok,TH} = econfd_maapi:start_trans(M, ?CONFD_RUNNING,
                                             ?CONFD_READ_WRITE),
    T = [server, [NS|servers]],
    ok = econfd_maapi:create(M, TH, [{?CONFD_BUF(Name)} |T]),
    ok = econfd_maapi:set_elem2(M,TH, [ip,{?CONFD_BUF(Name)} |T],
                                <<"3.4.5.6">>),
    ok = econfd_maapi:set_elem2(M,TH,[port, {?CONFD_BUF(Name)}|T],
                                <<"77">>),
    ok = econfd_maapi:apply_trans(M, TH, false),
    ok = econfd_maapi:finish_trans(M, TH),
    ok = econfd_maapi:end_user_session(M),
    econfd_maapi:close(M).


%% test enums as keys
m_create(M) ->
    io:format("M_CREATE(~p)~n", [M]),
     NS = ?smp__ns_uri,
    ?line {ok,TH} = econfd_maapi:start_trans(M, ?CONFD_RUNNING,
                                             ?CONFD_READ_WRITE),

    T = [{?CONFD_BUF(<<"smtp">>)},server, [NS|servers]],
    ?line ok = econfd_maapi:create(M, TH, [{?CONFD_ENUM_VALUE(?smp_complete)},
                                           ee,foo|T]),
    MyRef1 = ?CONFD_OBJECTREF([[NS|servers]]),
    ?line ok = econfd_maapi:set_elem2(M, TH,
                                      [myref,
                                       {?CONFD_ENUM_VALUE(?smp_complete)},
                                       ee,foo|T], <<"/servers">>),

    ?line {error, {?CONFD_ERR_NOEXISTS, _}} =
        econfd_maapi:delete(M, TH,
                            [{?CONFD_ENUM_VALUE(?smp_incomplete)}, ee,foo|T]),

    ?line ok = econfd_maapi:create(M, TH, [{?CONFD_ENUM_VALUE(?smp_incomplete)},
                                           ee,foo|T]),
    MyRef2 = ?CONFD_OBJECTREF(T),
    ?line ok = econfd_maapi:set_elem(M, TH,
                                      [myref,
                                       {?CONFD_ENUM_VALUE(?smp_incomplete)},
                                       ee,foo|T],
                                     MyRef2),


    ?line {error, {?CONFD_ERR_BADPATH, Err0}} =
        econfd_maapi:create(M, TH,
                            [{?CONFD_INT32(42)},ee,foo|T]),
    io:format("Good err = '~s'~n", [binary_to_list(Err0)]),
    ?line {error, {?CONFD_ERR_BADPATH, Err1}} =
        econfd_maapi:create(M, TH,
                            [{?CONFD_ENUM_VALUE(<<"xxxcomplete">>)},ee,foo|T]),
    io:format("Good err = '~s'~n", [binary_to_list(Err1)]),
    ?line {error, {?CONFD_ERR_BADPATH, Err2}} =
        econfd_maapi:create(M, TH,
                            [{?CONFD_ENUM_VALUE( ?smp_incomplete),
                              ?CONFD_ENUM_VALUE( ?smp_complete)},ee,foo|T]),
    io:format("Good err = '~s'~n", [binary_to_list(Err2)]),
    ?line {error, {?CONFD_ERR_BADPATH, Err3}} =
        econfd_maapi:create(M, TH,
                            [{?CONFD_ENUM_VALUE( ?smp_incomplete)},bar,foo|T]),
    io:format("Good err = '~s'~n", [binary_to_list(Err3)]),
    ?line {error, {?CONFD_ERR_ALREADY_EXISTS, _}} =
        econfd_maapi:create(M, TH,
                            [{?CONFD_ENUM_VALUE( ?smp_incomplete)},ee,foo|T]),


    ?line {ok, MyRef1} =
        econfd_maapi:get_elem(M, TH, [myref,
                                      {?CONFD_ENUM_VALUE(?smp_complete)},
                                      ee,foo|T]),
    ?line {ok, MyRef2} =
        econfd_maapi:get_elem(M, TH, [myref,
                                      {?CONFD_ENUM_VALUE(?smp_incomplete)},
                                      ee,foo|T]),

    ?line ok = econfd_maapi:apply_trans(M, TH, false),
    ?line ok = econfd_maapi:finish_trans(M, TH).

%% test validation error/warning
m_test_validate(M) ->
    io:format("M_TEST_VALIDATE(~p)~n", [M]),
    NS = ?smp__ns_uri,
    T = [server, [NS|servers]],
    ?line {ok,TH} = econfd_maapi:start_trans(M, ?CONFD_RUNNING,
                                             ?CONFD_READ_WRITE),

    ?line ok = econfd_maapi:set_elem(M,TH, [port,{?CONFD_BUF(<<"www">>)} |T],
                                      ?CONFD_UINT16(224)),
    ?line {error, {?CONFD_ERR_EXTERNAL, Msg}} =
        econfd_maapi:validate_trans(M, TH, true, false),
    ?line <<"/smp:servers: Sum of portnumbers is > 300 -- it is 324">> = Msg,

    ?line ok = econfd_maapi:set_elem(M,TH, [port,{?CONFD_BUF(<<"www">>)} |T],
                                      ?CONFD_UINT16(200)),
    ?line {error, {?CONFD_ERR_VALIDATION_WARNING, Msgs}} =
        econfd_maapi:validate_trans(M, TH, true, false),
    ?line ["/smp:servers", "Sum of portnumbers is just 300"] =
        string:tokens(binary_to_list(Msgs), [0]),

    ?line ok = econfd_maapi:set_elem(M,TH, [port,{?CONFD_BUF(<<"www">>)} |T],
                                      ?CONFD_UINT16(180)),

    ?line ok = econfd_maapi:apply_trans(M, TH, false),
    ?line ok = econfd_maapi:finish_trans(M, TH).

%% test ordered-by user
m_test_ordered(M) ->
    io:format("M_TEST_ORDERED(~p)~n", [M]),
    NS = 'http://tail-f.com/test/ytest',
    KP = [entry, [NS|obu]],

    ?line {ok,TH} = econfd_maapi:start_trans(M, ?CONFD_RUNNING,
                                             ?CONFD_READ_WRITE),

    ?line ok = econfd_maapi:create(M, TH, [{?CONFD_INT32(3)}|KP]),
    ?line ok = econfd_maapi:set_elem2(M, TH,
                                      [value,{?CONFD_INT32(3)}|KP], <<"33">>),

    ?line ok = econfd_maapi:create(M, TH, [{?CONFD_INT32(1)}|KP]),
    ?line ok = econfd_maapi:set_elem2(M, TH,
                                      [value,{?CONFD_INT32(1)}|KP], <<"11">>),

    ?line ok = econfd_maapi:create(M, TH, [{?CONFD_INT32(7)}|KP]),
    ?line ok = econfd_maapi:set_elem2(M, TH,
                                      [value,{?CONFD_INT32(7)}|KP], <<"77">>),

    ?line ok = econfd_maapi:create(M, TH, [{?CONFD_INT32(4)}|KP]),
    ?line ok = econfd_maapi:set_elem2(M, TH,
                                      [value,{?CONFD_INT32(4)}|KP], <<"44">>),

    ?line ok = econfd_maapi:apply_trans(M, TH, false),
    ?line ok = econfd_maapi:finish_trans(M, TH),

    ?line ok = m_check_order(M, KP, [3, 1, 7, 4]),

    ?line {ok,TH2} = econfd_maapi:start_trans(M, ?CONFD_RUNNING,
                                              ?CONFD_READ_WRITE),

    ?line ok = econfd_maapi:move_ordered(M, TH2, [{?CONFD_INT32(7)}|KP],
                                         'first'),
    ?line ok = econfd_maapi:move_ordered(M, TH2, [{?CONFD_INT32(3)}|KP],
                                         'last'),
    ?line ok = econfd_maapi:move_ordered(M, TH2, [{?CONFD_INT32(1)}|KP],
                                         {'after', {?CONFD_INT32(3)}}),
    ?line ok = econfd_maapi:move_ordered(M, TH2, [{?CONFD_INT32(4)}|KP],
                                         {'before', {?CONFD_INT32(1)}}),

    ?line ok = econfd_maapi:apply_trans(M, TH2, false),
    ?line ok = econfd_maapi:finish_trans(M, TH2),

    ?line ok = m_check_order(M, KP, [7, 3, 4, 1]).

m_check_order(M, KP, L) ->
    ?line {ok,TH} = econfd_maapi:start_trans(M, ?CONFD_RUNNING, ?CONFD_READ),
    C = econfd_maapi:init_cursor(M, TH, KP),
    ?line ok = m_check_order(M, TH, KP, econfd_maapi:get_next(C), L),
    ?line ok = econfd_maapi:finish_trans(M, TH).

m_check_order(M, TH, KP, {ok, {?CONFD_INT32(Key)}, C}, [Key|Rest]) ->
    Val = ?CONFD_INT32(Key * 11),
    ?line {ok, Val} = econfd_maapi:get_elem(M, TH,
                                            [value,{?CONFD_INT32(Key)}|KP]),
    m_check_order(M, TH, KP, econfd_maapi:get_next(C), Rest);
m_check_order(_M, _TH, _KP, done, []) ->
    ok.

m_test_auth_cb(M) ->
    ?line {ok, _Groups} =
        econfd_maapi:authenticate(M, <<"admin">>, <<"admin">>,[]),
    ?line {ok, {0, <<"Access denied">>}} =
        econfd_maapi:authenticate(M, <<"oper">>, <<"oper">>,[]),
    ?line {ok, {0, <<"Bad password">>}} =
        econfd_maapi:authenticate(M, <<"oper">>, <<"badpass">>,[]),
    ok.

%% We need to attach this transaction to maapi, since we're
%% planning to read
vinit(Tctx) ->
    M = (Tctx#confd_trans_ctx.dx)#confd_daemon_ctx.d_opaque,
    NS = ?smp__ns_uri,
    ok = econfd_maapi:attach(M, NS, Tctx),
    ok.

vstop(_Tctx) ->
    ok.

iter_validate(IKP, Op, OldValue, Value, State) ->
    io:format("ITER_validate: IKP = ~p Op=~p, value=~p~n",
              [IKP, Op, Value]),
    case iter_check(IKP, Op, OldValue, Value) of
        ok ->
            {ok, ?ITER_RECURSE, newstate(State, lists:reverse(IKP))};
        Error ->
            Error
    end.

newstate(serverstate, _) ->
    serverstate;
newstate(_, [[_|servers],server|_]) ->
    serverstate;
newstate(_, _) ->
    newstate.

%% sanity checks
iter_check(_IKP, ?MOP_CREATED, undefined, undefined) ->
    ok;
iter_check(_IKP, ?MOP_DELETED, undefined, undefined) ->
    ok;
iter_check([Key|_], ?MOP_MODIFIED, undefined, undefined)
  when is_tuple(Key) ->
    ok;
iter_check(_IKP, _Op, _OldValue, ?CONFD_OBJECTREF([Tag|_]))
  when is_integer(Tag) ->
    {error, "Hash in objectRef"};
iter_check([Tag|_], ?MOP_VALUE_SET, undefined, _Value)
  when is_atom(Tag); is_list(Tag) ->
    ok;
iter_check([Key|_Path], ?MOP_MOVED_AFTER, undefined, {})
  when is_tuple(Key) ->
    ok;
iter_check([Key|_Path], ?MOP_MOVED_AFTER, undefined, Value)
  when is_tuple(Key), is_tuple(Value) ->
    ok;
iter_check(_IKP, _Op, _OldValue, _Value) ->
    {error, "Bad arguments to iter()"}.

validate2(Tctx, IKP, ?CONFD_ENUM_VALUE( NewVal)) when is_integer(NewVal) ->
    io:format("Validate2 ~p --> ~p~n", [IKP, NewVal]),
    Thandle = Tctx#confd_trans_ctx.thandle,
    M = (Tctx#confd_trans_ctx.dx)#confd_daemon_ctx.d_opaque,
    {ok, Newstate} = econfd_maapi:diff_iterate(M, Thandle,
                                               fun iter_validate/5,
                                               initstate),
    %econfd_maapi:detach(M, Thandle),
    io:format("RETURN from diff_iterate(), Newstate=~p\n",[Newstate]),
    NS = ?smp__ns_uri,
    IterKP = [server, [NS|servers]],
    {ok, Newstate2} = econfd_maapi:keypath_diff_iterate(M, Thandle, IterKP,
                                                        fun iter_validate/5,
                                                        initstate),
    io:format("RETURN from keypath_diff_iterate(), Newstate2=~p\n",[Newstate2]),
    if Newstate == newstate, Newstate2 == initstate -> ok;
       Newstate == serverstate, Newstate2 == serverstate -> ok;
       %% This should ideally not happen - callback invocation w/o diff
       %% - but we accept it here
       Newstate == initstate, Newstate2 == initstate -> ok;
       %% Anything else should be a diff_iterate bug
       true ->
            ErrStr = io_lib:format("Bad results from diff_iterate, "
                                   "Newstate=~p Newstate2=~p~n",
                                   [Newstate, Newstate2]),
            {error, list_to_binary(ErrStr)}
    end.



validate(Tctx, IKP, NewVal) ->
    io:format("Validate ~p --> ~p~n", [IKP, NewVal]),
    M = (Tctx#confd_trans_ctx.dx)#confd_daemon_ctx.d_opaque,
    Th = Tctx#confd_trans_ctx.thandle,
    %% Let's traverse all the servers and make sure the sum of the
    %% port numbers is < 300 :-)
    NS = ?smp__ns_uri,
    io:format("Th = ~p~n", [Th]),
    C = econfd_maapi:init_cursor(M, Th, [server, [NS|servers]]),
    trav(M, Th, C, econfd_maapi:get_next(C), 0).




trav(_M, _Th, _C, done, Sum) when Sum > 300 ->
    Estr = "Sum of portnumbers is > 300 -- it is " ++ integer_to_list(Sum),
    {error, list_to_binary(Estr)};
trav(_M, _Th, _C, done, Sum) when Sum == 300 ->
    {validation_warn, <<"Sum of portnumbers is just 300">>};
trav(_M, _Th, _C, done, _Sum)  ->
    ok;
trav(M, Th, _C, {ok, Key, C2}, Sum) ->
    NS = ?smp__ns_uri,
    {ok, ?CONFD_UINT16(Pno)} = econfd_maapi:get_elem(
                                M, Th,
                                [port,Key,server,[NS|servers]]),
    trav(M, Th,C2, econfd_maapi:get_next(C2), Pno+Sum);
trav(_M, _, _C, Err, _) ->
    error_logger:format("liberr: ~p~n", [Err]),
    {error, <<"lib error">>}.


%% Test actions with maapi and cli
action() ->
    action_maapi(),
    action_cli().

action_maapi() ->
    ?line {ok, M} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    NS = 'http://tail-f.com/ns/example/action',
    ?line ok = econfd_maapi:start_user_session(M, <<"admin">>, <<"maapi">>,
                                               [<<"admin">>],
                                               {127,0,0,1}, ?CONFD_PROTO_TCP),
    IKP = [math, {?CONFD_BUF(<<"fred">>)}, host, [NS|top]],
    P1 =  [{add, start}, {op1, 2}, {op2, 3}, {add, stop}],
    ?line {ok, [{[NS|result], 5}]} = econfd_maapi:request_action(M, P1, IKP),
    P2 =  [{del, start}, {op1, 65}, {op2, 23}, {del, stop}],
    ?line {ok, [{[NS|result], 42}]} = econfd_maapi:request_action(M, P2, IKP),
    PErr =  [{del, start}, {op1, 65}, {del, stop}],
    ?line {error, {?CONFD_ERR_NOTSET, Bin}} =
        econfd_maapi:request_action(M, PErr, IKP),
    ?line true = (string:str(binary_to_list(Bin), "/op2 is not provided") /= 0),
    ?line ok = econfd_maapi:close(M).

action_cli() ->
    P = open_port({spawn, "confd_cli -n -u admin"}, [eof]),
    ?line ok = cli_expect(P, ">", []),
    port_command(P, "request top host fred reboot\n"),
    ?line ok = cli_expect(P, "Really reboot? ", []),
    port_command(P, "yes\n"),
    ?line ok = cli_expect(P, "When? ", []),
    port_command(P, "soon\n"),
    ?line ok = cli_expect(P, "When? ", []),
    port_command(P, "later\n"),
    ?line ok = cli_expect(P, ">", []),
    port_command(P, "exit\n"),
    ?line ok = cli_expect_eof(P).

cli_expect(P, Str, Acc0) ->
    receive
        {P, {data, Data}} ->
            Acc = Acc0 ++ Data,
            case string:str(Acc, Str) of
                0 ->
                    cli_expect(P, Str, Acc);
                _ ->
                    ok
            end
    after 30000 ->
            timeout
    end.

cli_expect_eof(P) ->
    receive
        {P, eof} ->
            ok;
        {P, _} ->
            cli_expect_eof(P)
    after 30000 ->
            timeout
    end.


%% action callbacks

math_cs(Uinfo, Name, IKeyPath, Params) ->
    #confd_user_info{ username = <<"admin">> } = Uinfo, % assertion
    [Ns|math] = Name,
    [{<<"fred">>}, host, [Ns|top]] = IKeyPath, % assertion
    [{[Ns|Oper], start}, {[Ns|op1], Op1},
     {[Ns|op2], Op2}, {[Ns|Oper], stop}] = Params,
    Result = math_calc(Oper, Op1, Op2),
    {ok, [{[Ns|result], Result}]}.      % Ns is optional

math_calc(add, Op1, Op2) ->
    Op1 + Op2;
math_calc(del, Op1, Op2) ->
    Op1 - Op2.

reboot(Uinfo, Name, IKeyPath, Params) ->
    #confd_user_info{ username = <<"admin">> } = Uinfo, % assertion
    [] = Params,      % assertion
    [Ns|reboot] = Name,
    [{<<"fred">>}, host, [Ns|top]] = IKeyPath, % assertion
    Usid = Uinfo#confd_user_info.usid,
    {ok, S} = econfd_maapi:connect({127,0,0,1},?CONFD_PORT),
    {ok, <<"yes">>} =
        econfd_maapi:cli_prompt(S, Usid, <<"Really reboot? ">>, true),
    {ok, <<"later">>} =
        econfd_maapi:cli_prompt_oneof(S, Usid, <<"When? ">>,
                                      [<<"now">>,<<"later">>], 10),
    ok.

auth_check(#confd_authentication_ctx{uinfo = Uinfo, success = Success})
  when Uinfo#confd_user_info.username == <<"oper">> ->
    if Success ->
            {error, <<"Access denied">>};
       true ->
            error
    end;
auth_check(_) ->
    ok.

notif(Top) ->
    try notif2(Top) of
        ok -> ok
    catch
        throw:done ->
            Top ! {self(), done},
            %% For manual eventmgr blocking test - see below
            receive
                {Top, sleep} ->
                    receive stop -> ok end
            after 0 ->
                    ok
            end,
            ok;
        _:Err ->
            erep(Err),
            exit(err)
    end.

chk_notif(Notif, TestBlocking) ->
    if TestBlocking ->
            Notif ! {self(), sleep};
       true ->
            ok
    end,
    ?line {ok, MaapiSock} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    ?line ok = econfd_maapi:start_user_session(MaapiSock,
                                               <<"stop">>, <<"maapi">>,
                                               [<<"admin">>],
                                               {127,0,0,1}, ?CONFD_PROTO_TCP),
    receive {Notif, done} -> ok end,
    ?line {ok, Bin} = file:read_file("notif.out"),
    ?line {_, _} = binary:match(Bin, <<"NOTIF: Audit message">>),
    ?line {_, _} = binary:match(Bin, <<"NOTIF: User session">>),
    ?line {_, _} = binary:match(Bin, <<"NOTIF: Commit simple">>),
    ?line {_, _} = binary:match(Bin, <<"NOTIF: Commit diff">>),
    ?line {_, _} = binary:match(Bin, <<"comment=undefined label=undefined">>),
    CL = <<"comment=<<\"this is a comment\">> label=<<\"test_label\">>">>,
    ?line {_, _} = binary:match(Bin, CL),
    ?line {_, _} = binary:match(Bin, <<"ITER: IKP">>),
    ?line {_, _} = binary:match(Bin, <<"NOTIF: Confirmed commit">>),
    notif_block_test(TestBlocking),
    ok.

notif_block_test(false) ->
    ok;
%% don't know how much we need to reliably cause the socket to "fill up"
%% - just loop forever generating start/stop usess for manual test
notif_block_test(true) ->
    ?line {ok, MaapiSock} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    ?line ok = econfd_maapi:start_user_session(MaapiSock,
                                               <<"admin">>, <<"maapi">>,
                                               [<<"admin">>],
                                               {127,0,0,1}, ?CONFD_PROTO_TCP),
    ?line ok = econfd_maapi:close(MaapiSock),
    notif_block_test(true).

notif2(Top) ->
    ?line {ok, MaapiSock} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    ?line ok = econfd_maapi:start_user_session(MaapiSock,
                                               <<"admin">>, <<"maapi">>,
                                               [<<"admin">>],
                                               {127,0,0,1}, ?CONFD_PROTO_TCP),
    %% we keep this maapi socket and reuse it over and over

    {ok, Fd} = file:open("notif.out", [read, write]),
    {ok, NotifSock} = econfd_notif:connect({127,0,0,1},
                                           ?CONFD_NOTIF_AUDIT bor
                                           ?CONFD_NOTIF_SYSLOG bor
                                           ?CONFD_NOTIF_COMMIT_SIMPLE bor
                                           ?CONFD_NOTIF_USER_SESSION bor
                                           ?CONFD_NOTIF_COMMIT_DIFF  bor
                                           ?CONFD_NOTIF_HA_INFO bor
                                           ?CONFD_NOTIF_CONFIRMED_COMMIT bor
                                           ?CONFD_NOTIF_UPGRADE_EVENT),
    Top ! {self(), ok},
    notif(Fd, NotifSock, MaapiSock).



notif(Fd, NotifSock, MaapiSock) ->
    case econfd_notif:recv(NotifSock) of
        {ok, Msg} ->
            io:format(Fd, "NOTIF decoded ~p~n~n", [Msg]),
            handle_msg(Fd, NotifSock, MaapiSock, Msg),
            notif(Fd, NotifSock, MaapiSock);
        {error, closed} ->
            io:format("GOT close on notif socket\n",[]),
            ok
    end.


%% audit messages
handle_msg(Fd, _NotifSock, _MaapiSock,
           #econfd_notif_audit{logno = LogNo, user = User,
                               usid = Usid, msg = Msg}) ->
    io:format(Fd,
              "NOTIF: Audit message: ~p/~s user=~p/~p msg=~p~n",
              [LogNo,
               econfd_logsyms:get_logsymstr(LogNo),
               binary_to_list(User), Usid,
               binary_to_list(Msg)]);


%% syslog message
handle_msg(Fd, _NotifSock, _MaapiSock,
           #econfd_notif_syslog{logno = LogNo, prio = Severity,
                                msg = Msg}) ->
    io:format(Fd,
              "NOTIF: Syslog message: ~p/~s severity=~p Msg=~p~n",
              [LogNo,
               econfd_logsyms:get_logsymstr(LogNo),
               Severity,
               binary_to_list(Msg)]);

%% simple commit messages
handle_msg(Fd, _NotifSock, _MaapiSock,
           #econfd_notif_commit_simple{db = Db, user = User,
                                       ip = Ip} ) ->
    io:format(Fd,
              "NOTIF: Commit simple for db ~p from user ~p ip=~p~n",
              [Db, binary_to_list(User), Ip]);

%% full diff commit messages
handle_msg(Fd, NotifSock, MaapiSock,
           #econfd_notif_commit_diff{db = Db, usid=Usid,
                                     comment=Comment, label=Label,
                                     user = User, ip=Ip,th=TH})->
    io:format(Fd,
              "NOTIF: Commit diff for db ~p from user ~p "
              "ip=~p usid=~p th=~p comment=~p label=~p~n",
              [Db, binary_to_list(User), Ip, Usid, TH, Comment, Label]),
    _Sessions = econfd_maapi:get_user_sessions(MaapiSock),
    io:format(Fd, "Sessions = ~p~n", [_Sessions]),
    ?line ok = econfd_maapi:attach2(MaapiSock, -1, Usid, TH),
    Iter = fun(IKP, Op, _OldValue, Value, State) ->
                   io:format(Fd, "ITER: IKP = ~p Op=~p, value=~p~n",
                             [IKP, Op, Value]),
                   {ok, ?ITER_RECURSE, State}
           end,
    ?line {ok, foobar} = econfd_maapi:diff_iterate(MaapiSock,TH, Iter, foobar),
    econfd_notif:notification_done(NotifSock, TH),
    %% no need to detach - will be done automatically when the transaction
    %% is finished
    io:format(Fd, "*********************************** \n",[]);

%% user session
handle_msg(Fd, _NotifSock, _MaapiSock,
           #econfd_notif_user_session{user = User, ip=Ip, context=Ctx}) ->
    io:format(Fd, "NOTIF: User session: User = ~p Ip=~p, Ctx=~p~n",
              [binary_to_list(User), Ip, Ctx]),
    if User == <<"stop">> ->
            file:close(Fd),
            throw(done);
       true ->
            ok
    end;

%% confirmed commit
handle_msg(Fd, _NotifSock, _MaapiSock,
           #econfd_notif_confirmed_commit{type = Type, timeout = Timeout,
                                          uinfo = Uinfo }) ->
           #confd_user_info{username = User, ip=Ip, context=Ctx} = Uinfo,
    io:format(Fd, "NOTIF: Confirmed commit: Type = ~p Timeout = ~p "
              "User = ~p Ip=~p, Ctx=~p~n",
              [Type, Timeout, binary_to_list(User), Ip, Ctx]);

%% in-service upgrade
handle_msg(Fd, _NotifSock, _MaapiSock,
           #econfd_notif_upgrade{event = Event}) ->
    io:format(Fd, "NOTIF: Upgrade: Event = ~p~n", [Event]);

handle_msg(_,_,_,Other) ->
    io:format("Bad message ~p~n", [Other]),
    exit(err).




%% First use maapi to populate types.cs and then use cdb to
%%% read the values and check that they are the same
types() ->
    ?line {ok, M} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    NS = 'http://tail-f.com/ns/example/types',
    ?line ok = econfd_maapi:start_user_session(M, <<"admin">>, <<"maapi">>,
                                               [<<"admin">>],
                                               {127,0,0,1}, ?CONFD_PROTO_TCP),

    %% FIXME set to ?CONFD_READ and check error messages
    ?line {ok,TH} = econfd_maapi:start_trans(M, ?CONFD_RUNNING,
                                             ?CONFD_READ_WRITE),
    ?line ok = econfd_maapi:create(M, TH, [x, [NS|root]]),
    ?line R = econfd_maapi:set_elem(
                M, TH, [baz,bar,foo,x, [NS|root]],
                ?CONFD_LIST([?CONFD_UINT32(77), ?CONFD_UINT32(5)])),
    io:format("R = ~p~n", [R]),
    ?line ok = R,
    ?line ok = econfd_maapi:set_elem(
                 M, TH, [strlst, x, [NS|root]],
                 ?CONFD_LIST([<<"aaa">>, <<"bababa">>])),

    ?line ok = econfd_maapi:set_elem(
                 M, TH, [c_int8, x, [NS|root]],
                 ?CONFD_INT8(-126)),

    ?line ok = econfd_maapi:set_elem(
                 M, TH, [c_int16, x, [NS|root]],
                 ?CONFD_INT16(-3126)),

    ?line ok = econfd_maapi:set_elem(
                 M, TH, [c_int32, x, [NS|root]],
                 ?CONFD_INT32(-16626)),

    ?line ok = econfd_maapi:set_elem(
                 M, TH, [c_int64, x, [NS|root]],
                 ?CONFD_INT64(-126999999999999)),



    ?line ok = econfd_maapi:set_elem(
                 M, TH, [c_uint8, x, [NS|root]],
                 ?CONFD_UINT8(126)),

    ?line ok = econfd_maapi:set_elem(
                 M, TH, [c_uint16, x, [NS|root]],
                 ?CONFD_UINT16(3126)),

    ?line ok = econfd_maapi:set_elem(
                 M, TH, [c_uint32, x, [NS|root]],
                 ?CONFD_UINT32(16626)),

    ?line ok = econfd_maapi:set_elem(
                 M, TH, [c_uint64, x, [NS|root]],
                 ?CONFD_UINT64(126999999999999)),


    ?line ok = econfd_maapi:set_elem(
                 M, TH, [b, x, [NS|root]],
                 ?CONFD_BOOL(true)),


    ?line ok = econfd_maapi:set_elem(
                 M, TH, [f, x, [NS|root]],
                 ?CONFD_DOUBLE(3.14)),


    Y = 1960,
    Month = 3,
    D = 31,
    H = 23,
    Min = 23,
    S = 7,
    Mcr = 0,
    TZ = [],
    TZM = 0,

    ?line ok = econfd_maapi:set_elem(
                 M, TH, [datetime, x, [NS|root]],
                 ?CONFD_DATETIME({Y,Month,D,H,Min,S,Mcr,TZ,TZM})),


    ?line ok = econfd_maapi:set_elem(
                 M, TH, [date, x, [NS|root]],
                 ?CONFD_DATE({Y,Month,D,TZ,TZM})),

    ?line ok = econfd_maapi:set_elem(
                 M, TH, [time, x, [NS|root]],
                 ?CONFD_TIME({H,Min,S,Mcr,TZ,TZM})),

    ?line ok =  econfd_maapi:set_elem(
                  M, TH, [duration, x, [NS|root]],
                  ?CONFD_DURATION({Y,Month,D,H,Min,S,Mcr})),

    ?line ok =  econfd_maapi:set_elem(
                  M, TH, [b32, x, [NS|root]],
                  ?CONFD_BIT32(2#11)),

    ?line ok =  econfd_maapi:set_elem(
                  M, TH, [b64, x, [NS|root]],
                  ?CONFD_BIT64(2#10000001)),

    ?line ok =  econfd_maapi:set_elem(
                  M, TH, [bbig, x, [NS|root]],
                  ?CONFD_BITBIG(
                     econfd:bitbig_set_bit(<<0:(?types__size_myFlagsBig * 8)>>,
                                           ?types__pos_myFlagsBig_makeitbig))),


    ?line ok =  econfd_maapi:set_elem(
                  M, TH, [c_ipv4, x, [NS|root]],
                  ?CONFD_IPV4({127,0,0,1})),


    ?line ok =  econfd_maapi:set_elem(
                  M, TH, [c_ipv6, x, [NS|root]],
                  ?CONFD_IPV6({77,0,0,1,7,8,9,4})),


    ?line ok =  econfd_maapi:set_elem(
                  M, TH, [c_dns, x, [NS|root]],
                  ?CONFD_BUF(<<"www.dn.se">>)),


    ?line ok =  econfd_maapi:set_elem2(
                  M, TH, [oref, x, [NS|root]],
                  <<"/root/x/oref">>),



    ?line ok = econfd_maapi:apply_trans(M, TH, false),
    ?line ok = econfd_maapi:finish_trans(M, TH),

    ok.

%% Test read-write data provider via netconf
%% - in particular error reporting
dp() ->
    %% funny usernames trigger errors, see simple.erl
    CmdList = [ {"admin", "set"},
                {"errstr_data", "del"},
                {"cs_str_data", "del"},
                {"admin", "del"},
                {"errstr_trans_lock", "set"},
                {"cs_apptag_write_start", "set"},
                {"already_locked_trans_lock", "set"},
                {"cs_in_use_trans_lock", "set"},
                {"cs_in_use_str_trans_lock", "set"},
                {"in_use_write_start", "set"},
                {"cs_in_use_write_start", "set"},
                {"cs_in_use_str_write_start", "set"},
                {"in_use_prepare", "set"},      % write succeeds...
                {"cs_in_use_prepare", "del"},    % write succeeds...
                {"errstr_lock_candidate", "lock"}, % ok since we lock running
                {"errstr_lock_running", "lock"},
                {"errstr_unlock_running", "lock"},
                {"admin", "confirm"},
                {"errstr_add_checkpoint_running", "confirm"},
                {"errstr_del_checkpoint_running", "confirm"}
               ],
    Out = "netconf-dp.out",
    file:delete(Out),
    [ os:cmd("netconf-console-tcp -s all -u "++User++" cmd-"++Cmd++".xml >> "++
                 Out) ||
        {User, Cmd} <- CmdList ],
    ?line [] = os:cmd("diff netconf-dp.expect " ++ Out).

%% Test using schema info
schema(IsInternal) ->
    %% basic
    ?line NS1 = 'http://tail-f.com/test/ytest',
    ?line ok = econfd_schema:load({127,0,0,1}, ?CONFD_PORT),
    ?line NsL0 = econfd_schema:get_nslist(),
    ?line true = (length(NsL0) > 9),
    ?line NsL = [NsI || NsI <- NsL0, lists:member(NsI#confd_nsinfo.prefix,
                                                  ["aaa", "ytest", "types",
                                                   "smp", "simple", "procs",
                                                   "cdbwrite", "action"])],
    ?line 8 = length(NsL),
    ?line #confd_nsinfo{namespace = NS1,
                        prefix = "ytest",
                        revision = <<"2009-12-01">>} =
        lists:keyfind("ytest", #confd_nsinfo.prefix, NsL),

    %% get #confd_cs_node{}, str <-> val
    ?line Cs = econfd_schema:get_cs(NS1, [key, entry, obu]),
    ?line #confd_cs_node{primitive_type = ?C_INT32} = Cs,
    ?line Cs = econfd_schema:ikeypath2cs([key, {42}, entry, [NS1|obu]]),
    ?line
    ?line {ok, 42} = econfd_schema:str2val(Cs, <<"42">>),
    ?line {ok, <<"42">>} = econfd_schema:val2str(Cs, 42),

    %% navigate
    ?line CsP = econfd_schema:get_cs(Cs#confd_cs_node.namespace,
                                      tl(Cs#confd_cs_node.tagpath)),
    ?line #confd_cs_node{keys = [key], children = [key, value],
                         flags = Flags} = CsP,
    ?line [key, value] = econfd_schema:choice_children(CsP),
    ?line Exp = (?CONFD_CS_IS_LIST bor ?CONFD_CS_IS_WRITE bor ?CONFD_CS_IS_CDB),
    ?line Exp = (Flags band Exp),

    %% enum str <-> val
    ?line NS2 = 'http://tail-f.com/ns/example/smp',
    ?line #confd_cs_node{} = Cs2 =
        econfd_schema:get_cs(NS2, [e, server, servers]),
    ?line Defval = <<"complete">>,
    ?line {ok, Defval} = econfd_schema:val2str(Cs2, Cs2#confd_cs_node.default),
    ?line {ok, ?CONFD_ENUM_VALUE(E1)} = econfd_schema:str2val(Cs2, Defval),
    ?line ?CONFD_ENUM_VALUE(E1) = Cs2#confd_cs_node.default,

    %% identityref str <-> val
    ?line #confd_cs_node{primitive_type = ?C_IDENTITYREF} = Cs3 =
        econfd_schema:get_cs(?types__ns_uri, [idref, x, root]),
    ?line {ok, ?CONFD_IDENTITYREF({?types__ns, ?types_aes}) = Vaes} =
        econfd_schema:str2val(Cs3, "aes"),
    ?line {ok, <<"types:aes">> = Saes} = econfd_schema:val2str(Cs3, Vaes),
    ?line {ok, Vaes} = econfd_schema:str2val(Cs3, Saes),
    ?line {ok, ?CONFD_IDENTITYREF({?types__ns, ?types_des})} =
        econfd_schema:str2val(Cs3, "types:des"),

    %% get_type(), str <-> val
    ?line Type1 = econfd_schema:get_type(int8),
    ?line {ok, ?CONFD_INT8(42)} = econfd_schema:str2val(Type1, <<"42">>),
    ?line {ok, <<"42">>} = econfd_schema:val2str(Type1, ?CONFD_INT8(42)),
    ?line Type2 = econfd_schema:get_type(NS2, 'enum-type'),
    ?line EVal = <<"incomplete">>,
    ?line {ok, ?CONFD_ENUM_VALUE(E2)} = econfd_schema:str2val(Type2, EVal),
    ?line false = E1 == E2,
    ?line {ok, EVal} = econfd_schema:val2str(Type2, ?CONFD_ENUM_VALUE(E2)),

    %% basic "get" error tests
    ?line not_found = econfd_schema:get_cs(NS1, [key, obu]),
    ?line not_found = econfd_schema:get_cs(nosuchns, [key, entry, obu]),
    ?line not_found = econfd_schema:ikeypath2cs([key, {42}, entry, [NS1|o]]),
    ?line not_found = NoType1 = econfd_schema:get_type(nosuchtype),

    %% str <-> val error tests
    ?line {error, {?CONFD_ERR_BADTYPE, <<"Invalid string", _/binary>>}} =
        econfd_schema:str2val(Cs, <<"abc">>),
    ?line {error, {?CONFD_ERR_BADTYPE, <<"Invalid value", _/binary>>}} =
        econfd_schema:val2str(Cs, ?CONFD_UINT32(42)),
    ?line {error, {?CONFD_ERR_BADTYPE, <<"Invalid type", _/binary>>}} =
        econfd_schema:str2val(NoType1, <<"42">>),
    ?line {error, {?CONFD_ERR_BADTYPE, <<"Invalid type", _/binary>>}} =
        econfd_schema:val2str(NoType1, ?CONFD_UINT32(42)),
    ?line NoType2 = econfd_schema:get_type(nosuchns, 'enum-type'),
    ?line {error, {?CONFD_ERR_BADTYPE, <<"Invalid type", _/binary>>}} =
        econfd_schema:str2val(NoType2, <<"complete">>),
    ?line {error, {?CONFD_ERR_BADTYPE, <<"Invalid type", _/binary>>}} =
        econfd_schema:val2str(NoType2, ?CONFD_ENUM_VALUE(E1)),
    ?line NoType3 = econfd_schema:get_type(NS2, nosuchtype),
    ?line {error, {?CONFD_ERR_BADTYPE, <<"Invalid type", _/binary>>}} =
        econfd_schema:str2val(NoType3, <<"complete">>),
    ?line {error, {?CONFD_ERR_BADTYPE, <<"Invalid type", _/binary>>}} =
        econfd_schema:val2str(NoType3, ?CONFD_ENUM_VALUE(E1)),
    ?line {error, {?CONFD_ERR_BADTYPE, <<"Invalid string", _/binary>>}} =
        econfd_schema:str2val(Cs3, <<"des">>),
    ?line {error, {?CONFD_ERR_BADTYPE, <<"Invalid value", _/binary>>}} =
        econfd_schema:val2str(Cs3, ?CONFD_IDENTITYREF({17, 42})),

    %% get meta data
    ?line NS3 = 'http://example.com/meta-data',
    ?line #confd_cs_node{} = Cs4 =
        econfd_schema:get_cs(NS3, [meta, 'meta-data']),
    ?line MetaData = Cs4#confd_cs_node.meta_data,
    ?line true = lists:member({<<"foo">>, undefined}, MetaData),
    ?line true = lists:member({<<"bar">>, <<"something">>}, MetaData),
    ?line true = lists:member({<<"baz">>, <<>>}, MetaData),

    %% user-defined type
    if IsInternal ->
            %% ec_user_type is loaded by ConfD for internal test
            ok;
       true ->
            code:ensure_loaded(ec_user_type)
    end,
    ?line PortIKP = [port, x, [?types__ns_uri|root]],
    ?line #confd_cs_node{} = PortCs = econfd_schema:ikeypath2cs(PortIKP),
    ?line GoodString = <<"1 / 2/3">>,
    ?line {ok, ?CONFD_INT32(66051) = GoodValue} =
        econfd_schema:str2val(PortCs, GoodString),
    ?line {ok, <<"1/2/3">>} = econfd_schema:val2str(PortCs, GoodValue),
    ?line BadString = <<"1/2">>,
    ?line {error, {?CONFD_ERR_BADTYPE, <<"Wrong number of elements">>}} =
        econfd_schema:str2val(PortCs, BadString),
    ?line BadValue = ?CONFD_INT64(1),
    ?line {error, {?CONFD_ERR_BADTYPE, <<"Invalid value", _/binary>>}} =
        econfd_schema:val2str(PortCs, BadValue),
    if IsInternal ->
            %% Test ConfD's internal invocation of the callbacks
            ?line {ok, M} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
            ?line ok = econfd_maapi:start_user_session(M, <<"admin">>,
                                                       <<"maapi">>,
                                                       [<<"admin">>],
                                                       {127,0,0,1},
                                                       ?CONFD_PROTO_TCP),
            ?line {ok,TH} = econfd_maapi:start_trans(M, ?CONFD_RUNNING,
                                                     ?CONFD_READ_WRITE),
            ?line ok = econfd_maapi:set_elem2(M, TH, PortIKP, GoodString),
            ?line ok = econfd_maapi:set_elem(M, TH, PortIKP, GoodValue),
            ?line {error,
                   {?CONFD_ERR_BADTYPE, <<"Wrong number of elements">>}} =
                econfd_maapi:set_elem2(M, TH, PortIKP, BadString),
            ?line {error, {?CONFD_ERR_BADTYPE, _}} =
                econfd_maapi:set_elem(M, TH, PortIKP, BadValue),
            ?line ok = econfd_maapi:close(M),
            ok;
       true ->
            ok
    end.


xpath() ->
    ?line {ok, M} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    ?line ok =
        econfd_maapi:start_user_session(M, <<"admin">>, <<"maapi">>,
                                        [<<"admin">>],
                                        {127,0,0,1}, ?CONFD_PROTO_TCP),
    ?line {ok, Th} =
        econfd_maapi:start_trans(M, ?CONFD_RUNNING, ?CONFD_READ),

    X = fun (Str, Vs) ->
                econfd_maapi:xpath_eval_expr(M, Th, Str, [{varbindings, Vs}])
        end,

    ?line {ok, <<"2">>} = X(<<"1 + 1">>, []),
    ?line {ok, <<"3">>} = X(<<"1 + $a">>, [{"a", <<"2">>}]),
    ?line {ok, <<"3">>} = X(<<"1 + $a">>, [{"a", <<"1 + 1">>}]),
    ?line {ok, <<"44">>} = X(<<"/obu/entry[3]/value">>, []),
    ?line {ok, <<"4">>} = X(<<"count($foo)">>, [{"foo", "/obu/entry"}]),
    ?line {ok, <<"3">>} = X(<<"count(/obu/entry[value > 20])">>, []),
    ?line {ok, <<"3">>} = X(<<"count($foo[value > 20])">>,
                            [{"foo", "/obu/entry"}]),
    ?line {ok, <<"hello world">>} = X("concat($h, ' ', $w)",
                                      [{"h", "'hello'"},
                                       {"w", <<"'world'">>}]),
    ?line {ok, <<"hello">>} = X("$h", [{"h", <<"\"hello\"">>}]),

    econfd_maapi:close(M),
    ok.

log(true) ->
    Log = "devel.log",
    ?line ok = econfd:log(?CONFD_LEVEL_INFO,
                          "Internal ECONFD_INFO"),
    ?line ok = econfd_log_check(Log, "Internal ECONFD_INFO"),

    ?line ok = econfd:log(?CONFD_LEVEL_ERROR,
                          "Internal ECONFD_~s", ["ERROR"]),
    ?line ok = econfd_log_check(Log, "Internal ECONFD_ERROR"),

    ?line ok = econfd:log(ignored, ?CONFD_LEVEL_TRACE,
                          "Internal ECONFD_~s", ["TRACE"]),
    ?line ok = econfd_log_check(Log, "Internal ECONFD_TRACE"),
    ok;
log(false) ->
    ?line Log = "external_econfd.log",
    ?line file:delete(Log),
    ?line {ok, IoDevice} = file:open(Log, [write]),
    ?line ok = econfd:log(?CONFD_LEVEL_INFO, "External STDOUT"),
    ?line ok = econfd:log(?CONFD_LEVEL_INFO, "External STDOUT ~s",["+Args"]),

    ?line ok = econfd:log(IoDevice, ?CONFD_LEVEL_INFO,
                          "External ECONFD_~s",["INFO"]),
    ?line ok = econfd_log_check(Log, "External ECONFD_INFO"),

    ?line ok = econfd:log(IoDevice, ?CONFD_LEVEL_ERROR,
                          "External ECONFD_~s", ["ERROR"]),
    ?line ok = econfd_log_check(Log, "External ECONFD_ERROR"),

    ?line ok = econfd:log(IoDevice, ?CONFD_LEVEL_TRACE,
                          "External ECONFD_~s", ["TRACE"]),
    ?line ok = econfd_log_check(Log, "External ECONFD_TRACE"),
    ok.

econfd_log_check(Log, String) ->
    {ok, B} = file:read_file(Log),
    case string:str(binary_to_list(B), String) of
        0 ->
            string_not_found;
        _Pos ->
            ok
    end.

%% instance-identifiers as keys
iid_keys() ->
    NS = ?iid__ns_uri,
    EKP = [iid,[NS|ext]],
    CKP = [iid,[NS|cdb]],
    ?line {ok, M} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    ?line ok = econfd_maapi:start_user_session(M, <<"admin">>, <<"maapi">>,
                                               [<<"admin">>],
                                               {127,0,0,1}, ?CONFD_PROTO_TCP),
    ?line {ok, Th} = econfd_maapi:start_trans(M, ?CONFD_RUNNING,
                                              ?CONFD_READ_WRITE),

    ?line C = econfd_maapi:init_cursor(M, Th, EKP),
    ?line 3 = iid_loop1(M, Th, EKP, CKP, econfd_maapi:get_next(C), 0),
    ?line {ok, {_, 3, 9}} = econfd_maapi:diff_iterate(M, Th, fun iid_iter/5,
                                                      {CKP, 0, 0}),
    ?line {ok, Sub} = proc_lib:start_link(?MODULE, iid_cdbsub, [self(), CKP]),
    ?line ok = econfd_maapi:apply_trans(M, Th, false),
    ?line ok = econfd_maapi:close(M),
    ?line {Sub, {ok, {_, 3, 9}}} = receive X -> X after 20000 -> timeout end,
    ?line {ok, S} = econfd_cdb:connect({127,0,0,1}),
    ?line {ok, CDB} = econfd_cdb:new_session(S, ?CDB_RUNNING),
    ?line {ok, 3} = {ok, Numi} = econfd_cdb:num_instances(CDB, CKP),
    ?line ok = iid_loop2(CDB, CKP, 0, Numi),
    ?line ok = econfd_cdb:close(S),
    ok.

%% get_next walk towards data provider, verifying keys
%% (see iid.erl) and copying to CDB
iid_loop1(_M, _Th, _EKP, _CKP, done, N) ->
    N;
iid_loop1(M, Th, EKP, CKP, {ok, {Str, IId} = Keys, C}, N) ->
    ?line IId = iid:objref(Str),
    ?line ?CONFD_OBJECTREF(IIdPath) = IId,
    ?line false = lists:any(fun (E) -> is_integer(E) end, IIdPath),
    ?line {ok, IId} = econfd_maapi:get_elem(M, Th, [obj, Keys | EKP]),
    ?line {ok, Val} = econfd_maapi:get_elem(M, Th, [value, Keys | EKP]),
    ?line ok = econfd_maapi:create(M, Th, [Keys | CKP]),
    ?line ok = econfd_maapi:set_elem(M, Th, [value, Keys | CKP], Val),
    iid_loop1(M, Th, EKP, CKP, econfd_maapi:get_next(C), N+1).

%% verify CDB changes
iid_iter([{Str, IId} | CKP], ?MOP_CREATED, _OldValue, _Value,
         {CKP, Create, Set}) ->
    ?line IId = iid:objref(Str),
    {ok, ?ITER_RECURSE, {CKP, Create + 1, Set}};
iid_iter([name, {Str, IId} | CKP], ?MOP_VALUE_SET, _OldValue, Str,
         {CKP, Create, Set}) ->
    ?line IId = iid:objref(Str),
    {ok, ?ITER_RECURSE, {CKP, Create, Set + 1}};
iid_iter([obj, {Str, IId} | CKP], ?MOP_VALUE_SET, _OldValue, IId,
         {CKP, Create, Set}) ->
    ?line IId = iid:objref(Str),
    {ok, ?ITER_RECURSE, {CKP, Create, Set + 1}};
iid_iter([value, {Str, IId} | CKP], ?MOP_VALUE_SET, _OldValue, _Value,
         {CKP, Create, Set}) ->
    ?line IId = iid:objref(Str),
    {ok, ?ITER_RECURSE, {CKP, Create, Set + 1}}.

%% CDB subscriber w diff_iterate
iid_cdbsub(Parent, CKP) ->
    ?line {ok, S} = econfd_cdb:connect({127,0,0,1}),
    ?line {ok, CDB} = econfd_cdb:subscribe_session(S),
    ?line {ok, Point} = econfd_cdb:subscribe(CDB, 1, 0, "/cdb/iid"),
    ?line ok = econfd_cdb:subscribe_done(CDB),
    proc_lib:init_ack(Parent, {ok, self()}),
    F = fun([P]) when P == Point ->
                ?line Res = econfd_cdb:diff_iterate(CDB, Point, fun iid_iter/5,
                                                    0, {CKP, 0, 0}),
                ?line Parent ! {self(), Res},
                close
        end,
    ?line ok = econfd_cdb:wait(CDB, 20000, F).

%% list walk towards CDB, verifying keys
iid_loop2(_CDB, _CKP, N, N) ->
    ok;
iid_loop2(CDB, CKP, Ix, N) ->
    ?line {ok, Str} = econfd_cdb:get_elem(CDB, [name, [Ix] | CKP]),
    ?line {ok, IId} = econfd_cdb:get_elem(CDB, [obj, [Ix] | CKP]),
    ?line IId = iid:objref(Str),
    ?line ?CONFD_OBJECTREF(IIdPath) = IId,
    ?line false = lists:any(fun (E) -> is_integer(E) end, IIdPath),
    ?line {ok, IId} = econfd_cdb:get_elem(CDB, [obj, {Str, IId} | CKP]),
    iid_loop2(CDB, CKP, Ix+1, N).


%% handle iterate-fun crash in a good way
%% - i.e. sync the socket and deliver the original exception
iterate_crash() ->
    NS = ?smp__ns_uri,
    T = [server, [NS|servers]],
    ?line {ok, M} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    ?line ok = econfd_maapi:start_user_session(M, <<"admin">>, <<"maapi">>,
                                               [<<"admin">>],
                                               {127,0,0,1}, ?CONFD_PROTO_TCP),
    ?line {ok, Th} = econfd_maapi:start_trans(M, ?CONFD_RUNNING,
                                              ?CONFD_READ_WRITE),
    ?line ok = econfd_maapi:create(M, Th, [{?CONFD_BUF(<<"another">>)}|T]),
    IterF = fun iter_fun/5,
    %% trigger crash in fun, verify exception
    ?line ok = try
                   econfd_maapi:diff_iterate(M, Th, IterF, 0, undefined)
               catch
                   error:badarith ->
                       case
                           lists:keyfind(iter_fun, 2, erlang:get_stacktrace())
                       of
                           false ->
                               bad1;
                           _ ->
                               ok
                       end;
                   Class:Reason ->
                       io:format("~p:~p~n~p~n",
                                 [Class, Reason, erlang:get_stacktrace()]),
                       bad2
               end,
    %% verify that socket is OK
    %% 3 b/c we get 2 value_set for leafs in addition to the create
    ?line {ok, 3} = try
                        econfd_maapi:diff_iterate(M, Th, IterF, 0, 0)
                    catch
                        _:_ ->
                            bad
                    end,
    ?line ok = econfd_maapi:close(M).

iter_fun(_IKP, _Op, _Oval, _Val, State) ->
    {ok, ?ITER_RECURSE, State + 1}.


%% Check that confd stops when started via open_port/2
%% if the port is immediately closed (RT 536)
port_test() ->
    try
        Confd = os:getenv("CONFD"),
        ?line false = (Confd == false),
        Args0 = " -c confd.conf --foreground --stop-on-eof --start-phase0 ",
        Flags = os:getenv("CONFD_FLAGS"),
        Args = Args0 ++ Flags,
        Log = "confd.log",
        Stamp = "stopped.log",
        file:delete(Log),
        file:delete(Stamp),
        Cmd = "sh -c '" ++  Confd ++ Args ++ "; touch " ++ Stamp ++ "'",
        spawn(fun() ->
                      open_port({spawn, Cmd}, [eof, exit_status])
              end),
        ?line ok = ensure_started_and_stopped(log, Confd, Log, Stamp, 60),
        ehalt(true, 0)
    catch
        _:Err ->
            erep(Err),
            ehalt(true, 1)
    end.

ensure_started_and_stopped(Waitfor, Confd, _Log, _Stamp, 0) ->
    os:cmd(Confd ++ " --stop"),
    "*** Failed to stop confd via port close (" ++ atom_to_list(Waitfor) ++ ")";
ensure_started_and_stopped(log, Confd, Log, Stamp, N) ->
    ?line case file:read_file(Log) of
        {error, enoent} ->
            %% Log not created yet
            timer:sleep(1000),
            ensure_started_and_stopped(log, Confd, Log, Stamp, N - 1);
        {ok, _B} ->
            ensure_started_and_stopped(eof, Confd, Log, Stamp, N)
    end;
ensure_started_and_stopped(eof, Confd, Log, Stamp, N) ->
    ?line {ok, B} = file:read_file(Log),
    case string:str(binary_to_list(B),"end-of-file") of
        0 ->
            %% "end-of-file" not present in Log yet
            timer:sleep(1000),
            ensure_started_and_stopped(eof, Confd, Log, Stamp, N - 1);
        _Pos ->
            ensure_started_and_stopped(stop, Confd, Log, Stamp, N)
    end;
ensure_started_and_stopped(stop, Confd, Log, Stamp, N) ->
    ?line case file:read_file(Stamp) of
        {error, enoent} ->
            %% ConfD not stopped yet
            timer:sleep(1000),
            ensure_started_and_stopped(stop, Confd, Log, Stamp, N - 1);
        {ok, _B} ->
            ok
    end.
