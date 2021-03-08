-module(notif).
-compile(export_all).

-include("../../include/econfd.hrl").


start() ->
    application:start(econfd),
    timer:sleep(1000),
    proc_lib:spawn(fun go/0).


go() ->
    process_flag(trap_exit, true),
    NCbs = #confd_notification_stream_cbs{
      streamname = linkUp,
      get_log_times = fun get_log_times/1,
      replay = fun replay/3
     },

    {ok,Daemon} = econfd:init_daemon(enotifier, ?CONFD_TRACE, user, none,
                                     {127,0,0,1}, ?CONFD_PORT),
    {ok, Nctx} = econfd:register_notification_stream(Daemon, NCbs),
    ok = econfd:register_done(Daemon),
    loop(Nctx, Daemon, 1).

-define(NS, 'http://tail-f.com/ns/example/notif').

loop(Nctx, Daemon, Counter) ->
    receive
        {'EXIT', Daemon, _} ->
            io:format("Daemon died \n",[]),
            erlang:halt(0)
    after 0 ->
            ok
    end,
    timer:sleep(1000),

    Exml = notif1(Counter),
    case econfd:notification_send(Nctx, atime(), Exml) of
        ok -> ok;
        Error -> io:format("Failed to send notification: ~p~n", [Error])
    end,
    loop(Nctx, Daemon, Counter+1).


notif1(N) when N rem 2 == 0 ->
    [{[?NS|linkDown],start},
     {[?NS|ifIndex],?CONFD_UINT32(N div 2)},
     {[?NS|linkDown],stop}];
notif1(N) ->
    Ix = N div 2,
    [{[?NS|linkUp],start},
     {[?NS|ifIndex],?CONFD_UINT32(Ix)},
     if N rem 4 == 1 ->
             KP = [ifIndex,{?CONFD_UINT32(Ix)},interface,[?NS|interfaces]],
             {[?NS|obj], ?CONFD_OBJECTREF(KP)};
        true ->
             KP1 = [ifIndex,{?CONFD_UINT32(Ix)},interface,[?NS|interfaces]],
             KP2 = [desc,{?CONFD_UINT32(Ix+1)},interface,[?NS|interfaces]],
             {[?NS|refs], [?CONFD_OBJECTREF(KP1),?CONFD_OBJECTREF(KP2)]}
     end,
     {[?NS|linkProperty],start},
     {[?NS|flags],{12,42}},
     {[?NS|linkProperty],stop},
     {[?NS|linkProperty],start},
     {[?NS|flags],{12,17}},
     {[?NS|linkProperty],stop},
     {[?NS|linkUp],stop}].

notif2(N) when N rem 2 == 0 ->
    [{[?NS|linkDown],start},
     {[?NS|ifIndex],?CONFD_UINT32(N div 2)},
     {[?NS|linkDown],stop}];
notif2(N) ->
    [{[?NS|linkUp],start},
     {[?NS|ifIndex],?CONFD_UINT32(N div 2)},
     {[?NS|linkProperty],start},
     {[?NS|newlyAdded],leaf},
     {[?NS|flags],{12,11}},
     {[?NS|extensions],start},
     {[?NS|name],{12,1}},
     {[?NS|value],{12,22}},
     {[?NS|extensions],stop},
     {[?NS|extensions],start},
     {[?NS|name],{12,2}},
     {[?NS|value],{12,33}},
     {[?NS|extensions],stop},
     {[?NS|linkProperty],stop},
     {[?NS|linkUp],stop}].

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

btime() ->
    Y = 2007,
    Month = 3,
    D = 30,
    H = 23,
    Min = 23,
    S = 7,
    Mcr = 0,
    TZ = [],
    TZM = 0,
    {?C_DATETIME, {Y,Month,D,H,Min,S,Mcr,TZ,TZM}}.

get_log_times(_Nctx) ->
    {ok, {atime(), btime()}}.


replay(Nctx, _From, _To) ->
    io:format("Invoked in replay \n",[]),
    spawn(fun() ->
                  Exml = notif1(4),
                  ok = econfd:notification_send(Nctx, atime(), Exml),
                  ok = econfd:notification_replay_complete(Nctx)
          end),
    ok.

