-module(subscriber).
-export([start/1]).

-include("../../include/econfd.hrl").

start([Type, Filter, MaxEventsAtom]) ->
    Opts = [{stream_name, linkUp}] ++
        if Type == replay ->
                [{start_time, ?CONFD_DATETIME({2007,07,28,15,23,36,0,0,0})}];
           true ->
                []
        end ++
        if Filter ->
                [{xpath_filter, <<"/linkUp/linkProperty">>}];
           true ->
                []
        end,
    MaxEvents = (catch list_to_integer(atom_to_list(MaxEventsAtom))),
    {ok, Sock} = econfd_notif:connect({127,0,0,1}, ?CONFD_PORT,
                                      ?CONFD_NOTIF_STREAM_EVENT, Opts),
    io:format("Waiting for events...~n",[]),
    loop(Sock, MaxEvents, 0),
    erlang:halt(0).

loop(_Sock, MaxEvents, MaxEvents) ->
    ok;
loop(Sock, MaxEvents, Events) ->
    {ok, Event} = econfd_notif:recv(Sock),
    case Event#econfd_notif_stream_event.type of
        notification_event ->
            ?CONFD_DATETIME({Y,M,D,Hr,Min,Sec,Micro,_TZ,_TZM}) =
                Event#econfd_notif_stream_event.event_time,
            io:format("~nNotification event at "
                      "~w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~6..0w:~n",
                      [Y,M,D,Hr,Min,Sec,Micro]),
            io:format("~p~n", [Event#econfd_notif_stream_event.values]),
            loop(Sock, MaxEvents, Events + 1);
        replay_failed ->
            io:format("~nReplay failed: ~s~n",
                      [Event#econfd_notif_stream_event.replay_error]);
        Complete ->
            io:format("~n~s~n", [Complete])
    end.
