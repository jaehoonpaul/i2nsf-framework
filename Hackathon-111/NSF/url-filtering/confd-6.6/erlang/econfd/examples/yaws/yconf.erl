-module(yconf).
-compile(export_all).


%% This example is only runnable if yaws is poperly installed
%% in /usr/local

-include("../../include/econfd.hrl").
-include("/usr/local/lib/yaws/include/yaws_api.hrl").
-include("/usr/local/lib/yaws/include/yaws.hrl").

-define(NS, 'http://tail-f.com/ns/example/yaws').

start() ->
    application:start(econfd),
    spawn(fun go/0).

go() ->
    {ok, S} = econfd_cdb:connect({127,0,0,1}),
    read_and_load(S),
    {ok, S2} = econfd_cdb:connect({127,0,0,1}),
    {ok, Sub} = econfd_cdb:subscribe_session(S2),
    {ok, Point1} = econfd_cdb:subscribe(Sub, 1, ?NS, "/yaws"),
    {ok, Point2} = econfd_cdb:subscribe(Sub, 1, ?NS, "/yaws/id"),
    ok = econfd_cdb:subscribe_done(Sub),

    F = fun(Update) -> io:format("Received update: ~p~n", [Update]),
                       read_and_load(S),
                       ?CDB_DONE_PRIORITY
        end,
    econfd_cdb:wait(Sub, infinity, F).



read_and_load(S) ->
    {ok, CDB} = econfd_cdb:new_session(S, ?CDB_RUNNING),
    Y = [[?NS|yaws]],
    {ok, LogDir} = econfd_cdb:get_elem(CDB, [logdir| Y]),
    {ok, Id} = econfd_cdb:get_elem(CDB, [id| Y]),
    GC = yaws_config:make_default_gconf(Id, true),
    {ok, LogWrapSize} = econfd_cdb:get_elem(CDB, [log_wrap_size|Y]),
    {ok, CopyErrorLog} = econfd_cdb:get_elem(CDB, [copy_error_log| Y]),
    {ok, PickF} = econfd_cdb:get_elem(CDB,[pick_first_virthost_on_nomatch|Y]),

    GC2 = ?gc_set_pick_first_virthost_on_nomatch(GC, PickF),
    GC3 = ?gc_set_copy_errlog(GC2, CopyErrorLog),
    GC4 = GC3#gconf{logdir = binary_to_list(LogDir),
                    log_wrap_size = LogWrapSize},

    {ok,N} = econfd_cdb:num_instances(CDB, [vserver, vservers |Y]),
    io:format("N = ~p~n",[N]),
    Groups = get_groups(CDB, N-1, []),

    %% the commented call below would actually set the new
    %% conf for yaws ... if we were running yaws in the same node
    %%yaws_api:setconf(GC4, Groups),
    io:format("Would set ~p~n", [{GC4, Groups}]),
    econfd_cdb:end_session(CDB).


get_groups(_CDB, -1, Ack) ->
    Ack;
get_groups(CDB, N, Ack) ->
    T = [[N],vserver,vservers,[?NS|yaws]],
    {ok, Ip} = econfd_cdb:get_elem(CDB,[listen|T]),
    {ok, {_,Port}} = econfd_cdb:get_elem(CDB,[port|T]),
    {ok,NumSrv} = econfd_cdb:num_instances(CDB,[server,servers | T]),

    Servers = get_servers(CDB, N, NumSrv-1, []),
    Group = lists:map(fun(SC) -> SC#sconf{listen =Ip,port = Port} end,Servers),
    get_groups(CDB,N-1,[Group|Ack]).

%% use pushd to show how
get_servers(_CDB, _N, -1, Ack) -> Ack;
get_servers(CDB, N, M, Ack) ->
    Tail = [[M],server,servers,[N],vserver,vservers,[?NS|yaws]],
    {ok, Rhost} = econfd_cdb:get_elem(CDB,[rhost|Tail]),
    {ok,Deflate} = econfd_cdb:get_elem(CDB,[deflate|Tail]),
    {ok, Docroot} = econfd_cdb:get_elem(CDB,[docroot|Tail]),
    SC =  yaws_config:make_default_sconf(),
    SC1 = SC#sconf{rhost = Rhost, docroot= Docroot},
    get_servers(CDB, N, M-1, [SC1|Ack]).




