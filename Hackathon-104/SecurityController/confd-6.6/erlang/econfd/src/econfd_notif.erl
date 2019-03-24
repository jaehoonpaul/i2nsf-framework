%%%-------------------------------------------------------------------
%%% @copyright 2006 Tail-F Systems AB
%%% @version {$Id$}
%%% @doc An Erlang interface equivalent to the event notifications C-API,
%%% (documented in confd_lib_events(3)).
%%% @end
%%%-------------------------------------------------------------------

-module(econfd_notif).

%%% external exports

-export([connect/2,
         connect/3,
         connect/4,
         close/1,
         recv/1,
         recv/2,
         handle_notif/1,
         notification_done/2]).

-include("../include/econfd.hrl").
-include("../include/econfd_errors.hrl").
-include("econfd_internal.hrl").

%%%--------------------------------------------------------------------
%%% External functions
%%%--------------------------------------------------------------------

%% @spec connect(Address::ip(), Mask::integer()) ->
%%     {ok, Socket::term()} | {error, Reason::term()}
%% @doc Connect to the notif server on host with address Address
connect(Address, Mask) ->
    connect(Address, ?CONFD_PORT, Mask).

%% @spec connect(Address::ip(), Port::integer(), Mask::integer()) ->
%%     {ok, Socket::term()} | {error, Reason::term()}
%% @doc Connect to the notif server on host with address Address:Port
%%
%% If the port is changed it must also be changed in confd.conf.
%% The Mask argument is a bitmask made out of the
%% bits defined in econfd.hrl as CONFD_NOTIF_XXX.
%% To close a notification socket, use {@link close/1}.
connect(Address, Port, Mask) ->
    connect(Address, Port, Mask, []).

%% @spec connect(Address::ip(), Port::integer(), Mask::integer(), [Option]) ->
%%     {ok, Socket::term()} | {error, Reason::term()}
%% Option = {heartbeat_interval, integer()} |
%%          {health_check_interval, integer()} |
%%          {stream_name, atom()} |
%%          {start_time, econfd:datetime()} |
%%          {stop_time, econfd:datetime()} |
%%          {xpath_filter, binary()} |
%%          {usid, integer()}
%% @doc Connect to the notif server on host with address Address:Port
%%
%% If the port is changed it must also be changed in confd.conf.
%% The Mask argument is a bitmask made out of the
%% bits defined in econfd.hrl as CONFD_NOTIF_XXX.
%% If CONFD_NOTIF_HEARTBEAT and/or CONFD_NOTIF_HEALTH_CHECK is included in
%% Mask, the corresponding desired interval (in milliseconds) must be
%% included in the options list.
%% If CONFD_NOTIF_STREAM_EVENT is included in Mask, 'stream_name' must be
%% included in the options list, 'start_time'/'stop_time' option may be used
%% for replay, 'xpath_filter' may be used for event notification filtering,
%% and 'usid' may be given to apply AAA restrictions.
%% To close a notification socket, use {@link close/1}.
connect(Address, Port, Mask, Options) ->
    if (Mask band ?CONFD_NOTIF_HEARTBEAT) /= 0 ->
            {value, {_, HbInter}} =
                lists:keysearch(heartbeat_interval, 1, Options);
       true ->
            HbInter = 0
    end,
    if (Mask band ?CONFD_NOTIF_HEALTH_CHECK) /= 0 ->
            {value, {_, HcInter}} =
                lists:keysearch(health_check_interval, 1, Options);
        true ->
            HcInter = 0
    end,
    if (Mask band ?CONFD_NOTIF_STREAM_EVENT) /= 0 ->
            {value, {_, StreamName}} =
                lists:keysearch(stream_name, 1, Options),
            case lists:keysearch(start_time, 1, Options) of
                {value, {_, StartTime}} ->
                    case lists:keysearch(stop_time, 1, Options) of
                        {value, {_, StopTime}} -> ok;
                        false                  -> StopTime = undefined
                    end;
                false ->
                    StartTime = StopTime = undefined
            end,
            case lists:keysearch(xpath_filter, 1, Options) of
                {value, {_, Filter}} -> ok;
                false                -> Filter = undefined
            end,
            case lists:keysearch(usid, 1, Options) of
                {value, {_, Usid}} -> ok;
                false              -> Usid = 0
            end,
            UseIKP = 1,
            StreamInfo = {StreamName, StartTime, StopTime, Filter, Usid,UseIKP};
        true ->
            StreamInfo = undefined
    end,
    case econfd_internal:connect(Address, Port, ?CLIENT_EVENT_MGR, []) of
        {ok, Socket} ->
            Term = {Mask, HbInter, HcInter, StreamInfo},
            case econfd_internal:confd_call(Socket, Term) of
                {ok, _Term} ->           % always 'ok'
                    {ok, Socket};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @spec close(Socket::term()) -> ok | {error, Reason::term()}
%% @doc Close the event notification connection
close(Socket) ->
    econfd_internal:close(Socket).

%% @equiv recv(Socket, infinity)
recv(Socket) ->
    recv(Socket, infinity).

%% @spec recv(Socket::term(), TimeOut::integer() | infinity) ->
%%                   {ok, Notification} | {error, Reason::term()}
%%   Notification =
%%    #econfd_notif_audit{} |
%%    #econfd_notif_syslog{} | #econfd_notif_commit_simple{} |
%%    #econfd_notif_commit_diff{} | #econfd_notif_user_session{} |
%%    #econfd_notif_ha{} | #econfd_notif_subagent_info{} |
%%    #econfd_notif_commit_failed{} | #econfd_notif_snmpa{} |
%%    #econfd_notif_forward_info{} | #econfd_notif_confirmed_commit{} |
%%    #econfd_notif_upgrade{} | #econfd_notif_commit_progress{} |
%%    confd_heartbeat | confd_health_check
%% @doc Wait for an event notification message and return corresponding
%% record depending on the type of the message.
%% The logno element in the record is an integer.
%% These integers can be used as an index to the
%% function econfd_logsyms:get_logsym(LogNo) in order to get a
%% textual description for the event.
%%
%% When recv/2 returns <tt>{error, timeout}</tt> the connection (and its
%% event subscriptions) is still active and the application needs to call
%% recv/2 again. But if recv/2 returns
%% <tt>{error, Reason}</tt> the connection to ConfD is closed and all
%% event subscriptions associated with it are cleared.
recv(Socket, TimeOut) ->
    case econfd_internal:term_read(Socket, TimeOut) of
        {ok, Notif} ->
            {ok, handle_notif(Notif)};
        {error, closed} ->
            {error, closed};
        {error, timeout} ->
            {error, timeout};
        {error, _Reason} ->
            econfd_internal:close(Socket),
            {error, {?CONFD_ERR_OS, <<"">>}}
    end.


%% @spec handle_notif(B::binary()) ->
%%    #econfd_notif_audit{} |
%%    #econfd_notif_syslog{} | #econfd_notif_commit_simple{} |
%%    #econfd_notif_commit_diff{} | #econfd_notif_user_session{} |
%%    #econfd_notif_ha{} | #econfd_notif_subagent_info{} |
%%    #econfd_notif_commit_failed{} | #econfd_notif_snmpa{} |
%%    #econfd_notif_forward_info{} | #econfd_notif_confirmed_commit{} |
%%    #econfd_notif_upgrade{} | #econfd_notif_commit_progress{} |
%%    #econfd_notif_stream_event{} |
%%    confd_heartbeat | confd_health_check
%% @deprecated Use the function {@link recv/2} instead.
%% @doc Decode the notif message and return corresponding
%% record depending on the type of the message.
%% It is the resposibility of the application to
%% read data from the notifications socket.
handle_notif(B) when is_binary(B) ->
    handle_notif(?b2t(B));
handle_notif(Term) ->
    if element(1, Term) == ?CONFD_NOTIF_AUDIT ->
            #econfd_notif_audit{logno = element(2, Term),
                                user =  element(3, Term),
                                usid =  element(4, Term),
                                msg =   element(5, Term)};
       element(1, Term) == ?CONFD_NOTIF_DAEMON;
       element(1, Term) == ?CONFD_NOTIF_NETCONF;
       element(1, Term) == ?CONFD_NOTIF_DEVEL ->
            #econfd_notif_syslog{logno = element(2, Term),
                                 prio =  element(3, Term),
                                 msg =   element(4, Term)};
       element(1, Term) == ?CONFD_NOTIF_COMMIT_SIMPLE ->
            Uinfo = econfd:mk_uinfo(element(3, Term)),
            #econfd_notif_commit_simple{
                              db =      element(2, Term),
                              uinfo =   Uinfo,
                              commit_diff_available = element(4, Term),
                              flags =   element(5, Term),
                              user =    Uinfo#confd_user_info.username,
                              ip =      Uinfo#confd_user_info.ip,
                              context = Uinfo#confd_user_info.context,
                              usid =    Uinfo#confd_user_info.usid,
                              proto =   Uinfo#confd_user_info.proto
                             };
       element(1, Term) == ?CONFD_NOTIF_COMMIT_DIFF ->
            Uinfo = econfd:mk_uinfo(element(3, Term)),
            #econfd_notif_commit_diff{
                              db =      element(2, Term),
                              uinfo =   Uinfo,
                              th =      element(4, Term),
                              flags =   element(5, Term),
                              comment = maybe_element(6, Term),
                              label =   maybe_element(7, Term),
                              user =    Uinfo#confd_user_info.username,
                              ip =      Uinfo#confd_user_info.ip,
                              context = Uinfo#confd_user_info.context,
                              usid =    Uinfo#confd_user_info.usid,
                              proto =   Uinfo#confd_user_info.proto
                             };
       element(1, Term) == ?CONFD_NOTIF_USER_SESSION ->
            Uinfo = econfd:mk_uinfo(element(3, Term)),
            #econfd_notif_user_session{
                              type    = element(2, Term),
                              uinfo   = Uinfo,
                              db      = element(4, Term),
                              user =      Uinfo#confd_user_info.username,
                              ip =        Uinfo#confd_user_info.ip,
                              context =   Uinfo#confd_user_info.context,
                              usid =      Uinfo#confd_user_info.usid,
                              proto =     Uinfo#confd_user_info.proto,
                              clearpass = Uinfo#confd_user_info.clearpass,
                              logintime = Uinfo#confd_user_info.logintime
                             };
       element(1, Term) ==  ?CONFD_NOTIF_HA_INFO ->
            {Type, Data} = element(2, Term),
            if Type == ?CONFD_HA_INFO_NOMASTER ;
               Type == ?CONFD_HA_INFO_SLAVE_INITIALIZED;
               Type == ?CONFD_HA_INFO_IS_MASTER ;
               Type == ?CONFD_HA_INFO_IS_NONE ;
               Type == ?CONFD_HA_INFO_BESLAVE_RESULT ->
                    #econfd_notif_ha{type = Type,
                                     data = Data};
               Type == ?CONFD_HA_INFO_SLAVE_DIED;
               Type == ?CONFD_HA_INFO_SLAVE_ARRIVED ->
                    #econfd_notif_ha{type = Type,
                                     data = unpack_ha_node(Data)}
            end;
       element(1, Term) ==  ?CONFD_NOTIF_SUBAGENT_INFO ->
            Info = element(2, Term),
            #econfd_notif_subagent_info{
                               type = element(1, Info),
                               name = element(2, Info)
                              };
       element(1, Term) == ?CONFD_NOTIF_COMMIT_FAILED ->
            Provider = element(2, Term),
            case Provider of
                ?DP_NETCONF ->
                    #econfd_notif_commit_failed {
                   provider = Provider,
                   dbname   = element(3, Term),
                   ip       = element(4, Term),
                   port     = element(5, Term)
                };

                ?DP_EXTERNAL ->
                    #econfd_notif_commit_failed {
                   provider = Provider,
                   dbname      = element(3, Term),
                   daemon_name = element(4, Term)
                  }
            end;
       element(1, Term) == ?CONFD_NOTIF_SNMPA ->
            #econfd_notif_snmpa {
                              pdutype = element(2, Term),
                              ip     = element(3, Term),
                              port    = element(4, Term),
                              errstatus = element(5, Term),
                              errindex = element(6, Term),
                              varbind  = len = element(8, Term)
                             };
       element(1, Term) == ?CONFD_NOTIF_FORWARD_INFO ->
            #econfd_notif_forward_info {
                              type = element(2, Term),
                              target = element(3, Term),
                              uinfo = econfd:mk_uinfo(element(4, Term))
                             };
       element(1, Term) == ?CONFD_NOTIF_HEARTBEAT ->
            confd_heartbeat;
       element(1, Term) == ?CONFD_NOTIF_HEALTH_CHECK ->
            confd_health_check;
       element(1, Term) == ?CONFD_NOTIF_CONFIRMED_COMMIT ->
            #econfd_notif_confirmed_commit {
                              type = element(2, Term),
                              timeout = element(3, Term),
                              uinfo = econfd:mk_uinfo(element(4, Term))
                             };
       element(1, Term) == ?CONFD_NOTIF_UPGRADE_EVENT ->
            #econfd_notif_upgrade {
                              event = element(2, Term)
                             };
       element(1, Term) == ?CONFD_NOTIF_COMMIT_PROGRESS ->
            #econfd_notif_commit_progress {
                              dbname = element(2, Term),
                              usid = element(3, Term),
                              thandle = element(4, Term),
                              msg = element(5, Term)
                             };
       element(1, Term) == ?CONFD_NOTIF_STREAM_EVENT ->
            case element(2, Term) of
                Atom when Atom == notification_complete;
                          Atom == replay_complete ->
                    #econfd_notif_stream_event{type = Atom};
                {replay_failed, Msg} ->
                    #econfd_notif_stream_event{type = replay_failed,
                                               replay_error = Msg};
                {EventTime, Exml} ->
                    #econfd_notif_stream_event{type = notification_event,
                                               event_time = EventTime,
                                               values = Exml}
            end;
       element(1, Term) == ?NCS_NOTIF_PACKAGE_RELOAD ->
            ncs_package_reload;
       element(1, Term) == ?NCS_NOTIF_CQ_PROGRESS ->
            #econfd_notif_ncs_cq_progress {
                              type = element(2, Term),
                              timestamp = element(3, Term),
                              cq_tag = element(4, Term),
                              cq_id = element(5, Term),
                              completed_devices = element(6, Term),
                              transient_devices = element(7, Term),
                              failed_devices = element(8, Term),
                              completed_services = element(9, Term),
                              failed_services = element(10, Term)
                             };
       element(1, Term) == ?CONFD_NOTIF_REOPEN_LOGS ->
            confd_reopen_logs
    end.

unpack_ha_node({NodeId, Peer}) ->
    #ha_node{nodeid = NodeId, addr = Peer}.

maybe_element(N, Tuple) when tuple_size(Tuple) < N ->
    undefined;
maybe_element(N, Tuple) ->
    case element(N, Tuple) of
        <<>> -> undefined;
        Elem -> Elem
    end.

%% @spec notification_done(Sock::port(), Thandle::integer()) ->
%%   ok | {error,term()}
%% @doc Indicate that we're done
%% Whenever we subscribe to ?CONFD_NOTIF_COMMIT_DIFF we must indicate to
%% confd that we're done with the diff processing. The transaction
%% hangs until we've done this.
notification_done(Sock, Thandle) ->
    econfd_internal:term_write(Sock, {Thandle, done}).
