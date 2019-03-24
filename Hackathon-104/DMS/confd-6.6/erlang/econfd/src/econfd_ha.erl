%%%-------------------------------------------------------------------
%%% @copyright 2006 Tail-F Systems AB
%%% @version {$Id$}
%%% @doc An Erlang interface equivalent to the HA C-API
%%% (documented in confd_lib_ha(3)).
%%% @end
%%%-------------------------------------------------------------------

-module(econfd_ha).

%%% external exports

-export([connect/2,
         connect/3,
         close/1,
         bemaster/2,
         slave_dead/2,
         beslave/4,
         benone/1,
         berelay/1,
         getstatus/1]).

-import(econfd_internal,
        [
         confd_call/2,
         term_write/2
        ]).

-include("../include/econfd.hrl").
-include("../include/econfd_errors.hrl").
-include("econfd_internal.hrl").

%%%--------------------------------------------------------------------
%%% External functions
%%%--------------------------------------------------------------------

%% @spec connect(Address::ip(), Token::binary()) ->
%%     {ok, Socket::term()} | {error, Reason::term()}
%% @doc Connect to the HA subsystem at the ConfD server on host with
%% address Address. The Token is a unique identifier which must be shared by
%% all nodes in the cluster.
connect(Address, Token) ->
    connect(Address, ?CONFD_PORT, Token).

%% @spec connect(Address::ip(), Port::integer(), Token::binary()) ->
%%     {ok, Socket::term()} | {error, Reason::term()}
%% @doc Connect to the HA subsystem on host with address Address:Port
%%
%% If the port is changed it must also be changed in confd.conf
%% To close a HA socket, use {@link close/1}.
connect(Address, Port, Token) ->
    case econfd_internal:connect(Address, Port, ?CLIENT_HA, []) of
        {ok, Socket} ->
            econfd_internal:bin_write(Socket, Token),
            {ok, Socket};
        Error ->
            Error
    end.

%% @spec close(Socket::term()) -> ok | {error, Reason::term()}
%% @doc Close the HA connection
close(Socket) ->
    econfd_internal:close(Socket).

%% @spec bemaster(Socket::term(), MyNodeId::econfd:value()) ->
%% ok | {error, Reason}
%% @doc instruct a HA node to be master in the cluster.
bemaster(Socket, MyNodeId) ->
    case confd_call(Socket, {?CONFD_HA_ORDER_BEMASTER, MyNodeId}) of
        {ok, Reply} -> Reply;
        Err -> Err
    end.

%% @spec slave_dead(Socket::term(), NodeId::econfd:value()) ->
%% ok | {error, Reason}
%% @doc instruct ConfD that another node is dead
slave_dead(Socket, NodeId) ->
    case confd_call(Socket, {?CONFD_HA_ORDER_SLAVE_DEAD, NodeId}) of
        {ok, Reply} -> Reply;
        Err -> Err
    end.


%% @spec beslave(Socket::term(), MyNodeId::econfd:value(),
%%                Master::#ha_node{},
%%                WaitReplyBool::integer()) ->
%% ok | {error, Reason}
%% @doc instruct a HA node to be slave in the cluster where
%% MasterNodeId is master.
beslave(Socket, MyNodeId, Master, WaitP) ->
    Request = {?CONFD_HA_ORDER_BESLAVE, MyNodeId,
               {Master#ha_node.nodeid, Master#ha_node.addr},
               WaitP},
    if
        WaitP == 1 ->
            case confd_call(Socket, Request) of
                {ok, Reply} -> Reply;
                Err -> Err
            end;
        WaitP == 0 ->
            term_write(Socket, Request)
    end.

%% @spec benone(Socket::term()) ->
%% ok | {error, Reason}
%% @doc instruct a HA node to be nothing in the cluster.
benone(Socket) ->
    case confd_call(Socket, {?CONFD_HA_ORDER_BENONE}) of
        {ok, Reply} -> Reply;
        Err -> Err
    end.

%% @spec berelay(Socket::term()) ->
%% ok | {error, Reason}
%% @doc instruct a HA slave to be a relay for other slaves.
berelay(Socket) ->
    case confd_call(Socket, {?CONFD_HA_ORDER_BERELAY}) of
        {ok, Reply} -> Reply;
        Err -> Err
    end.

%% @spec getstatus(Socket::term()) ->
%% {ok, #ha_status{}} | {error, Reason}
%% @doc Request status from a HA node
getstatus(Socket) ->
    case confd_call(Socket, {?CONFD_HA_ORDER_GETSTATUS}) of
        {ok, Reply} ->
            {Status, List} = Reply,
            {ok, #ha_status{status = Status,
                            data = lists:map(fun({Nid, Addr}) ->
                                                     #ha_node{nodeid = Nid,
                                                              addr = Addr}
                                             end, List)}};
        Err -> Err
    end.

