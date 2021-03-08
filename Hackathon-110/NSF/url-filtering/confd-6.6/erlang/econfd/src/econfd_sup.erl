%%%-------------------------------------------------------------------
%%% @copyright 2010 Tail-F Systems AB
%%% @version {$Id$}
%%% @doc Supervisor for the confd application
%%% @end
%%%-------------------------------------------------------------------
-module(econfd_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [top]).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%% None of these are allowed to die; if they do we restart everything.
init([top]) ->
    Confd = {econfd_server, {econfd_server, start_link, []},
             permanent, 2000, worker, [econfd_server]},
    Schemas = {econfd_schema, {econfd_schema, start_link, []},
               permanent, 2000, worker, [econfd_schema]},
    {ok,{{one_for_one ,0,1}, [Confd, Schemas]}}.


