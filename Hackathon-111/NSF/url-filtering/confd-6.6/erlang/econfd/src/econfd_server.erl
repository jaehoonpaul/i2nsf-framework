%%%-------------------------------------------------------------------
%%% @copyright 2006 Tail-F Systems AB
%%% @version {$Id$}
%%% @private
%%% @doc Main server for ConfD API.
%%%
%%% Currently only owns a couple of ets tables.
%%% @end
%%%-------------------------------------------------------------------
-module(econfd_server).

-behaviour(gen_server).

-include("../include/econfd.hrl").
-include("econfd_internal.hrl").
-include("econfd_proto.hrl").

%%% external exports
-export([start_link/0]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
         }).

-define(SERVER, ?MODULE).

%%%--------------------------------------------------------------------
%%% External functions
%%%--------------------------------------------------------------------

%% @spec start_link() -> {ok, pid()}
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%--------------------------------------------------------------------
%%% gen_server callbacks
%%%--------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    ets:new(confd_next_map_from_int, [public, named_table]),
    ets:insert(confd_next_map_from_int, {incr, 1}),
    ets:new(confd_installed_crypto_keys, [public, named_table]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State} .

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
