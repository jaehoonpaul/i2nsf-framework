%%%-------------------------------------------------------------------
%%% @copyright 2006 Tail-F Systems AB
%%% @version {$Id$}
%%% @private
%%% @doc Application callback module for the econfd application
%%% @end
%%%-------------------------------------------------------------------
-module(econfd_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    case econfd_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.
