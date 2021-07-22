-module(ec_user_type_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("econfd/include/econfd.hrl").

-record(state, {}).

%% A port consists of three integer elements
%% "<shelf>/<slot>/<port>" - stored in a C_INT32 */

-define(MAX_SHELF, 3).
-define(MAX_SLOT, 15).
-define(MAX_PORT, 63).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% call(Msg) ->
%%     gen_server:call(?SERVER, Msg, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
init([]) ->
    %% process_flag(trap_exit, true), % Triggers call to terminate/2
    error_logger:error_msg("~p: Register type\n", [?SERVER]),
    TypeCbs = #confd_type_cbs{typepoint  = port_type,
                              str_to_val = fun port_str_to_val/2,
                              val_to_str = fun port_val_to_str/2,
                              validate   = fun port_validate/2},
    econfd_schema:register_type_cbs(TypeCbs),

    log(info, "Server started", []),

    {ok, #state{}}.

%%--------------------------------------------------------------------
handle_call(Req, _From, State) ->
    error_logger:error_msg("~p: Got unexpected call: ~p\n",
                           [?SERVER, Req]),
    Reply = error,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    error_logger:error_msg("~p: Got unexpected cast: ~p\n",
                           [?SERVER, Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
handle_info(Info, State) ->
    error_logger:error_msg("~p: Got unexpected info: ~p\n",
                           [?SERVER, Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    log(info, "Server stopped - ~p", [Reason]),
    ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

port_str_to_val(_Ctx, String) ->
    StrL = string:tokens(String, "/"),
    [Shelf, Slot, Port] = [list_to_integer(string:strip(Str)) || Str <- StrL],
    if Shelf >= 0, Shelf =< ?MAX_SHELF,
       Slot >= 0, Slot =< ?MAX_SLOT,
       Port >= 0, Port =< ?MAX_PORT ->
            {ok, ?CONFD_INT32((Shelf bsl 16) bor (Slot bsl 8) bor Port)}
    end.

port_val_to_str(_Ctx, ?CONFD_INT32(Int)) ->
    Shelf = (Int bsr 16) band 16#ff,
    Slot = (Int bsr 8) band 16#ff,
    Port = Int band 16#ff,
    {ok, io_lib:format("~w/~w/~w", [Shelf, Slot, Port])}.

port_validate(_Ctx, ?CONFD_INT32(Int)) ->
    Shelf = (Int bsr 16) band 16#ff,
    Slot = (Int bsr 8) band 16#ff,
    Port = Int band 16#ff,
    if Shelf >= 0, Shelf =< ?MAX_SHELF,
       Slot >= 0, Slot =< ?MAX_SLOT,
       Port >= 0, Port =< ?MAX_PORT ->
            ok
    end.

log(error, Format, Args) ->
    econfd:log(?CONFD_LEVEL_ERROR, "~p: " ++ Format, [?SERVER | Args]);
log(info, Format, Args) ->
    econfd:log(?CONFD_LEVEL_INFO,  "~p: " ++ Format, [?SERVER | Args]);
log(trace, Format, Args) ->
    econfd:log(?CONFD_LEVEL_TRACE, "~p: " ++ Format, [?SERVER | Args]).
