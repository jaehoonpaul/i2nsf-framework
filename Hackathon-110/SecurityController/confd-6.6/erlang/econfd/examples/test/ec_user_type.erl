-module(ec_user_type).

-include("../../include/econfd.hrl").

%% A port consists of three integer elements
%% "<shelf>/<slot>/<port>" - stored in a C_INT32 */

-define(MAX_SHELF, 3).
-define(MAX_SLOT, 15).
-define(MAX_PORT, 63).

-on_load(on_load/0).

on_load() ->
    TypeCbs = #confd_type_cbs{typepoint  = port_type,
                              str_to_val = fun port_str_to_val/2,
                              val_to_str = fun port_val_to_str/2,
                              validate   = fun port_validate/2},
    econfd_schema:register_type_cbs(TypeCbs),
    ok.

port_str_to_val(_Ctx, String) ->
    try
        StrL = string:tokens(String, "/"),
        [Shelf, Slot, Port] =
            [list_to_integer(string:strip(Str)) || Str <- StrL],
        if Shelf >= 0, Shelf =< ?MAX_SHELF,
           Slot >= 0, Slot =< ?MAX_SLOT,
           Port >= 0, Port =< ?MAX_PORT ->
                {ok, ?CONFD_INT32((Shelf bsl 16) bor (Slot bsl 8) bor Port)}
        end
    catch
        error:{badmatch, _} ->
            {error, <<"Wrong number of elements">>};
        error:badarg ->
            {error, <<"Non-integer element">>};
        error:if_clause ->
            {error, <<"Value out of range">>}
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
