-module(math_eapi).
-compile(export_all).

-include("../../include/econfd.hrl").

start() ->
    application:start(econfd),
    timer:sleep(1000),
    proc_lib:spawn(fun go/0).


go() ->
    Action = #confd_action_cb{actionpoint = math3,
                              action = fun math/4},
    {ok,Daemon} = econfd:init_daemon(math_eapi, ?CONFD_TRACE, user, none,
                                    {127,0,0,1}, ?CONFD_PORT),
    ok = econfd:register_action_cb(Daemon, Action),
    ok = econfd:register_done(Daemon).


math(_Uinfo, _Name, _KeyPath, Params) ->

%%    io:format("~nmath Uinfo=~p~n Name=~p~n KeyPath=~p~n Params=~p~n",
%%            [_Uinfo, _Name, _KeyPath, Params]),

    %%  we know that we get exactly 3 parameters;
    %%     {add | sub, start}
    %%     {operand, [Operand1, Operand2]}
    %%     {add | sub, stop}

    [{Oper, start}, {_, [Op1, Op2]}|_] = Params,
    Result = case get_tag(Oper) of
                 add ->
                     io:format("~nmath: calculating ~p + ~p~n", [Op1, Op2]),
                     Op1 + Op2;
                 sub ->
                     io:format("~nmath: calculating ~p - ~p~n", [Op1, Op2]),
                     Op1 - Op2
             end,
    {ok, [{result, Result}]}.


get_tag([_Ns|Tag]) -> Tag;
get_tag(Tag) -> Tag.
