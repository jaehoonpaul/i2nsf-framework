%%%-------------------------------------------------------------------
%%% @copyright 2012 Tail-F Systems AB
%%% @version {$Id$}
%%% @private Internal functions used in standalone econfd
%%% @end
%%%-------------------------------------------------------------------
-module(econfd_internal).

%%% application internal exports

%% - for communication
-export([connect/4, controlling_process/2, close/1]).
-export([confd_call/2, confd_call/3, confd_call/4]).
-export([confd_call_active/2, confd_call_bin/3]).
-export([term_write/2, term_write/3, term_write/4, bin_write/2]).
-export([term_read/2, term_read/3, term_get/1]).

%% - for econfd_schema
-export([schema_init/0]).
-export([load_schemas/2]).
-export([get_nslist/0, get_cs/2]).
-export([str2val/2, val2str/2]).
-export([load_schemas/3]).
-export([nsinfo/1]).
-export([register_type_cbs/1]).

-export([log/3,
         log/4]).

%%% internal exports
-export([check_val/2]).

%%% records
% econfd_schema gen_server state
-record(state, {
          nslist = []   %% econfd:ns() (ets tables)
         }).

%%% include files
-include("econfd_internal.hrl").
-include("econfd_cdb_api.hrl").
-include("../include/econfd.hrl").
-include("../include/econfd_errors.hrl").
-undef(MAAPI_UPGRADE_KILL_ON_TIMEOUT).
-undef(MAAPI_FLAG_HINT_BULK).
-undef(MAAPI_FLAG_NO_DEFAULTS).
-undef(MAAPI_FLAG_CONFIG_ONLY).
-undef(MAAPI_FLAG_HIDE_INACTIVE).
-undef(MAAPI_FLAG_DELAYED_WHEN).
-undef(MAAPI_FLAG_RUN_SET_HOOKS).
-include("econfd_maapi_proto.hrl").

%%% macros
-define(SERVER, econfd_schema).
-define(IPC_MOD, gen_tcp).
-define(OPTS_MOD, inet).

%%%--------------------------------------------------------------------
%%% External functions
%%%--------------------------------------------------------------------

%%%--------------------------------------------------------------------
%%% communication support
%%%--------------------------------------------------------------------

connect(Address, Port, Id, Opts) ->
    ConnOpts = [binary, {active, false}, {nodelay, true}, {packet, 0}],
    case ?IPC_MOD:connect(Address, Port, ConnOpts) of
        {ok, Socket} ->
            ?IPC_MOD:send(Socket, <<Id,?LIB_PROTO_VSN:16,?LIB_VSN:32,0,0>>),
            case check_vsn_reply(Socket) of
                ok ->
                    ?OPTS_MOD:setopts(Socket, [{packet, 4}] ++ Opts),
                    {ok, Socket};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

check_vsn_reply(Socket) ->
    case ?IPC_MOD:recv(Socket, 9) of
        {ok, <<0:8,_ProtoVsn:16,_ConfdVsn:32,_MaxDepth,_MaxKeys>>} ->
            ok;
        {ok, <<1:8,_ProtoVsn:16,_ConfdVsn:32,_MaxDepth,_MaxKeys>>} ->
            error_logger:format("bad lib version - confd rejects us",[]),
            ?IPC_MOD:close(Socket),
            {error, badvsn};
        Err ->
            Err
    end.

controlling_process(Socket, Pid) ->
    ?IPC_MOD:controlling_process(Socket, Pid).

close(Socket) ->
    ?IPC_MOD:close(Socket).

confd_call(Sock, Term) ->
    case term_write(Sock, Term) of
        ok ->
            term_read(Sock, infinity);
        Error ->
            Error
    end.

confd_call(Socket, Term, Op) ->
    case term_write(Socket, Term, Op) of
        ok ->
            term_read(Socket, Op, infinity);
        Error ->
            Error
    end.

confd_call(Socket, Term, Op, TH) ->
    case term_write(Socket, Term, Op, TH) of
        ok ->
            term_read(Socket, Op, infinity);
        Error ->
            Error
    end.

confd_call_active(Sock, Term) ->
    case term_write(Sock, Term) of
        ok ->
            term_read_active(Sock);
        Error ->
            Error
    end.

confd_call_bin(Socket, Bin, Op) ->
    case bin_write(Socket, Bin, Op) of
        ok ->
            term_read(Socket, Op, infinity);
        Error ->
            Error
    end.

term_write(Socket, Term) ->
    TermBin = ?t2b(Term),
    ?IPC_MOD:send(Socket, TermBin).

term_write(Socket, Term, Op) ->
    TermBin = ?t2b(Term),
    ?IPC_MOD:send(Socket, <<Op:32, TermBin/binary>>).

term_write(Socket, Term, Op, -1) ->
    term_write(Socket, Term, Op);
term_write(Socket, Term, Op, TH) ->
    TermBin = ?t2b(Term),
    ?IPC_MOD:send(Socket, <<Op:32, TH:32, TermBin/binary>>).

bin_write(Socket, Bin) ->
    ?IPC_MOD:send(Socket, Bin).

bin_write(Socket, Bin, Op) ->
    ?IPC_MOD:send(Socket, <<Op:32, Bin/binary>>).

%% @spec term_read(Socket::term(), Timeout::integer() | infinity) ->
%%  ok | {ok, term()} | {error, Reason::term()}
term_read(Socket, TimeOut) ->
    case ?IPC_MOD:recv(Socket, 0, TimeOut) of
        {ok, Data} ->
            term_res(term_get(Data));
        Error ->
            Error
    end.

%% @spec term_read(Socket::term(), ExpectOp::integer(),
%%                         Timeout::integer() | infinity) ->
%%  ok | {ok, T::term()} | econfd_maapi:err()
term_read(Socket, ExpectOp, TimeOut) ->
    case ?IPC_MOD:recv(Socket, 0, TimeOut) of
        {ok, <<ExpectOp:32>>} ->
            ok;
        {ok, <<ExpectOp:32, Reply/binary>>} ->
            {ok, term_get(Reply)};
        {ok, <<Op:32, Ecode:32>>} when ?REPLY_IS_ERROR(Op) ->
            {error, {Ecode, <<"">>}};                   % No error desription?
        {ok, <<Op:32, Ecode:32, Reason0/binary>>}
          when ?REPLY_IS_ERROR(Op) ->
            Reason = case binary_to_term(Reason0) of
                         Bin when is_binary(Bin) -> Bin;
                         {Bin, _Tag} -> Bin    % FIXME? AppTag for maapi
                     end,
            %% Terminating NUL is helpful for a C interface...
            {String, _NULL} = split_binary(Reason, size(Reason)-1),
            {error, {Ecode, String}};
        {ok, Data} ->
            error_logger:format("~w: Unexpected data on socket! "
                                "Expect:~p ~999p\n",
                                [?MODULE, ExpectOp, Data]),
            ?IPC_MOD:close(Socket),
            {error, {?CONFD_ERR_INTERNAL, <<"">>}};
        {error, closed} ->
            {error, closed};
        {error, timeout} ->
            {error, timeout};
        {error, _Reason} ->
            ?IPC_MOD:close(Socket),
            {error, {?CONFD_ERR_OS, <<"">>}}
    end.

term_read_active(Sock) ->
    receive
        {tcp, Sock, Data} ->
            term_res(term_get(Data));
        {tcp_closed, Sock} ->
            {error, closed}
    end.

term_res({error, Code, Reason}) ->
    {error, {Code, Reason}};
term_res(Res) ->
    {ok, Res}.

term_get(Bin) ->
    ?b2t(Bin).

%%%--------------------------------------------------------------------
%%% econfd_schema support
%%%--------------------------------------------------------------------

%% @spec get_nslist() -> [#confd_nsinfo{}]
%% @doc Get a list of loaded namespaces with info
get_nslist() ->
    gen_server:call(?SERVER, get_nslist, infinity).

%% @spec get_cs(Ns::econfd:namespace(), Tagpath::econfd:tagpath()) ->
%%         #confd_cs_node{} | not_found
%% @doc Find schema node by namespace and tagpath.
get_cs(Ns, Tagpath) ->
    case catch ets:lookup(Ns, Tagpath) of
        [Cs] when is_record(Cs, confd_cs_node) ->
            Cs;
        _ ->
            not_found
    end.

str2val(Type, Lexical) ->
   string2value([], [], Type, Lexical, true, undefined, []).

val2str(Type, Value) ->
    value2string([], Type, Value, xml).

register_type_cbs(TypeCbs) ->
    ets:insert(confd_type_cbs, TypeCbs).

schema_init() ->
    ets:new(confd_type_cbs, [{keypos, 2}, public, named_table]),
    process_flag(trap_exit, true),
    {ok, #state{}}.

%% @spec load_schemas(Address::ip(), Port::integer()) ->
%%                                 ok | {error, Reason::term()}
%% @doc Load schema info from ConfD
load_schemas(Address, Port) ->
    gen_server:call(?SERVER, {load, Address, Port}, infinity).

%% @spec load_schemas(Address::ip(), Port::integer(), #state{}) ->
%%          {ok, #state{}} | {error, Reason::term()}
load_schemas(Address, Port, State) ->
    case do_load_schemas(Address, Port, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        Error ->
            {reply, Error, State}
    end.

do_load_schemas(Address, Port, State) ->
    case econfd_maapi:connect(Address, Port) of
        {ok, Sock} ->
            try
                Bin = ?t2b(ikp),
                Req = <<?MAAPI_LOAD_ALL_NS:32, Bin/binary>>,
                case ?IPC_MOD:send(Sock, Req) of
                    ok ->
                        load_all_ns(Sock, State, [], 0, []);
                    Error ->
                        Error
                end
            catch
                _:Rsn ->
                    ?cerror_msg("load_schemas failed: ~p\n", [Rsn]),
                    {error, {?CONFD_ERR_INTERNAL, <<"">>}}
            after
                econfd_maapi:close(Sock)
            end;
        Error ->
            Error
    end.

recv_ns_data(_Sock, [Term|Rest]) ->
    {ok, Term, Rest};
recv_ns_data(Sock, []) ->
    case ?IPC_MOD:recv(Sock, 0) of
        {ok, Data} ->
            case ?b2t(Data) of
                [Term|Rest] ->
                    {ok, Term, Rest};
                Term ->
                    {ok, Term, []}
            end;
        Error ->
            Error
    end.

load_all_ns(Sock, State, Loaded, N, Acc0) ->
    case recv_ns_data(Sock, Acc0) of
        {ok, Term, Acc1} ->
            case Term of
                _ when element(1, Term) == ns ->
                    Uri    = element(2, Term),
                    Prefix = element(3, Term),
                    IdH    = element(4, Term),
                    Rev    = maybe_element(5, Term),
                    Mod    = maybe_element(6, Term),
                    TempName = ?l2a("confd_ns_" ++ ?i2l(N)),
                    Tab = ets:new(TempName, [{keypos, 2}, public, named_table]),
                    NsHeader = #ns_header{key = ns_header, uri = Uri,
                                          prefix = ?a2l(Prefix),
                                          id_hash_value = IdH,
                                          revision = Rev, module = Mod},
                    Res = try
                              ets:insert(Tab, NsHeader),
                              load_one_ns(Sock, Tab, Uri, Acc1)
                          catch
                              _:Rsn ->
                                  ?cerror_msg("load_schemas failed: ~p\n",
                                              [Rsn]),
                                  {error, {?CONFD_ERR_INTERNAL, <<"">>}}
                          end,
                    case Res of
                        {ok, Acc2} ->
                            load_all_ns(Sock, State,
                                        [TempName|Loaded], N+1, Acc2);
                        Error ->
                            load_failed([TempName|Loaded], Error)
                    end;
                eof ->
                    [ets:delete(Tab) || Tab <- State#state.nslist],
                    F = fun (Tab) ->
                                [#ns_header{uri = Uri}] =
                                    ets:lookup(Tab, ns_header),
                                ets:rename(Tab, Uri)
                        end,
                    {ok, State#state{nslist = [F(Tab) || Tab <- Loaded]}};
                Other ->
                    ?error_msg("bad schema data: ~p~n", [Other]),
                    load_failed(Loaded,
                                {error,
                                 {?CONFD_ERR_INTERNAL, <<"Bad schema data">>}})
            end;
        {error, closed} ->
            load_failed(Loaded, {error, closed});
        {error, _Reason} ->
            load_failed(Loaded, {error, {?CONFD_ERR_OS, <<"">>}})
    end.

maybe_element(N, Tuple) when tuple_size(Tuple) < N ->
    undefined;
maybe_element(N, Tuple) ->
    case element(N, Tuple) of
        '' -> undefined;
        Elem -> Elem
    end.

load_one_ns(Sock, Tab, Uri, Acc0) ->
    case recv_ns_data(Sock, Acc0) of
        {ok, Term, Acc1} ->
            case Term of
                {node,
                 Tagpath, Type, Ptype, Default, MinOccurs, MaxOccurs,
                 Children, Keys, Flags, Actions, Cmp, Notifs, MetaData} ->
                    Cs = #confd_cs_node{tagpath = Tagpath,
                                        namespace = Uri,
                                        type = typeref(Type, Uri),
                                        primitive_type = Ptype,
                                        default = Default,
                                        min_occurs = MinOccurs,
                                        max_occurs = MaxOccurs,
                                        children = Children,
                                        keys = Keys,
                                        flags = Flags,
                                        actions = Actions,
                                        cmp = Cmp,
                                        notifs = Notifs,
                                        meta_data = MetaData},
                    ets:insert(Tab, Cs),
                    load_one_ns(Sock, Tab, Uri, Acc1);
                {type, Name, Type, Derivation} ->
                    ET = #exs_type{name = Name, type = Type,
                                   derivation = mk_derivation(Derivation, Uri)},
                    ets:insert(Tab, ET),
                    load_one_ns(Sock, Tab, Uri, Acc1);
                end_of_ns ->
                    {ok, Acc1};
                Other ->
                    ?error_msg("bad schema data: ~p~n", [Other]),
                    {error, {?CONFD_ERR_INTERNAL, <<"Bad schema data">>}}
            end;
        {error, closed} ->
            {error, closed};
        {error, _Reason} ->
            {error, {?CONFD_ERR_OS, <<"">>}}
    end.

mk_derivation({restriction, Base, Facets}, Ns) ->
    #restriction{base = typeref(Base, Ns),
                 facets = [mk_facet(F) || F <- Facets]};
mk_derivation({list, ItemType}, Ns) ->
    #list{item_type = typeref(ItemType, Ns)};
mk_derivation({union, MemberTypes}, Ns) ->
    #union{member_types = [typeref(T, Ns) || T <- MemberTypes]};
mk_derivation({bits, Fields, Size}, _Ns) ->
    #bits{fields = Fields, size = Size};
mk_derivation(undefined, _Ns) ->  % user-defined type
    undefined.

mk_facet({enumeration, String, Value}) ->
    #enumeration{value = String, hash_value = Value};
mk_facet({fraction_digits, Value}) ->
    #fraction_digits{value = Value};
mk_facet({length, Value}) ->
    #length{value = Value};
mk_facet({unique_list}) ->
    #unique_list{};
mk_facet({max_exclusive, Value}) ->
    #max_exclusive{value = Value};
mk_facet({max_inclusive, Value}) ->
    #max_inclusive{value = Value};
mk_facet({max_length, Value}) ->
    #max_length{value = Value};
mk_facet({min_exclusive, Value}) ->
    #min_exclusive{value = Value};
mk_facet({min_inclusive, Value}) ->
    #min_inclusive{value = Value};
mk_facet({min_length, Value}) ->
    #min_length{value = Value};
mk_facet({pattern, Value}) ->
    #pattern{value = Value};
mk_facet({display_hint, Value}) ->
    #display_hint{value = Value};
mk_facet({total_digits, Value}) ->
    #total_digits{value = Value};
mk_facet({white_space, Value}) ->
    #white_space{value = Value};
mk_facet({identities, _} = IgnoreValue) ->
    #ignore_facet{value = [IgnoreValue]};
mk_facet({ignore_facet, Value}) ->
    #ignore_facet{value = Value};
mk_facet({range_facet, Value}) ->
    #range_facet{value = Value}.

typeref(undefined, _Ns)              -> undefined;
typeref({'', TypeName}, _Ns)         -> econfd_schema:get_type(TypeName);
typeref(Name, Ns) when is_atom(Name) -> {Ns, Name};
typeref(Other, _Ns)                  -> Other.

load_failed(Loaded, Error) ->
    [ets:delete(Tab) || Tab <- Loaded],
    Error.

nsinfo(#state{nslist = NsList} = State) ->
    {reply, nsinfo(NsList), State};
nsinfo([Ns|Rest]) ->
    [#ns_header{uri = Uri, prefix = Prefix,
                revision = Revision, module = Module}] =
        ets:lookup(Ns, ns_header),
    [#confd_nsinfo{namespace = Uri, prefix = Prefix,
                   revision = Revision, module = Module}|nsinfo(Rest)];
nsinfo([]) ->
    [].

%% @spec string2value(NsList, NsStack, Type, Lexical, TraverseP, VerifyVal,
%%                    Options) ->
%%           {ok, Value} | {error, Reason::term()}
%%     NsList = saxlite:ns_list()
%%     NsStack = saxlite:ns_stack()
%%     Type = {Ns, TypeName}
%%     TypeName = atom()
%%     Ns = atom()
%%     Tagpath = tagpath()
%%     Lexical = binary()
%%     TraverseP = boolean()
%%     VerifyVal = Value | undefined
%%     Options = [Option]
%%     Option = ignore_facets | ignore_user_defined_types
%%     Value = xsd:types()
%% @private Convert lexical value to value in value space.
%% Ns and TypeName must refer to matching qualified namespace and type
%% in NsList. NsStack is only needed if the type you try to convert is
%% a QName type in W3C's XML Schema standard. ?B* is as defined in exs.hrl.
string2value(NsList, NsStack, Type, Lexical, TraverseP, VerifyVal, Options)
  when is_binary(Lexical) ->
    string2value(NsList, NsStack, Type, ?b2l(Lexical), TraverseP, VerifyVal,
                 Options);
string2value(NsList, NsStack, {Ns, TypeName}, Lexical, TraverseP, VerifyVal,
             Options) ->
    try
        TypeStack = mk_type_stack(NsList, Ns, {Ns, TypeName}, false),
        {ok, convert_string(NsStack, Lexical, TraverseP, VerifyVal, Options,
                            TypeStack)}
    catch
        {failed, #cs_error{code = invalid_type_name}} ->
            {error, {?CONFD_ERR_BADTYPE, <<"Invalid type">>}};
        {failed, #cs_error{code = unknown_namespace}} ->
            {error, {?CONFD_ERR_BADTYPE, <<"Invalid type">>}};
        {failed, #cs_error{code = user_error, opaque = Reason}} ->
            {error, {?CONFD_ERR_BADTYPE, Reason}};
        {failed, _Reason} ->
            {error, {?CONFD_ERR_BADTYPE, <<"Invalid string for type">>}}
    end;
string2value(_NsList, _NsStack, _Type, _Lexical, _TraverseP, _VerifyVal,
             _Options) ->
    {error, {?CONFD_ERR_BADTYPE, <<"Invalid type">>}}.

%% @spec value2string(NsList, Type, Value, FmtType) ->
%%           {ok, string()} | {error, term()}
%%     NsList = saxlite:ns_list()
%%     Type = {Ns, TypeName}
%%     Ns = atom()
%%     TypeName = atom()
%%     Value = term()
%%     FmtType = cli | xml
%% @private Convert value space value to a lexical value.
%% Ns and TypeName must refer to matching qualified namespace and type
%% in NsList.
value2string(NsList, {Ns, TypeName}, Value, FmtType) ->
    try
        TypeStack = mk_type_stack(NsList, Ns, {Ns, TypeName}, false),
        {ok, convert_value({Ns, TypeName}, Value, TypeStack, FmtType)}
    catch
        {failed, #cs_error{code = invalid_type_name}} ->
            {error, {?CONFD_ERR_BADTYPE, <<"Invalid type">>}};
        {failed, #cs_error{code = unknown_namespace}} ->
            {error, {?CONFD_ERR_BADTYPE, <<"Invalid type">>}};
        {failed, #cs_error{code = user_error, opaque = Reason}} ->
            {error, {?CONFD_ERR_BADTYPE, Reason}};
        {failed, _Reason} ->
            {error, {?CONFD_ERR_BADTYPE, <<"Invalid value for type">>}}
    end;
value2string(_NsList, _Type, _Value, _FmtType) ->
    {error, {?CONFD_ERR_BADTYPE, <<"Invalid type">>}}.

%% @spec check_val(TypeId, Value) ->
%%           {ok, Lexical::binary()} | {error, #cs_error{}}
%%     TypeId = #confd_cs_node{} | econfd:type()
%%     Value = econfd:value()
%% @private Check if a value is valid according to its type.
%% This function first converts the value to a string (if possible) in order
%% to make it to check its lexical value space (regexp pattern matching and
%% more) and its actual native value according to its type.
check_val(#confd_cs_node{type = Type}, Value) ->
    check_val(Type, Value);
check_val(Type, Value) ->
    case get_type_stack([], Type) of
        {atomic, _Flags, _ValidFacets,
         [#exs_type{type = {user_defined, Id}}], []} ->
            try
                [#confd_type_cbs{validate = Validate, val_to_str = Val2Str}] =
                    ets:lookup(confd_type_cbs, Id),
                Res = case Validate(undefined, Value) of
                          ok ->
                              Val2Str(undefined, Value);
                          R ->
                              R
                      end,
                case Res of
                    {ok, String} ->
                        {ok, String};
                    {error, Reason} ->
                        {error, {?CONFD_ERR_BADTYPE, Reason}}
                end
            catch
                error:_ ->
                    {error, {?CONFD_ERR_BADTYPE, <<"Invalid value for type">>}}
            end;
        _ ->
            case econfd_schema:val2str(Type, Value) of
                {ok, String} ->
                    case string2value([], [], Type, String, true,
                                      Value, []) of
                        {ok, _} ->
                            {ok, String};
                        Error -> Error
                    end;
                Error -> Error
            end
    end.

verify_value(Value, undefined) ->
    Value;
verify_value(Value, Value) ->
    Value;
verify_value(Value, VerifyVal)
  when is_float(Value), is_float(VerifyVal) ->
    Value;
verify_value([], []) ->
    [];
verify_value([VH|VT], [VVH|VVT]) ->
    [verify_value(VH, VVH)|verify_value(VT, VVT)];
verify_value(_Value, VerifyVal) ->
    throw({failed, cs_error(bad_value, VerifyVal)}).

verify_hd([H|_]) -> H;
verify_hd(V)     -> V.

verify_tl([_|T]) -> T;
verify_tl(V)     -> V.

get_type_stack(NsList, Type) ->
    get_type_stack(NsList, Type, _AddNamespacesP = false).

get_type_stack(NsList, {Ns, TypeName}, AddNamespacesP) ->
    try
        mk_type_stack(NsList, Ns, TypeName, AddNamespacesP)
    catch
        {failed, Reason} -> {error, Reason}
    end.

format_type({Uri, TypeName}) -> ?a2l(Uri)++":"++?a2l(TypeName);
format_type(TypeName) -> ?a2l(TypeName).

lookup_sh(?W3C_SCHEMA_URI, _) ->
    fun schema_handler/1;
lookup_sh(?CONFD_URI, _) ->
    fun schema_handler/1;
lookup_sh(Ns, _) ->
    fun (Type) ->
            case catch ets:lookup(Ns, Type) of
                [Exs] when is_record(Exs, exs_type) ->
                    [Exs];
                _ -> []
            end
    end.

%%% Make type stack

mk_type_stack(NsList, Ns, TypeName, AddNamespacesP) ->
    SH = lookup_sh(Ns, NsList),
    mk_type_stack(NsList, Ns, TypeName, AddNamespacesP, SH).

mk_type_stack(NsList, Ns, TypeName, AddNamespacesP, SH) ->
    mk_type_stack(NsList, Ns, TypeName, AddNamespacesP, SH,
                  {atomic, 0, [], []}).

mk_type_stack(NsList, Ns, {Ns, TypeName}, AddNamespacesP, SH,
              {Mode, ModeFlags, AtomicTypeStack, ListTypeStack}) ->
    case lookup_type(TypeName, SH) of
        ExsType when ExsType#exs_type.derivation == undefined ->
            {Mode, ModeFlags, applicable_facets(TypeName),
             [ExsType|AtomicTypeStack], ListTypeStack};
        #exs_type{derivation = #restriction{base = Base}} = ExsType ->
            mk_type_stack(NsList, Ns, Base, AddNamespacesP, SH,
                          {Mode, ModeFlags, [ExsType|AtomicTypeStack],
                           ListTypeStack});
        #exs_type{derivation = #list{item_type = ItemType}} ->
            ListFlags = 0,
            mk_type_stack(NsList, Ns, ItemType, AddNamespacesP, SH,
                          {list, ListFlags, [], AtomicTypeStack});
        #exs_type{derivation = #union{member_types = MemberTypes}} = ExsType ->
            MemberTypeStacks =
                lists:map(fun(Type) ->
                                  mk_type_stack(NsList, Ns, Type,
                                                AddNamespacesP, SH)
                          end, MemberTypes),
            {Mode, ModeFlags, undefined,
             [{union, ExsType, MemberTypeStacks}|AtomicTypeStack],
             ListTypeStack};
        #exs_type{derivation = #bits{fields = Fields, size = Size}} ->
            {bits, ModeFlags, TypeName, Fields, Size}
    end;
mk_type_stack(NsList, _Ns, {NewNs, TypeName}, AddNamespacesP, _SH, Acc) ->
    NewSH = lookup_sh(NewNs, NsList),
    mk_type_stack(NsList, NewNs, {NewNs, TypeName}, AddNamespacesP, NewSH, Acc);
mk_type_stack(NsList, Ns, TypeName, AddNamespacesP, SH, Acc) ->
    mk_type_stack(NsList, Ns, {Ns, TypeName}, AddNamespacesP, SH, Acc).

lookup_type(TypeName, SH) ->
    case SH(TypeName) of
        [] ->
            throw({failed, cs_error(invalid_type_name, TypeName)});
        [ExsType] ->
            ExsType
    end.

%% One clause for each primitive type in xs.exs.
applicable_facets(collapsed_string) -> ?collapsed_string_facets;
applicable_facets(token) -> applicable_facets(collapsed_string);
applicable_facets(anyURI) -> applicable_facets(collapsed_string);
applicable_facets(base64Binary) -> applicable_facets(collapsed_string);
applicable_facets(hexBinary) -> applicable_facets(collapsed_string);
applicable_facets('NOTATION') -> applicable_facets(collapsed_string);
applicable_facets('QName') -> applicable_facets(collapsed_string);
applicable_facets(string) -> ?string_facets;
applicable_facets(normalizedString) -> applicable_facets(string);
applicable_facets(float) -> ?float_facets;
applicable_facets(double) -> applicable_facets(float);
applicable_facets(integer) -> ?integer_facets;
applicable_facets(nonPositiveInteger) -> ?integer_facets;
applicable_facets(negativeInteger) -> ?integer_facets;
applicable_facets(long) -> ?integer_facets;
applicable_facets(int) -> ?integer_facets;
applicable_facets(short) -> ?integer_facets;
applicable_facets(byte) -> ?integer_facets;
applicable_facets(nonNegativeInteger) -> ?integer_facets;
applicable_facets(positiveInteger) -> ?integer_facets;
applicable_facets(unsignedLong) -> ?integer_facets;
applicable_facets(unsignedInt) -> ?integer_facets;
applicable_facets(unsignedShort) -> ?integer_facets;
applicable_facets(unsignedByte) -> ?integer_facets;
applicable_facets(decimal) -> ?decimal_facets;
applicable_facets(boolean) -> ?boolean_facets;
applicable_facets(dateTime) -> ?date_facets;
applicable_facets(date) -> ?date_facets;
applicable_facets(time) -> ?date_facets;
applicable_facets(duration) -> ?date_facets;
applicable_facets(decimal64) -> ?decimal_facets;
applicable_facets(identityref) -> ?identityref_facets;
%%  FIXME: To fix this for primitive types. Not the right thing to do.
applicable_facets(_) -> applicable_facets(collapsed_string)++?float_facets.

%%% convert_string

convert_string(NsStack, Lexical, TraverseP, VerifyVal, Options,
                {atomic, _Flags, ValidFacets, AtomicTypeStack, []}) ->
    verify_value(get_value(NsStack, Lexical, TraverseP, VerifyVal, Options,
                           ValidFacets, AtomicTypeStack), VerifyVal);
convert_string(NsStack, Lexical, TraverseP, VerifyVal, Options,
               {list, _Flags, ValidFacets, AtomicTypeStack, ListTypeStack}) ->
%%    case ?bit_is_set(Flags, ?F_EXS_TYPE_IS_LEAF_LIST) of
%%        true ->
%%            ListItems = misc:tokenize(misc:strip(Lexical), newline_is_ws);
%%        false ->
            ListItems = string:tokens(ws_collapse(Lexical), " "),
%%    end,
    ItemValues =
        get_item_values(NsStack, TraverseP, VerifyVal, Options, ListItems,
                        ValidFacets, AtomicTypeStack),
    %% Check list. May throw an exception.
    get_value(NsStack, Lexical, ItemValues, TraverseP, VerifyVal, Options,
              ?list_facets, ListTypeStack, ListTypeStack),
    verify_value(ItemValues, VerifyVal);
convert_string(_NsStack, Lexical, _TraverseP, VerifyVal, _Options,
               {bits, _Flags, TypeName, Fields, Size}) ->
    verify_value(get_bits_value(Lexical, TypeName, Fields, Size), VerifyVal).

get_value(NsStack, Lexical, TraverseP, VerifyVal, Options, ValidFacets,
          TypeStack) ->
    get_value(NsStack, Lexical, Lexical, TraverseP, VerifyVal, Options,
              ValidFacets, TypeStack, TypeStack).

get_value(_NsStack, _Lexical, Value, _TraverseP, _VerifyVal, _Options,
          _ValidFacets, [], _FullTypeStack) ->
    Value;
get_value(NsStack, Lexical, Value, TraverseP, VerifyVal, Options, ValidFacets,
          [{union, #exs_type{name = TypeName}, MemberTypeStacks}|Rest],
         FullTypeStack) ->
    Value2 =
        get_member_value(NsStack, Value, TraverseP, VerifyVal, Options,
                         TypeName, MemberTypeStacks),
    get_value(NsStack, Lexical, Value2, TraverseP, VerifyVal, Options,
              ValidFacets, Rest, FullTypeStack);
get_value(NsStack, Lexical, _Value, TraverseP, VerifyVal, Options, ValidFacets,
          [ExsType|Rest], FullTypeStack)
  when ExsType#exs_type.type == primitive;
       element(1, ExsType#exs_type.type) == user_defined ->
    Lexical2 = lexical_value(ExsType, Lexical),
    Value2 = value(NsStack, FullTypeStack, ExsType, Lexical2, Options),
%%    case TraverseP of
%%      false -> Value2;
%%      true ->
            get_value(NsStack, Lexical2, Value2, TraverseP, VerifyVal, Options,
                      ValidFacets, Rest, FullTypeStack);
%%    end;
get_value(NsStack, Lexical, Value, TraverseP, VerifyVal, Options, ValidFacets,
          [#exs_type{name = TypeName, type = derived,
                     derivation = #restriction{base = Base, facets = Facets}}|
           Rest], FullTypeStack) ->
    applicable_facets(ValidFacets, Base, Facets),
    Flags = 0,
    {Lexical2, Value2} = eval_facets(TypeName, Lexical, Value, Facets, Flags),
    get_value(NsStack, Lexical2, Value2, TraverseP, VerifyVal, Options,
              ValidFacets, Rest, FullTypeStack).

get_member_value(_NsStack, Lexical, _TraverseP, _VerifyVal, _Options, _TypeName,
                 []) ->
    throw({failed, cs_error(bad_lexical, Lexical)});
get_member_value(NsStack, Lexical, TraverseP, VerifyVal, Options, TypeName,
                 [TypeStack|Rest]) ->
    try
        convert_string(NsStack, Lexical, TraverseP, VerifyVal, Options,
                       TypeStack)
    catch
        {failed, _} ->
            get_member_value(NsStack, Lexical, TraverseP, VerifyVal, Options,
                             TypeName, Rest)
    end.

lexical_value(#exs_type{lexical_value_fun = undefined}, Lexical) ->
    Lexical;
lexical_value(#exs_type{lexical_value_fun = F}, Lexical) ->
    case catch F(Lexical) of
        {'EXIT', _Reason} ->
            throw({failed, cs_error(bad_lexical, Lexical)});
        Lexical2 -> Lexical2
    end.

value(_NsStack, _TypeStack,
      #exs_type{type = {user_defined, Id}}, Lexical, _Options) ->
    try
        [#confd_type_cbs{str_to_val = Str2Val}] =
            ets:lookup(confd_type_cbs, Id),
        case Str2Val(undefined, Lexical) of
            {ok, Value} ->
                Value;
            {error, Message} ->
                throw({failed, cs_error(user_error, Message)})
        end
    catch
        error:_ ->
            throw({failed, cs_error(bad_lexical, Lexical)})
    end;
value(_NsStack, _TypeStack, #exs_type{value_fun = undefined}, Lexical,
      _Options) ->
    ?l2b(Lexical);
value(NsStack, TypeStack, #exs_type{value_fun = F}, Lexical,
      _Options) ->
    Ctx = #exs_ctx{ns_stack = NsStack, type_stack = TypeStack },
    case catch F(Ctx, Lexical) of
        {'EXIT', _Reason} ->
            throw({failed, cs_error(bad_lexical, Lexical)});
        {error, Reason} ->
            throw({failed, Reason});
        Value ->
            Value
    end.

applicable_facets(_ValidFacets, _Base, []) -> ok;
applicable_facets(ValidFacets, Base, [Facet|Rest]) ->
    FacetName = element(1, Facet),
    case lists:member(FacetName, ValidFacets) of
        true ->
            applicable_facets(ValidFacets, Base, Rest);
        false ->
            throw({failed,
                   cs_error(invalid_facet, {format_type(Base), FacetName})})
    end.

eval_facets(TypeName, Lexical, Value, Facets, Flags) ->
    eval_facets(TypeName, Lexical, Value, Facets, Flags,
                {undefined, undefined}).

eval_facets(TypeName, _Lexical, _Value, [], Flags,
            {{true, EnumValue}, PatternP}) ->
    eval_facets(TypeName, _Lexical, EnumValue, [], Flags, {true, PatternP});
eval_facets(_TypeName, Lexical, _Value, [], _Flags, {false, _}) ->
    throw({failed, cs_error(enumeration, Lexical)});
%% pattern not supported...
%%eval_facets(_TypeName, Lexical, _Value, [], _Flags, {_, {false, []}}) ->
%%    throw({failed, cs_error(pattern, Lexical)});
%%eval_facets(_TypeName, _Lexical, _Value, [], _Flags,
%%            {_, {false, ErrorMessage}}) ->
%%    throw({failed, cs_error(custom_facet_error_message, {ErrorMessage, []})});
eval_facets(_TypeName, Lexical, Value, [], _Flags, _State) ->
    {Lexical, Value};
%% enumeration
eval_facets(TypeName, Lexical, Value,
            [#enumeration{value = Value, hash_value = HashValue}|Rest],
            Flags, {_EnumP, PatternP})
  when is_binary(Value), HashValue /= undefined  ->
    eval_facets(TypeName, Lexical, Value, Rest, Flags,
                {{true, {?C_ENUM_VALUE, HashValue}}, PatternP});
eval_facets(TypeName, Lexical, Value, [#enumeration{value = Value}|Rest],
            Flags, {_EnumP, PatternP}) ->
    eval_facets(TypeName, Lexical, Value, Rest, Flags, {true, PatternP});
eval_facets(TypeName, Lexical, Value, [#enumeration{}|Rest], Flags,
            {undefined, PatternP}) ->
    eval_facets(TypeName, Lexical, Value, Rest, Flags, {false, PatternP});
eval_facets(TypeName, Lexical, Value, [#enumeration{}|Rest], Flags, State) ->
    eval_facets(TypeName, Lexical, Value, Rest, Flags, State);
%% length
eval_facets(TypeName, Lexical, Value,
            [#length{value = Expr, error_message = ErrorMessage,
                     error_app_tag = ErrorAppTag}|Rest], Flags,
            State) ->
    case check_length(Value, Expr) of
        ok ->
            eval_facets(TypeName, Lexical, Value, Rest, Flags, State);
        mismatch when ErrorMessage == [], ErrorAppTag == [] ->
            throw({failed, cs_error(length, Lexical)});
        mismatch ->
            throw({failed, cs_error(custom_facet_error_message,
                                    {ErrorMessage, ErrorAppTag})})
    end;
%% uniqueList
eval_facets(TypeName, Lexical, Value, [#unique_list{}|Rest], Flags, State)
  when is_list(Value) ->
    case is_unique(Value) of
        true ->
            eval_facets(TypeName, Lexical, Value, Rest, Flags, State);
        false ->
            throw({failed, cs_error(uniqueList, Lexical)})
    end;
%% maxLength
eval_facets(TypeName, Lexical, Value,
            [#max_length{value = N, error_message = ErrorMessage,
                        error_app_tag = ErrorAppTag}|Rest], Flags,
            State)
  when is_binary(Value) ->
    if
        size(Value) > N, ErrorMessage == [], ErrorAppTag == [] ->
            throw({failed, cs_error(max_length, {Lexical, N})});
        size(Value) > N ->
            throw({failed,
                   cs_error(custom_facet_error_message,
                            {ErrorMessage, ErrorAppTag})});
        true ->
            eval_facets(TypeName, Lexical, Value, Rest, Flags, State)
    end;
eval_facets(TypeName, Lexical, Value,
            [#max_length{value = N, error_message = ErrorMessage,
                        error_app_tag = ErrorAppTag}|Rest], Flags,
            State)
  when is_atom(Value) ->
    String = ?a2l(Value),
    if
        length(String) > N, ErrorMessage == [], ErrorAppTag == [] ->
            throw({failed, cs_error(max_length, {Lexical, N})});
        length(String) > N ->
            throw({failed,
                   cs_error(custom_facet_error_message,
                            {ErrorMessage, ErrorAppTag})});
        true ->
            eval_facets(TypeName, Lexical, Value, Rest, Flags, State)
    end;
eval_facets(TypeName, Lexical, {?C_BINARY, Value1} = Value,
            [#max_length{value = N, error_message = ErrorMessage,
                        error_app_tag = ErrorAppTag}|Rest], Flags,
            State) ->
    if
        size(Value1) > N, ErrorMessage == [], ErrorAppTag == [] ->
            throw({failed, cs_error(max_length, {Lexical, N})});
        size(Value1) > N ->
            throw({failed,
                   cs_error(custom_facet_error_message,
                            {ErrorMessage, ErrorAppTag})});
        true ->
            eval_facets(TypeName, Lexical, Value, Rest, Flags, State)
    end;
eval_facets(TypeName, Lexical, {?C_OID, Value1} = Value,
            [#max_length{value = N, error_message = ErrorMessage,
                         error_app_tag = ErrorAppTag}|Rest], Flags,
            State) ->
    if
        size(Value1) div 4 > N, ErrorMessage == [], ErrorAppTag == [] ->
            throw({failed, cs_error(max_length, {Lexical, N})});
        size(Value1) div 4 > N ->
            throw({failed,
                   cs_error(custom_facet_error_message,
                            {ErrorMessage, ErrorAppTag})});
        true ->
            eval_facets(TypeName, Lexical, Value, Rest, Flags, State)
    end;
eval_facets(TypeName, Lexical, Value,
            [#max_length{value = N, error_message = ErrorMessage,
                         error_app_tag = ErrorAppTag}|Rest], Flags,
            State) ->
    if
        length(Value) > N, ErrorMessage == [], ErrorAppTag == [] ->
            throw({failed, cs_error(max_length, {Lexical, N})});
        length(Value) > N ->
            throw({failed,
                   cs_error(custom_facet_error_message,
                            {ErrorMessage, ErrorAppTag})});
        true ->
            eval_facets(TypeName, Lexical, Value, Rest, Flags, State)
    end;
%% minLength
eval_facets(TypeName, Lexical, Value,
            [#min_length{value = N, error_message = ErrorMessage,
                         error_app_tag = ErrorAppTag}|Rest], Flags,
            State)
  when is_binary(Value) ->
    if
        size(Value) < N, ErrorMessage == [], ErrorAppTag == [] ->
            throw({failed, cs_error(min_length, {Lexical, N})});
        size(Value) < N ->
            throw({failed,
                   cs_error(custom_facet_error_message,
                            {ErrorMessage, ErrorAppTag})});
        true ->
            eval_facets(TypeName, Lexical, Value, Rest, Flags, State)
    end;
eval_facets(TypeName, Lexical, Value,
            [#min_length{value = N, error_message = ErrorMessage,
                         error_app_tag = ErrorAppTag}|Rest], Flags,
            State)
  when is_atom(Value) ->
    String = ?a2l(Value),
    if
        length(String) < N, ErrorMessage == [], ErrorAppTag == [] ->
            throw({failed, cs_error(min_length, {Lexical, N})});
        length(String) < N ->
            throw({failed,
                   cs_error(custom_facet_error_message,
                            {ErrorMessage, ErrorAppTag})});
        true ->
            eval_facets(TypeName, Lexical, Value, Rest, Flags, State)
    end;
eval_facets(TypeName, Lexical, {?C_BINARY, Value1} = Value,
            [#min_length{value = N, error_message = ErrorMessage,
                         error_app_tag = ErrorAppTag}|Rest], Flags,
            State) ->
    if
        size(Value1) < N, ErrorMessage == [], ErrorAppTag == [] ->
            throw({failed, cs_error(min_length, {Lexical, N})});
        size(Value1) < N ->
            throw({failed,
                   cs_error(custom_facet_error_message,
                            {ErrorMessage, ErrorAppTag})});
        true ->
            eval_facets(TypeName, Lexical, Value, Rest, Flags, State)
    end;
eval_facets(TypeName, Lexical, {?C_OID, Value1} = Value,
            [#min_length{value = N, error_message = ErrorMessage,
                        error_app_tag = ErrorAppTag}|Rest], Flags,
            State) ->
    if
        size(Value1) div 4 < N, ErrorMessage == [], ErrorAppTag == [] ->
            throw({failed, cs_error(min_length, {Lexical, N})});
        size(Value1) div 4 < N ->
            throw({failed,
                   cs_error(custom_facet_error_message,
                            {ErrorMessage, ErrorAppTag})});
        true ->
            eval_facets(TypeName, Lexical, Value, Rest, Flags, State)
    end;
eval_facets(TypeName, Lexical, Value,
            [#min_length{value = N, error_message = ErrorMessage,
                        error_app_tag = ErrorAppTag}|Rest], Flags,
            State) ->
    if
        length(Value) < N, ErrorMessage == [], ErrorAppTag == [] ->
            throw({failed, cs_error(min_length, {Lexical, N})});
        length(Value) < N ->
            throw({failed,
                   cs_error(custom_facet_error_message,
                            {ErrorMessage, ErrorAppTag})});
        true ->
            eval_facets(TypeName, Lexical, Value, Rest, Flags, State)
    end;
%% pattern
%% pattern not supported...
eval_facets(TypeName, Lexical, Value,
            [#pattern{}|Rest], Flags,
            {EnumP, _PatternP}) ->
    eval_facets(TypeName, Lexical, Value, Rest, Flags, {EnumP, true});
eval_facets(TypeName, Lexical, Value,
            [#display_hint{}|Rest], Flags,
            {EnumP, _PatternP}) ->
    eval_facets(TypeName, Lexical, Value, Rest, Flags, {EnumP, true});
%%eval_facets(TypeName, Lexical, Value,
%%            [#pattern{value = Regex, error_message = ErrorMessage,
%%                      error_app_tag = ErrorAppTag}|Rest], Flags,
%%            {EnumP, PatternP}) ->
%%    IsYang = ?bit_is_set(Flags, ?F_YANG_PATTERN),
%%    Match = case IsYang of
%%                true ->
%%                    w3cregex:cmatchp(Regex, Lexical);
%%                false ->
%%                    pregex:cmatchp(["^(", Regex, ")$"], Lexical)
%%            end,
%%    case Match of
%%      true ->
%%          eval_facets(TypeName, Lexical, Value, Rest, Flags, {EnumP, true});
%%      false when IsYang, ErrorMessage == [] ->
%%            %% YANG patterns are ANDed - fail immediately
%%            throw({failed, cs_error(pattern, Lexical)});
%%      false when IsYang ->
%%            %% YANG patterns are ANDed - fail immediately
%%            throw({failed, cs_error(custom_facet_error_message,
%%                                    {ErrorMessage, ErrorAppTag})});
%%      false when PatternP == undefined ->
%%          eval_facets(TypeName, Lexical, Value, Rest, Flags,
%%                        {EnumP, {false, ErrorMessage}});
%%      false when PatternP == true ->
%%          eval_facets(TypeName, Lexical, Value, Rest, Flags, {EnumP, true});
%%      false when element(1, PatternP) == false ->
%%          eval_facets(TypeName, Lexical, Value, Rest, Flags,
%%                        {EnumP, {false, ErrorMessage}});
%%      {error, _Reason} ->
%%            throw({failed, cs_error(invalid_regex, Regex)})
%%    end;
%% maxExclusive
eval_facets(_TypeName, Lexical, Value,
            [#max_exclusive{value = N, error_message = []}|_], _Flags, _State)
  when Value >= N ->
    throw({failed, cs_error(max_exclusive, Lexical)});
eval_facets(_TypeName, _Lexical, Value,
            [#max_exclusive{value = N, error_message = ErrorMessage,
                           error_app_tag = ErrorAppTag}|_], _Flags,
            _State)
  when Value >= N ->
    throw({failed, cs_error(custom_facet_error_message,
                            {ErrorMessage, ErrorAppTag})});
eval_facets(TypeName, Lexical, Value, [#max_exclusive{value = _}|Rest], Flags,
            State) ->
    eval_facets(TypeName, Lexical, Value, Rest, Flags, State);
%% maxInclusive
eval_facets(_TypeName, Lexical, Value,
            [#max_inclusive{value = N, error_message = []}|_], _Flags, _State)
  when Value > N ->
    throw({failed, cs_error(max_inclusive, Lexical)});
eval_facets(_TypeName, _Lexical, Value,
            [#max_inclusive{value = N, error_message = ErrorMessage,
                            error_app_tag = ErrorAppTag}|_], _Flags,
            _State)
  when Value > N ->
    throw({failed, cs_error(custom_facet_error_message,
                            {ErrorMessage, ErrorAppTag})});
eval_facets(TypeName, Lexical, Value, [#max_inclusive{value = _}|Rest], Flags,
            State) ->
    eval_facets(TypeName, Lexical, Value, Rest, Flags, State);
%% minExclusive
eval_facets(_TypeName, Lexical, Value,
            [#min_exclusive{value = N, error_message = []}|_], _Flags, _State)
  when Value =< N ->
    throw({failed, cs_error(min_exclusive, Lexical)});
eval_facets(_TypeName, _Lexical, Value,
            [#min_exclusive{value = N, error_message = ErrorMessage,
                            error_app_tag = ErrorAppTag}|_], _Flags,
            _State)
  when Value =< N ->
    throw({failed, cs_error(custom_facet_error_message,
                            {ErrorMessage, ErrorAppTag})});
eval_facets(TypeName, Lexical, Value, [#min_exclusive{value = _}|Rest], Flags,
            State) ->
    eval_facets(TypeName, Lexical, Value, Rest, Flags, State);
%% minInclusive
eval_facets(_TypeName, Lexical, Value,
            [#min_inclusive{value = N, error_message = []}|_], _Flags, _State)
  when Value < N ->
    throw({failed, cs_error(min_inclusive, Lexical)});
eval_facets(_TypeName, _Lexical, Value,
            [#min_inclusive{value = N, error_message = ErrorMessage,
                           error_app_tag = ErrorAppTag}|_], _Flags,
            _State)
  when Value < N ->
    throw({failed, cs_error(custom_facet_error_message,
                            {ErrorMessage, ErrorAppTag})});
eval_facets(TypeName, Lexical, Value, [#min_inclusive{value = _}|Rest], Flags,
            State) ->
    eval_facets(TypeName, Lexical, Value, Rest, Flags, State);
%% totalDigits
eval_facets(TypeName, Lexical, Value,
            [#total_digits{value = N, error_message = ErrorMessage,
                           error_app_tag = ErrorAppTag}|Rest],
            Flags, State) ->
    Lexical1 = lists:dropwhile(fun($0) -> true;
                                  ($-) -> true;
                                  ($+) -> true;
                                  (_) -> false
                               end, Lexical),
    case lists:member($., Lexical1) of
        true ->
            Lexical2 = lists:dropwhile(fun($0) -> true;
                                          (_) -> false
                                       end, lists:reverse(Lexical1));
        false ->
            Lexical2 = Lexical1
    end,
    Digits = count_digits(Lexical2),
    if
        Digits > N, ErrorMessage == [], ErrorAppTag == [] ->
            throw({failed, cs_error(total_digits, {Lexical, N})});
        Digits > N ->
            throw({failed,
                   cs_error(custom_facet_error_message,
                            {ErrorMessage, ErrorAppTag})});
        true ->
            eval_facets(TypeName, Lexical, Value, Rest, Flags, State)
    end;
%% fractionDigits
eval_facets(TypeName, Lexical, Value,
            [#fraction_digits{value = N, error_message = ErrorMessage,
                              error_app_tag = ErrorAppTag}|Rest],
            Flags, State) ->
    %% We do this stuff based on the lexical value instead on the value
    %% space. The reason for this is that we don't have infinte precision.
    Decimals = count_decimals(
                 lists:dropwhile(fun($0) -> true;
                                    (_) -> false
                                 end, lists:reverse(Lexical))),
    if
        Decimals > N, ErrorMessage == [], ErrorAppTag == [] ->
            throw({failed,
                   cs_error(fraction_digits, {Lexical, N})});
        Decimals > N ->
            throw({failed,
                   cs_error(custom_facet_error_message,
                            {ErrorMessage, ErrorAppTag})});
        true ->
            eval_facets(TypeName, Lexical, Value, Rest, Flags, State)
    end;
%% whiteSpace
eval_facets(TypeName, Lexical, Value,
            [#white_space{value = preserve}|Rest], Flags, State) ->
    eval_facets(TypeName, Lexical, Value, Rest, Flags, State);
eval_facets(TypeName, Lexical, _Value,
            [#white_space{value = replace}|Rest], Flags, State) ->
    Lexical2 = ws_replace(Lexical),
    eval_facets(TypeName, Lexical2, ?l2b(Lexical2), Rest, Flags, State);
eval_facets(TypeName, Lexical, _Value,
            [#white_space{value = collapse}|Rest], Flags, State) ->
    Lexical2 = ws_collapse(Lexical),
    eval_facets(TypeName, Lexical2, ?l2b(Lexical2), Rest, Flags, State);
%% ignoreFacet (internal)
eval_facets(TypeName, Lexical, Value, [#ignore_facet{}|Rest], Flags, State) ->
    eval_facets(TypeName, Lexical, Value, Rest, Flags, State);
%% range YANG facet
eval_facets(TypeName, Lexical, Value,
            [#range_facet{value = Expr, error_message = ErrorMessage,
                          error_app_tag = ErrorAppTag, step = Step}|Rest],
            Flags, State) ->
    case eval_range_expr(Value, Expr, {hd(Expr), Step}) of
        ok ->
            eval_facets(TypeName, Lexical, Value, Rest, Flags, State);
        mismatch when ErrorMessage == [], ErrorAppTag == [] ->
            throw({failed, cs_error(range, Lexical)});
        mismatch ->
            throw({failed, cs_error(custom_facet_error_message,
                                    {ErrorMessage, ErrorAppTag})})
    end.

eval_range_expr(_Value, [], _Step) ->
    mismatch;
eval_range_expr(Value, [{single, min}|Rest], Step) -> % ignore
    eval_range_expr(Value, Rest, Step);
eval_range_expr(Value, [{single, max}|Rest], Step) -> % ignore
    eval_range_expr(Value, Rest, Step);
eval_range_expr(_Value, [{min, max}|_Rest], _Step) -> % ignore
    ok;
eval_range_expr(Value, [{min, MaxValue}|_Rest], Step)
  when Value =< MaxValue ->
    eval_range_step(Value, Step);
eval_range_expr(Value, [{MinValue, max}|_Rest], Step)
  when Value >= MinValue ->
    eval_range_step(Value, Step);
eval_range_expr(Value, [{single, Value}|_Rest], Step) ->
    eval_range_step(Value, Step);
eval_range_expr(Value, [{single, _AnotherValue}|Rest], Step) ->
    eval_range_expr(Value, Rest, Step);
eval_range_expr(Value, [{MinValue, MaxValue}|_Rest], Step)
  when Value >= MinValue, Value =< MaxValue ->
    eval_range_step(Value, Step);
eval_range_expr(Value, [_|Rest], Step) ->
    eval_range_expr(Value, Rest, Step).

eval_range_step(_Value, {_FirstValue, 0}) -> ok;
eval_range_step(_Value, {_FirstValue, 0.0}) -> ok;
eval_range_step(_Value, {min, _N}) -> ok; % ignore
eval_range_step(_Value, {max, _N}) -> ok; % ignore
eval_range_step({Type, Value}, {{single, {Type, FirstValue}}, N}) ->
    eval_range_step_calc(Type, Value, FirstValue, N);
eval_range_step({Type, Value}, {{{Type, FirstValue}, _MaxValue}, N}) ->
    eval_range_step_calc(Type, Value, FirstValue, N);
eval_range_step({Type, Value}, {{Type, FirstValue}, N}) ->
    eval_range_step_calc(Type, Value, FirstValue, N).

eval_range_step_calc(?C_DECIMAL64,
                     {Value, FractionDigits},
                     {FirstValue, FractionDigits}, N) ->
    NN = N*math:pow(10, FractionDigits),
    eval_range_step_calc(undefined, Value, FirstValue, NN);
eval_range_step_calc(_Type, Value, FirstValue, N) ->
    case (Value-FirstValue) rem trunc(N) of
        0 ->
            ok;
        _ ->
            mismatch
    end.

check_length({?C_OID, Value}, Expr) ->
    eval_range_expr(size(Value) div 4, Expr, {-1, 0});
check_length({?C_BINARY, Value}, Expr) ->
    eval_range_expr(size(Value), Expr, {-1, 0});
check_length(Value, Expr) when is_binary(Value) ->
    eval_range_expr(size(Value), Expr, {-1, 0});
check_length(Value, Expr) when is_atom(Value) ->
    eval_range_expr(length(?a2l(Value)), Expr, {-1, 0});
check_length(Value, Expr) ->
    eval_range_expr(length(Value), Expr, {-1, 0}).

get_item_values(_NsStack, _TraverseP, _VerifyVal, _Options, [], _ValidFacets,
                _TypeStack) ->
    [];
get_item_values(NsStack, TraverseP, VerifyVal, Options, [Lexical|Rest],
                ValidFacets,
                [{union, #exs_type{name = TypeName}, MemberTypeStacks}|
                 TypeStack] = TypeStack2) ->
    Value = get_member_value(NsStack, Lexical, TraverseP, verify_hd(VerifyVal),
                             Options, TypeName, MemberTypeStacks),
    %% Check union. May throw an exception.
    get_value(NsStack, Lexical, Value, TraverseP, verify_hd(VerifyVal), Options,
              ?union_facets, TypeStack, TypeStack),
    [Value|get_item_values(NsStack, TraverseP, verify_tl(VerifyVal),
                           Options, Rest, ValidFacets, TypeStack2)];
get_item_values(NsStack, TraverseP, VerifyVal, Options, [Lexical|Rest],
                ValidFacets, TypeStack) ->
    [get_value(NsStack, Lexical, TraverseP, verify_hd(VerifyVal), Options,
               ValidFacets, TypeStack)|
     get_item_values(NsStack, TraverseP, verify_tl(VerifyVal), Options,
                     Rest, ValidFacets, TypeStack)].

get_bits_value(Lexical, TypeName, Fields, 32) ->
    Labels = string:tokens(ws_collapse(Lexical), " "),
    {?C_BIT32, set_bits(Lexical, TypeName, Fields, Labels)};
get_bits_value(Lexical, TypeName, Fields, 64) ->
    Labels = string:tokens(ws_collapse(Lexical), " "),
    {?C_BIT64, set_bits(Lexical, TypeName, Fields, Labels)};
get_bits_value(Lexical, TypeName, Fields, Size) ->
    Labels = string:tokens(ws_collapse(Lexical), " "),
    Bitmask = set_bits(Lexical, TypeName, Fields, Labels),
    Bin = binary:encode_unsigned(Bitmask, little),
    {?C_BITBIG, <<Bin/binary, 0:(Size - size(Bin) * 8)>>}.

set_bits(Lexical, TypeName, Fields,  Labels) ->
    set_bits(Lexical, TypeName, Fields, Labels, 0).

set_bits(_Lexical, _TypeName, _Fields, [], Value) -> Value;
set_bits(Lexical, TypeName, Fields, [Label|Rest], Value) ->
    case lists:keysearch(Label, 2, Fields) of
        {value, {Bit, Label}} ->
            set_bits(Lexical, TypeName, Fields, Rest,
                     set_bit_n(Bit, Value));
        false ->
            throw({failed,
                   cs_error(unknown_bit_label, {Lexical, Label})})
    end.

set_bit_n(Bit, Value) -> (1 bsl Bit) bor Value.

count_decimals(List) -> count_decimals(List, 0).

count_decimals([], _N) -> 0;
count_decimals([$.|_], N) -> N;
count_decimals([_|Rest], N) -> count_decimals(Rest, N+1).

count_digits(List) -> count_digits(List, 0).

count_digits([], N) -> N;
count_digits([$.|Rest], N) -> count_digits(Rest, N);
count_digits([_|Rest], N) -> count_digits(Rest, N+1).

%%% value2string

convert_value(TypeName, Value,
              {atomic, _Flags, _ValidFacets, AtomicTypeStack, []}, FmtType) ->
    get_string(TypeName, Value, AtomicTypeStack, FmtType);
convert_value(TypeName, Value,
              {list, Flags, _ValidFacets, AtomicTypeStack, _ListTypeStack},
              FmtType) ->
    get_items_string(TypeName, Value, Flags, AtomicTypeStack, FmtType);
convert_value(_TypeName, Value, {bits, _Flags, _Type, Fields, Size},
              _FmtType) ->
    get_bits_string(Value, Fields, Size).

get_string(_TypeName, Value, [], _FmtType) ->
    throw({failed, cs_error(bad_value, Value)});
get_string(_TypeName, Value,
           [#exs_type{name = Name, type = {user_defined, Id}}|_], _FmtType) ->
    try
        [#confd_type_cbs{val_to_str = Val2Str}] =
            ets:lookup(confd_type_cbs, Id),
        case Val2Str(undefined, Value) of
            {ok, String} ->
                ?l2b(String);
            {error, Message} ->
                throw({failed, cs_error(user_error, Message)})
        end
    catch
        error:_ ->
            throw({failed, cs_error(string_fun, {Name, Value})})
    end;
get_string(TypeName, {?C_ENUM_VALUE, HashValue},
           [#exs_type{derivation = #restriction{facets = Facets}}|Rest],
           FmtType) ->
    case get_enum_value(HashValue, Facets) of
        {yes, String} -> String;
        no -> get_string(TypeName, {?C_ENUM_VALUE, HashValue}, Rest, FmtType)
    end;
get_string(TypeName, {?C_ENUM_VALUE, HashValue}, [#exs_type{}|Rest], FmtType) ->
    get_string(TypeName, {?C_ENUM_VALUE, HashValue}, Rest, FmtType);
%% Just rely on the root string_fun. It's *not* an enum value.
get_string(_TypeName, Value,
           [#exs_type{name = Name, string_fun = undefined}|_],
           _FmtType) ->
    if
        is_binary(Value) ->
            Value;
        true ->
            throw({failed, cs_error(string_fun, {Name, Value})})
    end;
get_string(_TypeName, Value, [#exs_type{name = Name, string_fun = F}|_] =
           TypeStack, _FmtType) ->
    case catch F(Value, TypeStack) of
        {'EXIT', _Reason} ->
            throw({failed, cs_error(string_fun, {Name, Value})});
        {error, _Reason} ->
            throw({failed, cs_error(string_fun, {Name, Value})});
        String ->
            String
    end;
get_string(_TypeName, Value,
           [{union, #exs_type{name = TypeName}, MemberTypeStacks}|_],
           FmtType) ->
    get_member_string(TypeName, Value, MemberTypeStacks, FmtType).

get_enum_value(_HashValue, []) -> no;
get_enum_value(HashValue,
               [#enumeration{value = String, hash_value = HashValue}|_]) ->
    %% Only strings has hash values, i.e. no need to convert the value.
    {yes, String};
get_enum_value(HashValue, [_|Rest]) ->
    get_enum_value(HashValue, Rest).

get_member_string(_TypeName, Value, [], _FmtType) ->
    throw({failed, cs_error(bad_value, Value)});
get_member_string(TypeName, Value, [TypeStack|Rest], FmtType) ->
    try
        convert_value(TypeName, Value, TypeStack, FmtType)
    catch
        _:_ -> get_member_string(TypeName, Value, Rest, FmtType)
    end.

get_items_string(_TypeName, [], _Flags, _AtomicTypeStack, _FmtType) ->
    <<"">>;
get_items_string(TypeName, [Value], _Flags, AtomicTypeStack, FmtType) ->
    String = get_string(TypeName, Value, AtomicTypeStack, FmtType),
%%    case ?bit_is_set(Flags, ?F_EXS_TYPE_IS_LEAF_LIST) of
%%        true -> ?l2b(qval(String));
%%        false -> String
%%    end;
    String;
get_items_string(TypeName, [Value|Rest], Flags, AtomicTypeStack, FmtType) ->
    String = get_string(TypeName, Value, AtomicTypeStack, FmtType),
    Item =
%%        case ?bit_is_set(Flags, ?F_EXS_TYPE_IS_LEAF_LIST) of
%%            true -> ?l2b(qval(String));
%%            false -> String
%%        end,
        String,
    list_to_binary([Item,
                    <<" ">>,
                    get_items_string(TypeName, Rest, Flags, AtomicTypeStack,
                                     FmtType)]);
get_items_string(_TypeName, BadValue, _Flags, _AtomicTypeStack, _FmtType) ->
    throw({failed, cs_error(bad_value, BadValue)}).

get_bits_string({BType, BValue} = Value, Fields, Size) ->
    case
        case BType of
            ?C_BIT32 when Size == 32 ->
                bits_value2bitmask(Value);
            ?C_BIT64 when Size == 64 ->
                bits_value2bitmask(Value);
            ?C_BITBIG when Size > 64, size(BValue) * 8 =:= Size ->
                bits_value2bitmask(Value);
            _ ->
                error
        end
    of
        Bitmask when is_integer(Bitmask) ->
            try
                ?l2b(string:join(get_bit_labels(Bitmask,Bitmask,Fields), " "))
            catch
                throw:error ->
                    throw({failed, cs_error(bad_value, Value)})
            end;
        _ ->
            throw({failed, cs_error(bad_value, Value)})
    end.

get_bit_labels(0, _Value, _Fields) ->
    [];
get_bit_labels(_Bits, _Value, []) ->
    throw(error);
get_bit_labels(Bits, Value, [{Bit, Label}|Rest]) ->
    case Bits band (bnot (1 bsl Bit)) of
        Bits ->
            get_bit_labels(Bits, Value, Rest);
        RemBits ->
            [Label|get_bit_labels(RemBits, Value, Rest)]
    end.

bits_value2bitmask({?C_BIT32, Integer}) when is_integer(Integer) ->
    Integer;
bits_value2bitmask({?C_BIT64, Integer}) when is_integer(Integer) ->
    Integer;
bits_value2bitmask({?C_BITBIG, Binary}) when is_binary(Binary) ->
    binary:decode_unsigned(Binary, little);
bits_value2bitmask(_) ->
    error.

%%%--------------------------------------------------------------------
%%% Whitespace handling
%%%--------------------------------------------------------------------

ws_collapse(String) -> ws_collapse(ws_replace(String), []).

ws_replace([]) -> [];
ws_replace([$\t|Rest]) -> [$ |ws_replace(Rest)];
ws_replace([$\r|Rest]) -> [$ |ws_replace(Rest)];
ws_replace([$\n|Rest]) ->
    [$ |ws_replace(Rest)];
ws_replace([C|Rest]) -> [C|ws_replace(Rest)].

ws_collapse([], [$ |Acc]) -> reverse(Acc);
ws_collapse([], Acc) -> reverse(Acc);
ws_collapse([$ |Rest], [$ |Acc]) -> ws_collapse(Rest, [$ |Acc]);
ws_collapse([$ |Rest], Acc) -> ws_collapse(Rest, [$ |Acc]);
ws_collapse([C|Rest], Acc) -> ws_collapse(Rest, [C|Acc]).

reverse(String) ->
    case lists:reverse(String) of
        [$ |Rest] -> Rest;
        CollapsedString -> CollapsedString
    end.

%%%

%%%

is_unique(List) ->
    uniq(List) == List.

uniq([H|T]) ->
    [H | uniq(delete_all(H, T))];
uniq([]) ->
    [].

delete_all(X, [X|T]) ->
    delete_all(X, T);
delete_all(X, [H|T]) ->
    [H|delete_all(X,T)];
delete_all(_X, []) ->
    [].


cs_error(bad_value, {_, Value}) ->
    #cs_error{type = bad_value, code = bad_value, opaque = Value};
cs_error(bad_value, Value) ->
    #cs_error{type = bad_value, code = bad_value, opaque = Value};
cs_error(string_fun, {decimal64, {?C_DECIMAL64, {_Value, FractionDigits}}}) ->
    #cs_error{type = bad_value, code = wrong_decimal64_fraction_digits,
              opaque = FractionDigits};
cs_error(string_fun, {Name, Value}) ->
    #cs_error{type = bad_value, code = string_fun,
              opaque = {Name, type_name(Value)}};
cs_error(custom_facet_error_message, {[], []}) ->
    #cs_error{type = bad_value, code = custom_facet_error_message,
              opaque = <<"internal error">>};
cs_error(custom_facet_error_message, {[], AppTag}) ->
    #cs_error{type = bad_value, code = custom_facet_error_message,
              apptag = ?b2a(AppTag), opaque = []};
cs_error(custom_facet_error_message, {Message, []}) ->
    #cs_error{type = bad_value, code = custom_facet_error_message,
              opaque = Message};
cs_error(custom_facet_error_message, {Message, AppTag}) ->
    #cs_error{type = bad_value, code = custom_facet_error_message,
              apptag = ?b2a(AppTag), opaque = Message};
cs_error(Code, Opaque) ->
    #cs_error{type = bad_value, code = Code, opaque = Opaque}.

type_name(T) when is_binary(T) -> string;
type_name(Int) when is_integer(Int) -> int32;
type_name({BType, _Val}) ->
    case BType of
        ?C_INT8 -> int8;
        ?C_INT16 -> int16;
        ?C_INT64 -> int64;
        ?C_UINT8 -> uint8;
        ?C_UINT16 -> uint16;
        ?C_UINT32 -> uint32;
        ?C_UINT64 -> uint64;
        ?C_BIT32 -> bit32;
        ?C_BIT64 -> bit64;
        ?C_ENUM_VALUE -> enum;
        ?C_QNAME -> 'QName';
        ?C_DATETIME -> dateTime;
        ?C_DATE -> date;
        ?C_TIME -> time;
        ?C_DURATION -> duration;
        ?C_OBJECTREF -> objectRef;
        ?C_OID -> oid;
        ?C_BINARY -> binary;
        ?C_IPV4PREFIX -> ipv4Prefix;
        ?C_IPV6PREFIX -> ipv6Prefix;
        ?C_DECIMAL64 -> decimal64;
        ?C_IDENTITYREF -> identityref;
        ?C_DQUAD -> 'dotted-quad';
        ?C_HEXSTR -> 'hex-string';
        ?C_IPV4_AND_PLEN -> 'ipv4-address-and-prefix-length';
        ?C_IPV6_AND_PLEN -> 'ipv6-address-and-prefix-length';
        ?C_BITBIG -> bitbig;
        _ -> unknown
    end;
type_name({_,_,_,_}) -> ipv4;
type_name({_,_,_,_,_,_,_,_}) -> ipv6;
type_name(true) -> boolean;
type_name(false) -> boolean;
type_name(Vals) when is_list(Vals) -> list;
type_name(Float) when is_float(Float) -> float;
type_name(_) -> unknown.


%%%--------------------------------------------------------------------
%%% Schema handler for "builtin" types (Ns = ?W3C_SCHEMA_URI | ?CONFD_URI)
%%%--------------------------------------------------------------------

%% W3C

schema_handler(string) ->
    [#exs_type{name = string,
               type = primitive}];
schema_handler(boolean) ->
    [#exs_type{name = boolean,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun boolean_value/2,
               string_fun = fun boolean_string/2}];
schema_handler(float) ->
    [#exs_type{name = float,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun float_value/2,
               string_fun = fun float_string/2}];
schema_handler(double) ->
    [#exs_type{name = double,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun float_value/2,
               string_fun = fun float_string/2}];
schema_handler(decimal) ->
    [#exs_type{name = decimal,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun decimal_value/2,
               string_fun = fun decimal_string/2}];
schema_handler(hexBinary) ->
    [#exs_type{name = hexBinary,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun hexBinary_value/2,
               string_fun = fun hexBinary_string/2
              }];
schema_handler(base64Binary) ->
    [#exs_type{name = base64Binary,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun base64Binary_value/2,
               string_fun = fun base64Binary_string/2}];
schema_handler(anyURI) ->
    [#exs_type{name = anyURI,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1}];
schema_handler('QName') ->
    %% NYI
    [];
schema_handler('NOTATION') ->
    [#exs_type{name = 'NOTATION',
               type = primitive,
               lexical_value_fun = fun ws_collapse/1}];
schema_handler(normalizedString) ->
    [#exs_type{name = normalizedString,
               type = primitive,
               lexical_value_fun = fun ws_replace/1}];
schema_handler(token) ->
    [#exs_type{name = token,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1}];
schema_handler(language) ->
    [#exs_type{name = language,
               derivation = #restriction{
                 base = token,
                 facets = [#pattern{value =
                       <<"[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*">>}]}}];
schema_handler('IDREFS') ->
    [#exs_type{name = 'IDREFS',
               derivation = #restriction{
                 base = 'IDREF_LIST',
                 facets = [#min_length{value = 1}]
                }}];
schema_handler('IDREF_LIST') ->
    [#exs_type{name = 'IDREF_LIST',
               derivation = #list{
                 item_type = 'IDREF'
                }}];
schema_handler('ENTITIES') ->
    [#exs_type{name = 'ENTITIES',
               derivation = #restriction{
                 base = 'ENTITY_LIST',
                 facets = [#min_length{value = 1}]
                }}];
schema_handler('ENTITY_LIST') ->
    [#exs_type{name = 'ENTITY_LIST',
               derivation = #list{
                 item_type = 'ENTITY'
                }}];
schema_handler('NMTOKEN') ->
    [#exs_type{name = 'NMTOKEN',
               derivation = #restriction{
                 base = token,
                 facets = [#pattern{value = <<"[A-Za-z0-9._:-]+">>}]
                }}];
schema_handler('NMTOKENS') ->
    [#exs_type{name = 'NMTOKENS',
               derivation = #restriction{
                 base = 'NMTOKEN_LIST',
                 facets = [#min_length{value = 1}]
                }}];
schema_handler('NMTOKEN_LIST') ->
    [#exs_type{name = 'NMTOKEN_LIST',
               derivation = #list{
                 item_type = 'NMTOKEN'
                }}];
schema_handler('Name') ->
    [#exs_type{name = 'Name',
               derivation = #restriction{
                 base = atom,
                 facets = [#pattern{value = <<"[A-Za-z_:][A-Za-z0-9._:-]*">>}]
                }}];
schema_handler('NCName') ->
    [#exs_type{name = 'NCName',
               derivation = #restriction{
                 base = 'Name',
                 facets = [#pattern{value = <<"[^:]*">>}]
                }}];
schema_handler('ID') ->
    [#exs_type{name = 'ID',
               derivation = #restriction{
                 base = 'NCName'
                }}];
schema_handler('IDREF') ->
    [#exs_type{name = 'IDREF',
               derivation = #restriction{
                 base = 'NCName'
                }}];
schema_handler('ENTITY') ->
    [#exs_type{name = 'ENTITY',
               derivation = #restriction{
                 base = 'NCName'
                }}];
schema_handler(integer) ->
    [#exs_type{name = integer,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun integer_value/2,
               string_fun = fun integer_string/2}];
schema_handler(nonPositiveInteger) ->
    [#exs_type{name = nonPositiveInteger,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun non_positive_integer_value/2,
               string_fun = fun non_positive_integer_string/2}];
schema_handler(negativeInteger) ->
    [#exs_type{name = negativeInteger,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun negative_integer_value/2,
               string_fun = fun negative_integer_string/2}];
schema_handler(long) ->
    [#exs_type{name = long,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun long_value/2,
               string_fun = fun long_string/2}];
schema_handler(int) ->
    [#exs_type{name = int,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun int_value/2,
               string_fun = fun int_string/2}];
schema_handler(short) ->
    [#exs_type{name = short,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun short_value/2,
               string_fun = fun short_string/2}];
schema_handler(byte) ->
    [#exs_type{name = byte,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun byte_value/2,
               string_fun = fun byte_string/2}];
schema_handler(nonNegativeInteger) ->
    [#exs_type{name = nonNegativeInteger,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun non_negative_integer_value/2,
               string_fun = fun non_negative_integer_string/2}];
schema_handler(positiveInteger) ->
    [#exs_type{name = positiveInteger,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun positive_integer_value/2,
               string_fun = fun positive_integer_string/2}];
schema_handler(unsignedLong) ->
    [#exs_type{name = unsignedLong,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun unsigned_long_value/2,
               string_fun = fun unsigned_long_string/2}];
schema_handler(unsignedInt) ->
    [#exs_type{name = unsignedInt,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun unsigned_int_value/2,
               string_fun = fun unsigned_int_string/2}];
schema_handler(unsignedShort) ->
    [#exs_type{name = unsignedShort,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun unsigned_short_value/2,
               string_fun = fun unsigned_short_string/2}];
schema_handler(unsignedByte) ->
    [#exs_type{name = unsignedByte,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun unsigned_byte_value/2,
               string_fun = fun unsigned_byte_string/2}];
schema_handler(dateTime) ->
    [#exs_type{name = dateTime,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun dateTime_value/2,
               string_fun = fun dateTime_string/2}];
schema_handler(date) ->
    [#exs_type{name = date,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun date_value/2,
               string_fun = fun date_string/2}];
schema_handler(time) ->
    [#exs_type{name = time,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun time_value/2,
               string_fun = fun time_string/2}];
schema_handler(duration) ->
    [#exs_type{name = duration,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun duration_value/2,
               string_fun = fun duration_string/2}];

%% confd

schema_handler('objectRef') ->
    %% NYI (note if implementing: value has ikeypath, not hkeypath)
    [];
schema_handler('Counter32') ->
    [#exs_type{name = 'Counter32',
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun unsigned_int_value/2,
               string_fun = fun unsigned_int_string/2}];
schema_handler('Gauge32') ->
    [#exs_type{name = 'Gauge32',
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun unsigned_int_value/2,
               string_fun = fun unsigned_int_string/2}];
schema_handler('Counter64') ->
    [#exs_type{name = 'Counter64',
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun unsigned_long_value/2,
               string_fun = fun unsigned_long_string/2}];
schema_handler(atom) ->
    [#exs_type{name = atom,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun atom_value/2,
               string_fun = fun atom_string/2}];
schema_handler(inetAddress) ->
    [#exs_type{name = inetAddress,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun inetAddress_value/2,
               string_fun = fun inetAddress_string/2}];
schema_handler(inetAddressIP) ->
    [#exs_type{name = inetAddressIP,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun inetAddressIP_value/2,
               string_fun = fun inetAddressIP_string/2}];
schema_handler(inetAddressIPv4) ->
    [#exs_type{name = inetAddressIPv4,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun inetAddressIPv4_value/2,
               string_fun = fun inetAddressIPv4_string/2}];
schema_handler(inetAddressIPv6) ->
    [#exs_type{name = inetAddressIPv6,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun inetAddressIPv6_value/2,
               string_fun = fun inetAddressIPv6_string/2}];
schema_handler(inetAddressDNS) ->
    [#exs_type{name = inetAddressDNS,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun inetAddressDNS_value/2,
               string_fun = fun inetAddressDNS_string/2}];
schema_handler(inetPortNumber) ->
    [#exs_type{name = inetPortNumber,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun inetPortNumber_value/2,
               string_fun = fun inetPortNumber_string/2}];
schema_handler(ipv4Prefix) ->
    [#exs_type{name = ipv4Prefix,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun ipv4_prefix_value/2,
               string_fun = fun ipv4_prefix_string/2}];
schema_handler(ipv6Prefix) ->
    [#exs_type{name = ipv6Prefix,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun ipv6_prefix_value/2,
               string_fun = fun ipv6_prefix_string/2}];
schema_handler(ipPrefix) ->
    [#exs_type{name = ipPrefix,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun ip_prefix_value/2,
               string_fun = fun ip_prefix_string/2}];
schema_handler(ipv4AddressAndPrefixLength) ->
    [#exs_type{name = ipv4AddressAndPrefixLength,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun ipv4_and_plen_value/2,
               string_fun = fun ipv4_and_plen_string/2}];
schema_handler(ipv6AddressAndPrefixLength) ->
    [#exs_type{name = ipv6AddressAndPrefixLength,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun ipv6_and_plen_value/2,
               string_fun = fun ipv6_and_plen_string/2}];
schema_handler(ipAddressAndPrefixLength) ->
    [#exs_type{name = ipAddressAndPrefixLength,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun ip_and_plen_value/2,
               string_fun = fun ip_and_plen_string/2}];
schema_handler(decimal64) ->
    [#exs_type{name = decimal64,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun decimal64_value/2,
               string_fun = fun decimal64_string/2}];
schema_handler(identityref) ->
    [#exs_type{name = identityref,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun identityref_value/2,
               string_fun = fun identityref_string/2}];
schema_handler(size) ->
    [#exs_type{name = size,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun size_value/2,
               string_fun = fun size_string/2}];
schema_handler(hexValue) ->
    [#exs_type{name = hexValue,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun hexValue_value/2,
               string_fun = fun hexValue_string/2}];
schema_handler(oid) ->
    [#exs_type{name = oid,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun oid_value/2,
               string_fun = fun oid_string/2}];
schema_handler(hexList) ->
    [#exs_type{name = hexList,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun hexList_value/2,
               string_fun = fun hexList_string/2}];
schema_handler(hexString) ->
    [#exs_type{name = hexString,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun hexString_value/2,
               string_fun = fun hexString_string/2}];
schema_handler(octetList) ->
    [#exs_type{name = octetList,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun octetList_value/2,
               string_fun = fun octetList_string/2}];
schema_handler(dottedQuad) ->
    [#exs_type{name = dottedQuad,
               type = primitive,
               lexical_value_fun = fun ws_collapse/1,
               value_fun = fun dottedQuad_value/2,
               string_fun = fun dottedQuad_string/2}];
schema_handler('MD5DigestString') ->
    schema_handler(string);
schema_handler('SHA256DigestString') ->
    schema_handler(string);
schema_handler('SHA512DigestString') ->
    schema_handler(string);
schema_handler(cryptHash) ->
    schema_handler(string);
schema_handler('DES3CBCIVEncryptedString') ->
    schema_handler(string);
schema_handler('AESCFB128IVEncryptedString') ->
    schema_handler(string);

%% YANG

schema_handler(int8) ->
    schema_handler(byte);
schema_handler(int16) ->
    schema_handler(short);
schema_handler(int32) ->
    schema_handler(int);
schema_handler(int64) ->
    schema_handler(long);
schema_handler(uint8) ->
    schema_handler(unsignedByte);
schema_handler(uint16) ->
    schema_handler(unsignedShort);
schema_handler(uint32) ->
    schema_handler(unsignedInt);
schema_handler(uint64) ->
    schema_handler(unsignedLong);
schema_handler(binary) ->
    schema_handler(base64Binary);
schema_handler('instance-identifier') ->
    schema_handler('objectRef');

%% tailf

schema_handler('hex-list') ->
    schema_handler(hexList);
schema_handler('octet-list') ->
    schema_handler(octetList);
schema_handler('md5-digest-string') ->
    schema_handler('MD5DigestString');
schema_handler('sha-256-digest-string') ->
    schema_handler('SHA256DigestString');
schema_handler('sha-512-digest-string') ->
    schema_handler('SHA512DigestString');
schema_handler('des3-cbc-encrypted-string') ->
    schema_handler('DES3CBCIVEncryptedString');
schema_handler('aes-cfb-128-encrypted-string') ->
    schema_handler('AESCFB128IVEncryptedString');

schema_handler(_) -> [].



%%% xs:boolean

boolean_value(_Ctx, "true") -> true;
boolean_value(_Ctx, "false") -> false;
boolean_value(_Ctx, "1") -> true;
boolean_value(_Ctx, "0") -> false.

boolean_string(true, _TypeStack) -> <<"true">>;
boolean_string(false, _TypeStack) -> <<"false">>.

%%% xs:float

float_decimal_value(_Ctx, Mantissa0, Exponent) ->
    Mantissa =
        case Mantissa0 of
            [Sign,$.|Tail] when Sign == $+; Sign == $- ->
                [Sign,$0,$.|Tail];
            [$.|Tail] ->
                [$0,$.|Tail];
            _ ->
                case lists:reverse(Mantissa0) of
                    [$.|RHead] ->
                        lists:reverse([$0,$.|RHead]);
                    Assitnam ->
                        case lists:member($.,Mantissa0) of
                            true ->
                                Mantissa0;
                            false ->
                                lists:reverse([$0,$.|Assitnam])
                        end
                end
        end,

    String =
        case Exponent of
            "0" ->
                Mantissa;
            _ ->
                Mantissa ++ "e" ++ Exponent
        end,

    case catch ?l2f(String) of
        {'EXIT', _} -> float(?l2i(String));
        Value -> Value
    end.

float_value(Ctx, String0) ->
    {Mantissa,Exponent} =
        case string:tokens(String0,"eE") of
            [String0] ->
                {String0,"0"};
            [String1,String2] ->
                {String1,String2}
        end,
    float_decimal_value(Ctx, Mantissa, Exponent).

float_string(Value, _TypeStack) ->
    ?l2b(io_lib:format("~." ?FLOAT_DIGITS "g", [Value])).

%%% xs:decimal

decimal_value(Ctx, String0) ->
    false = lists:member($e, String0),
    false = lists:member($E, String0),
    float_decimal_value(Ctx, String0, "0").

decimal_string(Value, _TypeStack) when Value == 0 -> <<"0.0">>;
decimal_string(Value, TypeStack) ->
    %% The goal here is to a) not produce "float garbage" and b) not produce
    %% more decimals than allowed by restrictions. In case of large numbers
    %% we may violate the totalDigits restriction - no type checking here.
    Sign = if Value < 0 -> "-"; true -> "" end,
    AbsVal = abs(Value),
    DecPos = floor(math:log10(AbsVal)) + 1,
    IntStr = ?i2l(round(AbsVal * math:pow(10, ?DECIMAL_DIGITS - DecPos))),
    {Integer, Fraction0} =
        if
            DecPos >= ?DECIMAL_DIGITS ->
                {IntStr ++ lists:duplicate(DecPos - ?DECIMAL_DIGITS, $0), "0"};
            DecPos =< 0 ->
                {"0", lists:duplicate(-DecPos, $0) ++ IntStr};
            true ->
                lists:split(DecPos, IntStr)
        end,
    {TotalDigits, FractionDigits} =
        get_digits_restriction(TypeStack, 9999, 9999),
    Decimals = lists:min([?DECIMAL_DIGITS - DecPos, TotalDigits,
                          TotalDigits - DecPos, FractionDigits]),
    Fraction = lists:reverse(trim_decimals(Decimals, length(Fraction0),
                                           lists:reverse(Fraction0))),
    FinalSign = case {Integer, Fraction} of
                    {"0", "0"} -> "";
                    _ -> Sign
                end,
    ?l2b([FinalSign, Integer, ".", Fraction]).

get_digits_restriction(TypeStack, Total, Fraction) ->
    FFun = fun(#total_digits{value = TV}, {T, F}) when TV < T -> {TV, F};
              (#fraction_digits{value = FV}, {T, F}) when FV < F -> {T, FV};
              (_Facet, TF) -> TF
           end,
    TFun = fun(#exs_type{type = derived,
                         derivation = #restriction{facets = Facets}}, TF) ->
                   lists:foldl(FFun, TF, Facets);
              (_Type, TF) -> TF
           end,
    lists:foldl(TFun, {Total, Fraction}, TypeStack).

trim_decimals(_N, _Len, []) -> "0";
trim_decimals(N, Len, [_|T]) when Len > N -> trim_decimals(N, Len - 1, T);
trim_decimals(N, Len, "0" ++ T) -> trim_decimals(N, Len - 1, T);
trim_decimals(_N, _Len, F) -> F.

floor(V) when V >= 0 -> trunc(V);
floor(V) -> trunc(V - trunc(V) + 1) + trunc(V) - 1.

%%% xs:hexBinary

hexBinary_value(_Ctx, String) ->
    {?C_BINARY, ?l2b(hex2val(lists:reverse(String), []))}.

hex2val([Lso,Mso|R],Ack) ->
    hex2val(R, [hexdigit(Lso) bor (hexdigit(Mso) bsl 4)|Ack]);
hex2val([Lso],Ack) ->
    [hexdigit(Lso)|Ack];
hex2val([],Ack) ->
    Ack.

hexdigit(H) when H >= $0, H =< $9 -> H-$0;
hexdigit(H) when H >= $a, H =< $f -> H-$a+10;
hexdigit(H) when H >= $A, H =< $F -> H-$A+10.

hexBinary_string({?C_BINARY, Value}, _TypeStack) ->
    ?l2b(val2hex_bin(?b2l(Value))).

val2hex_bin([X|Xs]) ->
    [d2h(X bsr 4),d2h(X band 15)|val2hex_bin(Xs)];
val2hex_bin([]) ->
    [].

d2h(N) when N<10 -> N+$0;
d2h(N) -> N+$A-10.

%%% xs:base64Binary

base64Binary_value(_Ctx, String) ->
    {?C_BINARY, base64:decode(String)}.

base64Binary_string({?C_BINARY, Binary}, _TypeStack) ->
    base64:encode(Binary).

%%% xs:integer

integer_value(_Ctx, String) -> to_integer(?l2i(String)).

to_integer(Value) ->
    true = ((Value >= ?LONG_MIN) andalso (Value =< ?LONG_MAX)),
    {?C_INT64, Value}.

integer_string({?C_INT64, Integer}, _TypeStack) ->
    ?i2b(Integer).

%%% xs:nonPositiveInteger

non_positive_integer_value(_Ctx, String) ->
    to_non_positive_integer(?l2i(String)).

to_non_positive_integer(Value) ->
    true = ((Value >= ?LONG_MIN) andalso (Value =<  0)),
    {?C_INT64, Value}.

non_positive_integer_string(Value, TypeStack) ->
    integer_string(Value, TypeStack).

%%% xs:negativeInteger

negative_integer_value(_Ctx, String) -> to_negative_integer(?l2i(String)).

to_negative_integer(Value) ->
    true = ((Value >= ?LONG_MIN) andalso (Value <  0)),
    {?C_INT64, Value}.

negative_integer_string(Value, TypeStack) ->
    integer_string(Value, TypeStack).

%%% xs:long

long_value(Ctx, String) -> integer_value(Ctx, String).

long_string(Value, TypeStack) ->
    integer_string(Value, TypeStack).

%%% xs:int

int_value(_Ctx, String) -> to_int(?l2i(String)).

to_int(Value) ->
    true = ((Value >= ?INT_MIN) andalso (Value =< ?INT_MAX)),
    Value.

int_string(Integer, _TypeStack) -> ?i2b(Integer).

%%% xs:short

short_value(_Ctx, String) -> to_short(?l2i(String)).

to_short(Value) ->
    true = ((Value >= ?SHRT_MIN) andalso (Value =< ?SHRT_MAX)),
    {?C_INT16, Value}.

short_string({?C_INT16, Integer}, _TypeStack) ->
    ?i2b(Integer).

%%% xs:byte

byte_value(_Ctx, String) -> to_byte(?l2i(String)).

to_byte(Value) ->
    true = ((Value >= ?BYTE_MIN) andalso (Value =< ?BYTE_MAX)),
    {?C_INT8, Value}.

byte_string({?C_INT8, Integer}, _TypeStack) ->
    ?i2b(Integer).

%%% xs:nonNegativeInteger

non_negative_integer_value(_Ctx, String) ->
    to_non_negative_integer(?l2i(String)).

to_non_negative_integer(Value) ->
    true = ((Value >=  0) andalso (Value =< ?ULONG_MAX)),
    {?C_UINT64, Value}.

non_negative_integer_string({?C_UINT64, Integer}, _TypeStack) ->
    ?i2b(Integer).

%%% xs:positiveInteger

positive_integer_value(_Ctx, String) -> to_positive_integer(?l2i(String)).

to_positive_integer(Value) ->
    true = ((Value > 0) andalso (Value =< ?ULONG_MAX)),
    {?C_UINT64, Value}.

positive_integer_string(Value, TypeStack) ->
    non_negative_integer_string(Value, TypeStack).

%%% xs:unsignedLong

unsigned_long_value(_Ctx, String) -> to_unsigned_long(?l2i(String)).

to_unsigned_long(Value) ->
    true = ((Value >= ?ULONG_MIN) andalso (Value =< ?ULONG_MAX)),
    {?C_UINT64, Value}.

unsigned_long_string(Value, TypeStack) ->
    non_negative_integer_string(Value, TypeStack).

%%% xs:unsignedInt

unsigned_int_value(_Ctx, String) -> to_unsigned_int(?l2i(String)).

to_unsigned_int(Value) ->
    true = ((Value >= ?UINT_MIN) andalso (Value =< ?UINT_MAX)),
    {?C_UINT32, Value}.

unsigned_int_string({?C_UINT32, Integer}, _TypeStack) ->
    ?i2b(Integer).

%%% xs:unsignedShort

unsigned_short_value(_Ctx, String) -> to_unsigned_short(?l2i(String)).

to_unsigned_short(Value) ->
    true = ((Value >= ?USHRT_MIN) andalso (Value =< ?USHRT_MAX)),
    {?C_UINT16, Value}.

unsigned_short_string({?C_UINT16, Integer}, _TypeStack) ->
    ?i2b(Integer).

%%% xs:unsignedByte

unsigned_byte_value(_Ctx, String) -> to_unsigned_byte(?l2i(String)).

to_unsigned_byte(Value) ->
    true = ((Value >= ?UBYTE_MIN) andalso (Value =< ?UBYTE_MAX)),
    {?C_UINT8, Value}.

unsigned_byte_string({?C_UINT8, Integer}, _TypeStack) ->
    ?i2b(Integer).

%%% yang:date-and-time (point in time)

dateTime_value(_Ctx,
                [Y0, Y1, Y2, Y3, $-, M0, M1, $-, D0, D1, $T,
                 H0, H1, $:, Mi0, Mi1, $:, S0, S1|Rest]) ->
    Year = ?l2i([Y0, Y1, Y2, Y3]),
    Month =  ?l2i([M0, M1]),
    Day =  ?l2i([D0, D1]),
    Hour =  ?l2i([H0, H1]),
    Min =  ?l2i([Mi0, Mi1]),
    Sec =  ?l2i([S0, S1]),
    {Us, Cont} = us(Rest),
    {TZHour, TZMin} = timezone(Cont),
    to_dateTime({Year, Month, Day, Hour, Min, Sec, Us, TZHour, TZMin}).

to_dateTime({Year, Month, Day, Hour, Min, Sec, _Us, _TZHour, _TZMin} =
            Value) ->
    true = calendar:valid_date(Year, Month, Day),
    true = valid_time(Hour, Min, Sec),
    {?C_DATETIME, Value}.

us([$.|Rest]) -> extract_us(Rest);
us(List) -> {0, List}.

extract_us(Digits) -> extract_us(Digits, []).

extract_us([], Fraction) ->
    {fraction2us(Fraction), []};
extract_us([$+|Rest], Fraction) -> %% Start of a timezone
    {fraction2us(Fraction), [$+|Rest]};
extract_us([$-|Rest], Fraction) -> %% Start of a timezone
    {fraction2us(Fraction), [$-|Rest]};
extract_us([$Z|Rest], Fraction) -> %% End of secs in xs:duration
    {fraction2us(Fraction), [$Z|Rest]};
extract_us([$S|Rest], Fraction) -> %% End of secs in xs:duration
    {fraction2us(Fraction), [$S|Rest]};
extract_us([$s|Rest], Fraction) -> %% End of secs in xs:duration
    {fraction2us(Fraction), [$S|Rest]};
extract_us([N|Rest], Fraction) when N >= $0, N =< $9 ->
    extract_us(Rest, [N|Fraction]).

fraction2us(Fraction) ->
    ?l2i(lists:sublist(lists:reverse([$0,$0,$0,$0,$0,$0|Fraction]), 6)).

timezone([]) -> {[], 0};
timezone("-00:00") -> {[], 0};
timezone([$Z]) -> {0, 0};
timezone([$+, H0, H1, $:, M0, M1]) ->
    Hours = ?l2i([H0, H1]),
    true = (Hours =< 14),
    {Hours, ?l2i([M0, M1])};
timezone([$-, H0, H1, $:, M0, M1]) ->
    Hours = -(?l2i([H0, H1])),
    true = (Hours >= -14),
    if Hours =:= 0 ->
            {Hours, -(?l2i([M0, M1]))};
       true ->
            {Hours, ?l2i([M0, M1])}
    end.

valid_time(Hour, Min, Sec) ->
    ((Hour >= 0) andalso (Hour =< 23) andalso
     (Min >= 0) andalso (Min =< 59) andalso
     (Sec >= 0) andalso (Sec =< 60))
        or
          (Hour == 24 andalso Min == 0 andalso Sec == 0).

year_string(Year, _TypeStack) ->
    if
        Year < 0 ->
            io_lib:format("-~4.4.0w", [-Year]);
        true ->
            io_lib:format("~4.4.0w", [Year])
    end.

dateTime_string({?C_DATETIME,
                 {Year, Month, Day, Hour, Min, Sec, Us, TZHour, TZMin}},
                TypeStack) ->
    ?l2b([year_string(Year, TypeStack),
          "-",
          io_lib:format("~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0w",
                        [Month, Day, Hour, Min, Sec]),
          case Us of
              0 -> "";
              _ -> us2fraction(Us)
          end,
          dt_tz_string(TZHour, TZMin)]).

us2fraction(Us) ->
    string:strip(
      tl(lists:flatten(io_lib:format("~f", [Us/1000000.0]))), right, $0).

% yang:date-and-time /= xs:dateTime
dt_tz_string([], _) -> "-00:00";
dt_tz_string(0, 0) -> "+00:00";
dt_tz_string(TZHour, TZMin) -> fmt_tz_string(TZHour, TZMin).

tz_string([], _) -> [];
tz_string(0, 0) -> "Z";
tz_string(TZHour, TZMin) -> fmt_tz_string(TZHour, TZMin).

fmt_tz_string(TZHour, TZMin) when TZHour > 0, TZMin >= 0 ->
    io_lib:format("+~2.2.0w:~2.2.0w", [TZHour, TZMin]);
fmt_tz_string(TZHour, TZMin) when TZHour =:= 0, TZMin < 0 ->
    io_lib:format("-~2.2.0w:~2.2.0w", [TZHour, -TZMin]);
fmt_tz_string(TZHour, TZMin) ->
    io_lib:format("-~2.2.0w:~2.2.0w", [-TZHour, TZMin]).

%%% xs:date (period in time)

date_value(_Ctx, [$+|Rest]) -> date_value(1, _Ctx, Rest);
date_value(_Ctx, [$-|Rest]) -> date_value(-1, _Ctx, Rest);
date_value(_Ctx, List) -> date_value(1, _Ctx, List).

date_value(Sign, _Ctx, [Y0, Y1, Y2, Y3, $-, M0, M1, $-, D0, D1|Rest]) ->
    Year = ?l2i([Y0, Y1, Y2, Y3])*Sign,
    Month =  ?l2i([M0, M1]),
    Day =  ?l2i([D0, D1]),
    {TZHour, TZMin} = timezone(Rest),
    to_date({Year, Month, Day, TZHour, TZMin}).

to_date({Year, Month, Day, _TZHour, _TZMin} = Value) ->
    true = calendar:valid_date(abs(Year), Month, Day), %% duh!
    {?C_DATE, Value}.

date_string({?C_DATE, {Year, Month, Day, TZHour, TZMin}}, TypeStack) ->
    ?l2b([year_string(Year, TypeStack),
          "-",
          io_lib:format("~2.2.0w", [Month]),
          "-",
          io_lib:format("~2.2.0w", [Day]),
          tz_string(TZHour, TZMin)]).

%%% xs:time (recurring point in time)

time_value(_Ctx, [$+|Rest]) -> time_value(_Ctx, Rest);
time_value(_Ctx, [H0, H1, $:, Mi0, Mi1, $:, S0, S1|Rest]) ->
    Hour =  ?l2i([H0, H1]),
    Min = ?l2i([Mi0, Mi1]),
    Sec = ?l2i([S0, S1]),
    {Us, Cont} = us(Rest),
    {TZHour, TZMin} = timezone(Cont),
    to_time({Hour, Min, Sec, Us, TZHour, TZMin}).

to_time({Hour, Min, Sec, _Us, _TZHour, _TZMin} = Value) ->
    true = valid_time(Hour, Min, Sec),
    {?C_TIME, Value}.

time_string({?C_TIME, {Hour, Min, Sec, Us, TZHour, TZMin}}, _TypeStack) ->
    ?l2b([io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w", [Hour, Min, Sec]),
          case Us of
              0 -> "";
              _ -> us2fraction(Us)
          end,
          tz_string(TZHour, TZMin)]).

%%% xs:duration (period of time)

duration_value(_Ctx, [$P|Rest]) ->
    {Year, Cont1} = year(Rest),
    {Month, Cont2} = month(Cont1),
    {Day, Cont3} = day(Cont2),
    duration_value(Year, Month, Day, Cont3);
duration_value(_Ctx, Rest) -> %% P is optional
    {Year, Cont1} = year(Rest),
    {Month, Cont2} = month(Cont1),
    {Day, Cont3} = day(Cont2),
    duration_value(Year, Month, Day, Cont3).

duration_value(Year, Month, Day, []) ->
    to_duration({Year, Month, Day, 0, 0, 0, 0});
duration_value(Year, Month, Day, [$T|Rest]) ->
    {Hour, Cont} = hour(Rest),
    {Min, Cont2} = min(Cont),
    {Sec, Us, []} = sec(Cont2),
    to_duration({Year, Month, Day, Hour, Min, Sec, Us});
duration_value(Year, Month, Day, Rest) -> %% T is optional
    {Hour, Cont} = hour(Rest),
    {Min, Cont2} = min(Cont),
    {Sec, Us, []} = sec(Cont2),
    to_duration({Year, Month, Day, Hour, Min, Sec, Us}).

year(List) -> get_digits("Yy", List).

month(List) -> get_digits("M", List).

day(List) -> get_digits("Dd", List).

hour(List) -> get_digits("Hh", List).

min(List) -> get_digits("Mm", List).

sec(List) ->
    case get_digits("Ss", List) of
        {0, List} ->
            case get_digits(".", List) of
                {0, List} -> {0, 0, List};
                {Sec, Cont} ->
                    case us([$.|Cont]) of
                        {Us, [S|Cont2]} when S == $S ; S == $s ->
                            {Sec, Us, Cont2}
                    end
            end;
        {Sec, Cont} -> {Sec, 0, Cont}
    end.

get_digits([], List) ->
    {0, List};
get_digits([Token|Tokens], List) ->
    case get_digits(Token, List, []) of
        {_, List} ->
            get_digits(Tokens, List);
        Res ->
            Res
    end.

get_digits(Token, [C|Rest], Acc) when C >= $0, C =< $9 ->
    get_digits(Token, Rest, [C|Acc]);
get_digits(Token, [Token|Rest], Acc) -> {?l2i(lists:reverse(Acc)), Rest};
get_digits(_Token, List, Acc) -> {0, lists:reverse(Acc)++List}.

to_duration({Year, Month, Day, Hour, Min, Sec, Us} = Value) ->
    true = (((Year >= ?UINT_MIN) andalso (Year =< ?UINT_MAX)) andalso
            ((Month >= ?UINT_MIN) andalso (Month =< ?UINT_MAX)) andalso
            ((Day >= ?UINT_MIN) andalso (Day =< ?UINT_MAX)) andalso
            ((Hour >= ?UINT_MIN) andalso (Hour =< ?UINT_MAX)) andalso
            ((Min >= ?UINT_MIN) andalso (Min =< ?UINT_MAX)) andalso
            ((Sec >= ?UINT_MIN) andalso (Sec =< ?UINT_MAX)) andalso
            ((Us >= ?UINT_MIN) andalso (Us =< ?UINT_MAX))),
    {?C_DURATION, Value}.

duration_string({?C_DURATION, {Year, Month, Day, 0, 0, 0, 0}}, _TypeStack) ->
    D = ?l2b([$P, format_value(Year, $Y), format_value(Month, $M),
              format_value(Day, $D)]),
    if D == <<"P">> -> <<"PT0S">>;
       true -> D
    end;
duration_string({?C_DURATION,
                 {Year, Month, Day,  Hour, Min, Sec, Us}}, _TypeStack) ->
    D = ?l2b([$P, format_value(Year, $Y), format_value(Month, $M),
              format_value(Day, $D), $T, format_value(Hour, $H),
              format_value(Min, $M), format_sec_value(Sec, Us, $S)]),
    if D == <<"PT">> -> <<"PT0S">>;
       true -> D
    end.

format_sec_value(0, 0, _Token) -> "";
format_sec_value(Sec, 0, Token) -> [?i2l(Sec), Token];
format_sec_value(Sec, Us, Token) ->
    [?i2l(Sec), us2fraction(Us), Token].

format_value(0, _Token) -> "";
format_value(Value, Token) -> [?i2l(Value), Token].


atom_value(_Ctx, String) -> ?l2a(String).

atom_string(Value, _TypeStack) -> ?a2b(Value).

%%% inetAddress

inetAddress_value(Ctx, String) ->
    case catch inetAddressIP_value(Ctx, String) of
        {'EXIT', _} -> inetAddressDNS_value(Ctx, String);
        Value -> Value
    end.

inetAddress_string(Value, TypeStack) ->
    case catch inetAddressIP_string(Value, TypeStack) of
        {'EXIT', _} -> inetAddressDNS_string(Value, TypeStack);
        String -> String
    end.

%%% inetAddressIP

inetAddressIP_value(_Ctx, String) ->
    case inet_parse:ipv4strict_address(String) of
        {ok, Value} ->
            Value;
        _ ->
            {ok, Value} = inet_parse:ipv6strict_address(String),
            Value
    end.

inetAddressIP_string(Value, _TypeStack) ->
    ?l2b(string:to_lower(inet_parse:ntoa(Value))).

%%% inetAddressIPv4

inetAddressIPv4_value(_, String) ->
    {ok, Value} = inet_parse:ipv4strict_address(String),
    Value.

inetAddressIPv4_string(Value, _TypeStack) -> ?l2b(inet_parse:ntoa(Value)).

%%% inetAddressIPv6

inetAddressIPv6_value(_, String) ->
    {ok, Value} = inet_parse:ipv6strict_address(String),
    Value.

inetAddressIPv6_string(Value, _TypeStack) ->
    ?l2b(string:to_lower(inet_parse:ntoa(Value))).

%%% inetAddressDNS

inetAddressDNS_value(_Ctx, String) ->
    %% we can't be strict, not even non-strict... www.6wind.com is ok.
%    v_dns_label(String),
    ?l2b(String).

%-define(is_letter(H), (((H >= $a) and (H =< $z)) or
%                      ((H >= $A) and (H =< $Z)))).
%-define(is_digit(H), ((H >= $0) and (H =< $9))).
%-define(is_ld(H), (?is_letter(H) or ?is_digit(H))).

%v_dns_label([H | T]) when ?is_letter(H) ->
%    v_dns_ldh(T, 1).

%-ifdef(strict).
%v_dns_ldh(_, 64) -> erlang:error(error);
%v_dns_ldh([$-, H | T], N) when ?is_ld(H) ->
%    v_dns_ldh(T, N+2);
%v_dns_ldh([H | T], N) when ?is_ld(H) ->
%    v_dns_ldh(T, N+1);
%v_dns_ldh([$. | T], _) ->
%    v_dns_label(T);
%v_dns_ldh([], _) ->
%    ok.
%-else.
%v_dns_ldh([$. | T], _) ->
%    v_dns_label(T);
%v_dns_ldh([_ | T], N) ->
%    v_dns_ldh(T, N);
%v_dns_ldh([], _) ->
%    ok.
%-endif.

inetAddressDNS_string(Value, _TypeStack) when is_binary(Value) -> Value.

%%% inetPortNumber

inetPortNumber_value(_, String) ->
    case ?l2i(String) of
        Integer when Integer >= 0, Integer =< 65535 -> {?C_UINT16, Integer}
    end.

inetPortNumber_string({?C_UINT16, Integer}, _TypeStack) -> ?i2b(Integer).

%%% ipv4Prefix

ipv4_prefix_value(Ctx, String) ->
    [IpStr, PrefixStr] = string:tokens(String, "/"),
    Ip = inetAddressIPv4_value(Ctx, IpStr),
    case ?l2i(PrefixStr) of
        Prefix when Prefix >= 0, Prefix =< 32 ->
            assert_zero_bits(Ip, Prefix),
            {?C_IPV4PREFIX, {Ip, Prefix}}
    end.

ipv4_prefix_string({?C_IPV4PREFIX, {Ip, Prefix}}, _TypeStack) ->
    ?l2b([inet_parse:ntoa(Ip), $/, ?i2l(Prefix)]).

%%% ipv6Prefix

ipv6_prefix_value(Ctx, String) ->
    [IpStr, PrefixStr] = string:tokens(String, "/"),
    Ip = inetAddressIPv6_value(Ctx, IpStr),
    case ?l2i(PrefixStr) of
        Prefix when Prefix >= 0, Prefix =< 128 ->
            assert_zero_bits(Ip, Prefix),
            {?C_IPV6PREFIX, {Ip, Prefix}}
    end.

ipv6_prefix_string({?C_IPV6PREFIX, {Ip, Prefix}}, _TypeStack) ->
    ?l2b([string:to_lower(inet_parse:ntoa(Ip)), $/, ?i2l(Prefix)]).

%% assert that all bits outside PrefixLength are zero
assert_zero_bits({A1,A2,A3,A4}, PrefixLength) ->
    assert_zero_bits((((((A1 bsl 8) bor A2) bsl 8) bor A3) bsl 8) bor A4,
                     32 - PrefixLength);
assert_zero_bits({A1,A2,A3,A4,A5,A6,A7,A8}, PrefixLength) ->
    assert_zero_bits((((((((((((((A1 bsl 16) bor A2) bsl 16) bor A3) bsl 16)
                             bor A4) bsl 16) bor A5) bsl 16) bor A6) bsl 16)
                       bor A7) bsl 16) bor A8, 128 - PrefixLength);
assert_zero_bits(Address, SuffixLength)
  when (Address band ((1 bsl SuffixLength) - 1)) == 0 ->
    ok.

%%% IpPrefix

ip_prefix_value(Ctx, String) ->
    try
        ipv4_prefix_value(Ctx, String)
    catch
        error:_ ->
            ipv6_prefix_value(Ctx, String)
    end.

ip_prefix_string(Value, TypeStack) ->
    try
        ipv4_prefix_string(Value, TypeStack)
    catch
        error:_ ->
            ipv6_prefix_string(Value, TypeStack)
    end.

%%% ipv4AddressAndPrefixLength

ipv4_and_plen_value(Ctx, String) ->
    [IpStr, PrefixLenStr] = string:tokens(String, "/"),
    Ip = inetAddressIPv4_value(Ctx, IpStr),
    case ?l2i(PrefixLenStr) of
        PrefixLen when PrefixLen >= 0, PrefixLen =< 32 ->
            {?C_IPV4_AND_PLEN, {Ip, PrefixLen}}
    end.

ipv4_and_plen_string({?C_IPV4_AND_PLEN, {Ip, PrefixLen}}, _TypeStack) ->
    ?l2b([inet_parse:ntoa(Ip), $/, ?i2l(PrefixLen)]).

%%% ipv6AddressAndPrefixLength

ipv6_and_plen_value(Ctx, String) ->
    [IpStr, PrefixLenStr] = string:tokens(String, "/"),
    Ip = inetAddressIPv6_value(Ctx, IpStr),
    case ?l2i(PrefixLenStr) of
        PrefixLen when PrefixLen >= 0, PrefixLen =< 128 ->
            {?C_IPV6_AND_PLEN, {Ip, PrefixLen}}
    end.

ipv6_and_plen_string({?C_IPV6_AND_PLEN, {Ip, PrefixLen}}, _TypeStack) ->
    ?l2b([string:to_lower(inet_parse:ntoa(Ip)), $/, ?i2l(PrefixLen)]).

%%% ipAddressAndPrefixLength

ip_and_plen_value(Ctx, String) ->
    try
        ipv4_and_plen_value(Ctx, String)
    catch
        error:_ ->
            ipv6_and_plen_value(Ctx, String)
    end.

ip_and_plen_string(Value, TypeStack) ->
    try
        ipv4_and_plen_string(Value, TypeStack)
    catch
        error:_ ->
            ipv6_and_plen_string(Value, TypeStack)
    end.


%%% decimal64

decimal64_value(#exs_ctx{type_stack=TypeStack}, String) ->
    FractionDigits = decimal64_fraction_digits(TypeStack),
    ValStr = decimal64_intstr(FractionDigits, String, false),
    case ?l2i(ValStr) of
        Value when Value >= ?LONG_MIN, Value =< ?LONG_MAX ->
            {?C_DECIMAL64, {Value, FractionDigits}}
    end.

decimal64_intstr(N, [S|Rest], HasValue) when S == $-; S == $+ ->
    [S|decimal64_intstr(N, Rest, HasValue)];
decimal64_intstr(N, [I, $., F|Rest], _HasValue) ->
    [I|decimal64_fractstr(N, [F|Rest])];
decimal64_intstr(N, [D|Rest], _HasValue) ->
    [D|decimal64_intstr(N, Rest, true)];
decimal64_intstr(N, [], true) ->
    decimal64_fractstr(N, []).

decimal64_fractstr(N, [D|Rest]) when N > 0 ->
    [D|decimal64_fractstr(N-1, Rest)];
decimal64_fractstr(N, []) when N > 0 ->
    [$0|decimal64_fractstr(N-1, [])];
decimal64_fractstr(0, []) ->
    [].

decimal64_string({?C_DECIMAL64, {Value, FractionDigits}}, TypeStack) ->
    FractionDigits = decimal64_fraction_digits(TypeStack), % assertion
    case ?i2l(Value) of
        [$-|Str0] -> Sign = "-";
        Str0      -> Sign = ""
    end,
    Str = decimal64_decstr0(FractionDigits, lists:reverse(Str0)),
    ?l2b([Sign, lists:reverse(Str)]).

decimal64_decstr0(N, [$0|Rest]) when N > 1 ->
    decimal64_decstr0(N-1, Rest);
decimal64_decstr0(N, []) when N > 1 ->
    decimal64_decstr(1, []);
decimal64_decstr0(N, Str) ->
    decimal64_decstr(N, Str).

decimal64_decstr(N, [D|Rest]) when N > 0 ->
    [D|decimal64_decstr(N-1, Rest)];
decimal64_decstr(N, []) when N > 0 ->
    [$0|decimal64_decstr(N-1, [])];
decimal64_decstr(0, []) ->
    decimal64_decstr(0, "0");
decimal64_decstr(0, Str) ->
    [$.|Str].

decimal64_fraction_digits(
  [#exs_type{type=derived, derivation=#restriction{facets=Facets}}|Rest]) ->
    decimal64_fraction_digits(Facets,Rest);
decimal64_fraction_digits([_|Rest]) ->
    decimal64_fraction_digits(Rest).

%% there can only be one (in yang)
decimal64_fraction_digits([#fraction_digits{value = V}|_], _) -> V;
decimal64_fraction_digits([_|Facets], Rest) ->
    decimal64_fraction_digits(Facets, Rest);
decimal64_fraction_digits([], Rest) ->
    decimal64_fraction_digits(Rest).

%%% identityref

identityref_value(#exs_ctx{type_stack=TypeStack}, String) ->
    Identities = get_identities(TypeStack),
    case string:tokens(String, ":") of
        [Prefix, Name] ->
            PrefixB = ?l2b(Prefix),
            NameA = ?l2ea(Name),
            {_, IdVal} = lists:keyfind({PrefixB, NameA}, 1, Identities),
            {?C_IDENTITYREF, IdVal};
        [Name] ->
            NameA = ?l2ea(Name),
            [IdVal] = [IdVal || {{_, Id}, IdVal} <- Identities, Id == NameA],
            {?C_IDENTITYREF, IdVal}
    end.

identityref_string({?C_IDENTITYREF, {_HNs, _HName} = IdValue}, TypeStack) ->
    Identities = get_identities(TypeStack),
    {{PrefixB, Id}, _} = lists:keyfind(IdValue, 2, Identities),
    ?l2b([PrefixB, $:, ?a2l(Id)]).

get_identities(
  [#exs_type{type=derived, derivation=#restriction{facets=Facets}}|Rest]) ->
    get_identities(Facets, Rest);
get_identities([_|Rest]) ->
    get_identities(Rest).

get_identities([#ignore_facet{value = [{identities, Ids}]}|_], _) -> Ids;
get_identities([_|Facets], Rest) ->
    get_identities(Facets, Rest);
get_identities([], Rest) ->
    get_identities(Rest).

%%% size

-define(GIGA, (1024*1024*1024)).
-define(MEGA, (1024*1024)).
-define(KILO, 1024).

size_value(_, [$S|Rest]) ->
    {GB, Cont} = gb(Rest),
    {MB, Cont2} = mb(Cont),
    {KB, Cont3} = kb(Cont2),
    {B, []} = b(Cont3),
    TotalBytes = GB*?GIGA+MB*?MEGA+KB*?KILO+B,
    true = (TotalBytes =< ?ULONG_MAX),
    {?C_UINT64, TotalBytes};
size_value(_, Rest) ->
    {GB, Cont} = gb(Rest),
    {MB, Cont2} = mb(Cont),
    {KB, Cont3} = kb(Cont2),
    {B, []} = b(Cont3),
    TotalBytes = GB*?GIGA+MB*?MEGA+KB*?KILO+B,
    true = (TotalBytes =< ?ULONG_MAX),
    {?C_UINT64, TotalBytes}.

gb(List) -> get_digits("Gg", List).

mb(List) -> get_digits("Mm", List).

kb(List) -> get_digits("Kk", List).

b(List) -> get_digits("Bb", List).

size_string({?C_UINT64, 0}, _TypeStack) -> <<"S0B">>;
size_string({?C_UINT64, Bytes}, _TypeStack) ->
    GB = Bytes div ?GIGA,
    RemainingBytes = Bytes rem ?GIGA,
    MB = RemainingBytes div ?MEGA,
    RemainingBytes2 = RemainingBytes rem ?MEGA,
    KB = RemainingBytes2 div ?KILO,
    B = RemainingBytes2 rem ?KILO,
    list_to_binary([$S, format_value(GB, $G), format_value(MB, $M),
                    format_value(KB, $K), format_value(B, $B)]).

%% hexValue

hexValue_value(_, String) -> ?l2b(hex2int(String)).

hex2int([]) -> [];
hex2int([X, Y|Rest]) ->
    %% http://schemecookbook.org/Erlang/NumberConvOctHex
    {ok, [Value], []} = io_lib:fread("~16u", [X, Y]),
    [Value|hex2int(Rest)].

hexValue_string(Value, _TypeStack) -> ?l2b(int2hex(?b2l(Value))).

int2hex([]) -> [];
int2hex([Value|Rest]) ->
    %% http://schemecookbook.org/Erlang/NumberConvOctHex
    [io_lib:fwrite("~.16b", [Value])|int2hex(Rest)].


%% hexList
%% Format:  4F:4C:41  stored as <<"OLA">>

hexList_value(_,String) ->
    {?C_BINARY,?l2b( hex2val(String))}.

hex2val([Mso,Lso,$:|R]) ->
    [hexdigit(Lso) bor (hexdigit(Mso) bsl 4) | hex2val(R)];
hex2val([Mso,Lso]) ->
    [hexdigit(Lso) bor (hexdigit(Mso) bsl 4)];
hex2val([]) ->
    [].

hexList_string({?C_BINARY,Value},_TypeStack) ->
    ?l2b( val2hex_list( ?b2l(Value) )).

val2hex_list([X]) ->
    [ d2h(X bsr 4),d2h(X band 15)];
val2hex_list([X|Xs]) ->
    [ d2h(X bsr 4),d2h(X band 15),$:|val2hex_list(Xs)];
val2hex_list([]) ->
    [].


%% hexString
%% Format: same as hexList (but ?C_HEXSTR instead of ?C_BINARY)

hexString_value(_, String) ->
    {?C_HEXSTR, ?l2b(hex2val(String))}.

hexString_string({?C_HEXSTR, Value}, _TypeStack) ->
    ?l2b(val2hex_list(?b2l(Value))).


%% octetList
%% Format:  192.168.0.1  stored as <<192,168,0,1>>

octetList_value(_,"") ->
    {?C_BINARY,<<"">>};
octetList_value(_,String) ->
    {?C_BINARY,?l2b( octets2val(String,0))}.

octets2val([X, $. | [_|_] = Xs], Acc) when X >= $0, X =< $9, Acc =< 255 ->
    case Acc*10 + X - $0 of
        Acc1 when Acc1 =< 255 ->
            [Acc1|octets2val(Xs, 0)]
    end;
octets2val([X|Xs], Acc) when X >= $0, X =< $9, Acc =< 255 ->
    octets2val(Xs, Acc*10 + X - $0);
octets2val([], Acc) when Acc =< 255->
    [Acc].


octetList_string({?C_BINARY,Value},_TypeStack) ->
    ?l2b(val2octets( ?b2l(Value))).

val2octets([X]) ->
    integer_to_list(X);
val2octets([X|Xs]) ->
    [integer_to_list(X),$.|val2octets(Xs)];
val2octets([]) ->
    [].


%% dottedQuad
%% Format: same as octetList but always 4 octets
%% (and ?C_DQUAD instead of ?C_BINARY)

dottedQuad_value(_, String) ->
    case octets2val(String, 0) of
        OctetList when length(OctetList) == 4 ->
            {?C_DQUAD, ?l2b(OctetList)}
    end.

dottedQuad_string({?C_DQUAD, Value}, _TypeStack) ->
    ?l2b(val2octets(?b2l(Value))).


%% oid
%% Format:  1.3.6.1.4.1.24961  stored as <<0,0,0,1,0,0,0,3,0,0,0,6....>>
%% each integer (32bit) takes 4 bytes
%% Max 128 subidentifiers
oid_value(_,String) ->
    Val = oid2val(String, 0, 1),
    [0,0,0,E1,0,0,0,E2|_] = Val, % assert that we have at least two identifiers
    true = E1 < 3,  % assert that the first id is 0,1,2
    true = 255 >= E1 * 40 + E2, % assertion (snmp_pdus:end_oid_tag)
    {?C_OID, ?l2b(Val)}.

oid2val([X|Xs],Acc, N) when X>=$0,X=<$9,Acc=<16#ffffffff ->
    oid2val(Xs, Acc*10 + X -$0, N);
oid2val([$.|[_|_]=Xs],Acc, N) when Acc=<16#ffffffff, N =< 127->
    [ ((Acc bsr 24) band 16#ff),((Acc bsr 16)band 16#ff),
      ((Acc bsr 8)band 16#ff), (Acc band 16#ff)| oid2val(Xs,0, N+1)];
oid2val([],Acc, _N) when Acc=<16#ffffffff ->
    [ ((Acc bsr 24) band 16#ff),((Acc bsr 16)band 16#ff),
      ((Acc bsr 8)band 16#ff), (Acc band 16#ff)].

oid_string({?C_OID,Value},_TypeStack) ->
    ?l2b(val2oid(?b2l(Value))).


val2oid([X1,X2,X3,X4]) ->
    integer_to_list((X1 bsl 24)+(X2 bsl 16)+(X3 bsl 8)+X4);
val2oid([X1,X2,X3,X4|Xs]) ->
    [integer_to_list((X1 bsl 24)+(X2 bsl 16)+(X3 bsl 8)+X4),$.|val2oid(Xs)];
val2oid([]) ->
    [].

log(Level, Fmt, Args) ->
   io:format("<~s> " ++ Fmt ++ "~n", levelstr(Level) ++ Args).

log(IoDevice, Level, Fmt, Args) ->
   io:format(IoDevice, "<~s> " ++ Fmt ++ "~n", levelstr(Level) ++ Args).

levelstr(?CONFD_LEVEL_ERROR) -> ["ERR"];
levelstr(?CONFD_LEVEL_INFO) -> ["INFO"];
levelstr(?CONFD_LEVEL_TRACE) -> ["DEBUG"].
