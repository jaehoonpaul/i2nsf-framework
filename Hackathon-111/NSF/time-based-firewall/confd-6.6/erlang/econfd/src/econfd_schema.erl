%%%-------------------------------------------------------------------
%%% @copyright 2012 Tail-F Systems AB
%%% @version {$Id$}
%%% @doc Support for using schema information in the Erlang API.
%%%
%%% Keeps schema info in a set of ets tables named by the toplevel namespace.
%%% @end
%%%-------------------------------------------------------------------
-module(econfd_schema).

-behaviour(gen_server).

%%% external exports
-export([load/2]).
-export([get_nslist/0, get_cs/2, ikeypath2cs/1, choice_children/1]).
-export([str2val/2, val2str/2]).
-export([get_type/1, get_type/2]).
-export([register_type_cbs/1]).
-export([start_link/0]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% include files
-include("../include/econfd.hrl").
-include("../include/econfd_errors.hrl").
-include("econfd_internal.hrl").

%%% macros
-define(SERVER, ?MODULE).

%%%--------------------------------------------------------------------
%%% External functions
%%%--------------------------------------------------------------------

%% @spec load(Address::ip(), Port::integer()) -> ok | {error, Reason::term()}
%% @doc Load schema info from ConfD
load(Address, Port) ->
    econfd_internal:load_schemas(Address, Port).

%% @spec get_nslist() -> [#confd_nsinfo{}]
%% @doc Get a list of loaded namespaces with info
get_nslist() ->
    econfd_internal:get_nslist().

%% @spec get_cs(Ns::econfd:namespace(), Tagpath::econfd:tagpath()) ->
%%         #confd_cs_node{} | not_found
%% @doc Find schema node by namespace and tagpath.
get_cs(Ns, Tagpath) ->
    econfd_internal:get_cs(Ns, Tagpath).

%% @spec ikeypath2cs(IKeypath::econfd:ikeypath()) ->
%%         #confd_cs_node{} | not_found
%% @doc Find schema node by ikeypath
ikeypath2cs(IKeypath) ->
    {Ns, Tagpath} = ikeypath2nstagpath(IKeypath),
    get_cs(Ns, Tagpath).

%% @spec choice_children(C) -> [econfd:qtag()]
%%     C = #confd_cs_node{} | [econfd:qtag()|#confd_cs_choice{}]
%% @doc Get a flat list of children for a #confd_cs_node{},
%% with any choice/case structure(s) removed.
choice_children(#confd_cs_node{children = Children}) ->
    choice_children(Children);
choice_children([#confd_cs_choice{cases = Cases}|T]) ->
    lists:flatmap(fun(#confd_cs_case{children = Children}) ->
                          choice_children(Children)
                  end, Cases) ++
        choice_children(T);
choice_children([H|T]) ->
    [H|choice_children(T)];
choice_children([]) ->
    [].

%% @spec str2val(TypeId, Lexical) ->
%%           {ok, Value} | {error, term()}
%%     TypeId = #confd_cs_node{} | econfd:type()
%%     Lexical = binary()
%%     Value = econfd:value()
%% @doc Convert string to value based on schema type.
str2val(#confd_cs_node{type = Type}, Lexical) ->
    str2val(Type, Lexical);
str2val(Type, Lexical) ->
    econfd_internal:str2val(Type, Lexical).

%% @spec val2str(TypeId, Value) ->
%%           {ok, Lexical} | {error, term()}
%%     TypeId = #confd_cs_node{} | econfd:type()
%%     Value = econfd:value()
%%     Lexical = binary()
%% @doc Convert value to string based on schema type.
val2str(#confd_cs_node{type = Type}, Value) ->
    val2str(Type, Value);
val2str(Type, Value) ->
    econfd_internal:val2str(Type, Value).

%% @spec get_type(TypeName::atom()) ->
%%         econfd:type() | not_found
%% @doc Get schema type definition identifier for built-in type.
get_type(TypeName) ->
    get_builtin_type(TypeName).

%% @spec get_type(Ns::econfd:namespace(), TypeName::atom()) ->
%%         econfd:type()
%% @doc Get schema type definition identifier for type defined in namespace.
get_type(Ns, TypeName) ->
    {Ns, TypeName}.

%% @spec register_type_cbs(#confd_type_cbs{}) -> ok
%% @doc Register callbacks for a user-defined type. For an application
%% running in its own Erlang VM, this function registers the callbacks
%% in the loaded schema information, similar to confd_register_node_type()
%% in the C API. For an application running inside ConfD, this function
%% registers the callbacks in ConfD's internal schema information, similar
%% to using a shared object with confd_type_cb_init() in the C API.
register_type_cbs(TypeCbs)->
    econfd_internal:register_type_cbs(TypeCbs).

%% @spec start_link() -> {ok, pid()}
%% @private
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%--------------------------------------------------------------------
%%% gen_server callbacks
%%%--------------------------------------------------------------------

%% @private
init([]) ->
    econfd_internal:schema_init().

%% @private
handle_call({load, Address, Port}, _From, State) ->
    econfd_internal:load_schemas(Address, Port, State);
handle_call(get_nslist, _From, State) ->
    econfd_internal:nsinfo(State).

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%--------------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------

ikeypath2nstagpath(IKeypath) ->
    ikeypath2nstagpath(IKeypath, []).

ikeypath2nstagpath([[Ns|Tag]], Acc) ->
    {Ns, lists:reverse([Tag | Acc])};
ikeypath2nstagpath([Key | T], Acc) when is_tuple(Key) ->
    ikeypath2nstagpath(T, Acc);
ikeypath2nstagpath([[Ix] | T], Acc) when is_integer(Ix) ->
    ikeypath2nstagpath(T, Acc);
ikeypath2nstagpath([Tag | T], Acc) ->
    ikeypath2nstagpath(T, [Tag | Acc]).


get_builtin_type(string) ->
    {?W3C_SCHEMA_URI, string};
get_builtin_type(boolean) ->
    {?W3C_SCHEMA_URI, boolean};
get_builtin_type(float) ->
    {?W3C_SCHEMA_URI, float};
get_builtin_type(double) ->
    {?W3C_SCHEMA_URI, double};
get_builtin_type(decimal) ->
    {?W3C_SCHEMA_URI, decimal};
get_builtin_type(hexBinary) ->
    {?W3C_SCHEMA_URI, hexBinary};
get_builtin_type(base64Binary) ->
    {?W3C_SCHEMA_URI, base64Binary};
get_builtin_type(anyURI) ->
    {?W3C_SCHEMA_URI, anyURI};
get_builtin_type('QName') ->
    {?W3C_SCHEMA_URI, 'QName'};
get_builtin_type('NOTATION') ->
    {?W3C_SCHEMA_URI, 'NOTATION'};
get_builtin_type(normalizedString) ->
    {?W3C_SCHEMA_URI, normalizedString};
get_builtin_type(token) ->
    {?W3C_SCHEMA_URI, token};
get_builtin_type(language) ->
    {?W3C_SCHEMA_URI, language};
get_builtin_type('IDREFS') ->
    {?W3C_SCHEMA_URI, 'IDREFS'};
get_builtin_type('IDREF_LIST') ->
    {?W3C_SCHEMA_URI, 'IDREF_LIST'};
get_builtin_type('ENTITIES') ->
    {?W3C_SCHEMA_URI, 'ENTITIES'};
get_builtin_type('ENTITY_LIST') ->
    {?W3C_SCHEMA_URI, 'ENTITY_LIST'};
get_builtin_type('NMTOKEN') ->
    {?W3C_SCHEMA_URI, 'NMTOKEN'};
get_builtin_type('NMTOKENS') ->
    {?W3C_SCHEMA_URI, 'NMTOKENS'};
get_builtin_type('NMTOKEN_LIST') ->
    {?W3C_SCHEMA_URI, 'NMTOKEN_LIST'};
get_builtin_type('Name') ->
    {?W3C_SCHEMA_URI, 'Name'};
get_builtin_type('NCName') ->
    {?W3C_SCHEMA_URI, 'NCName'};
get_builtin_type('ID') ->
    {?W3C_SCHEMA_URI, 'ID'};
get_builtin_type('IDREF') ->
    {?W3C_SCHEMA_URI, 'IDREF'};
get_builtin_type('ENTITY') ->
    {?W3C_SCHEMA_URI, 'ENTITY'};
get_builtin_type(integer) ->
    {?W3C_SCHEMA_URI, integer};
get_builtin_type(nonPositiveInteger) ->
    {?W3C_SCHEMA_URI, nonPositiveInteger};
get_builtin_type(negativeInteger) ->
    {?W3C_SCHEMA_URI, negativeInteger};
get_builtin_type(long) ->
    {?W3C_SCHEMA_URI, long};
get_builtin_type(int) ->
    {?W3C_SCHEMA_URI, int};
get_builtin_type(short) ->
    {?W3C_SCHEMA_URI, short};
get_builtin_type(byte) ->
    {?W3C_SCHEMA_URI, byte};
get_builtin_type(nonNegativeInteger) ->
    {?W3C_SCHEMA_URI, nonNegativeInteger};
get_builtin_type(positiveInteger) ->
    {?W3C_SCHEMA_URI, positiveInteger};
get_builtin_type(unsignedLong) ->
    {?W3C_SCHEMA_URI, unsignedLong};
get_builtin_type(unsignedInt) ->
    {?W3C_SCHEMA_URI, unsignedInt};
get_builtin_type(unsignedShort) ->
    {?W3C_SCHEMA_URI, unsignedShort};
get_builtin_type(unsignedByte) ->
    {?W3C_SCHEMA_URI, unsignedByte};
get_builtin_type(dateTime) ->
    {?W3C_SCHEMA_URI, dateTime};
get_builtin_type(date) ->
    {?W3C_SCHEMA_URI, date};
get_builtin_type(time) ->
    {?W3C_SCHEMA_URI, time};
get_builtin_type(duration) ->
    {?W3C_SCHEMA_URI, duration};

get_builtin_type('objectRef') ->
    {?CONFD_URI, 'objectRef'};
get_builtin_type('Counter32') ->
    {?CONFD_URI, 'Counter32'};
get_builtin_type('Gauge32') ->
    {?CONFD_URI, 'Gauge32'};
get_builtin_type('Counter64') ->
    {?CONFD_URI, 'Counter64'};
get_builtin_type(atom) ->
    {?CONFD_URI, atom};
get_builtin_type(inetAddress) ->
    {?CONFD_URI, inetAddress};
get_builtin_type(inetAddressIP) ->
    {?CONFD_URI, inetAddressIP};
get_builtin_type(inetAddressIPv4) ->
    {?CONFD_URI, inetAddressIPv4};
get_builtin_type(inetAddressIPv6) ->
    {?CONFD_URI, inetAddressIPv6};
get_builtin_type(inetAddressDNS) ->
    {?CONFD_URI, inetAddressDNS};
get_builtin_type(inetPortNumber) ->
    {?CONFD_URI, inetPortNumber};
get_builtin_type(ipv4Prefix) ->
    {?CONFD_URI, ipv4Prefix};
get_builtin_type(ipv6Prefix) ->
    {?CONFD_URI, ipv6Prefix};
get_builtin_type(ipPrefix) ->
    {?CONFD_URI, ipPrefix};
get_builtin_type(ipv4AddressAndPrefixLength) ->
    {?CONFD_URI, ipv4AddressAndPrefixLength};
get_builtin_type(ipv6AddressAndPrefixLength) ->
    {?CONFD_URI, ipv6AddressAndPrefixLength};
get_builtin_type(ipAddressAndPrefixLength) ->
    {?CONFD_URI, ipAddressAndPrefixLength};
get_builtin_type(decimal64) ->
    {?CONFD_URI, decimal64};
get_builtin_type(identityref) ->
    {?CONFD_URI, identityref};
get_builtin_type(size) ->
    {?CONFD_URI, size};
get_builtin_type(hexValue) ->
    {?CONFD_URI, hexValue};
get_builtin_type(oid) ->
    {?CONFD_URI, oid};
get_builtin_type(hexList) ->
    {?CONFD_URI, hexList};
get_builtin_type(hexString) ->
    {?CONFD_URI, hexString};
get_builtin_type(octetList) ->
    {?CONFD_URI, octetList};
get_builtin_type(dottedQuad) ->
    {?CONFD_URI, dottedQuad};
get_builtin_type('MD5DigestString') ->
    {?CONFD_URI, 'MD5DigestString'};
get_builtin_type('SHA256DigestString') ->
    {?CONFD_URI, 'SHA256DigestString'};
get_builtin_type('SHA512DigestString') ->
    {?CONFD_URI, 'SHA512DigestString'};
get_builtin_type(cryptHash) ->
    {?CONFD_URI, cryptHash};
get_builtin_type('DES3CBCIVEncryptedString') ->
    {?CONFD_URI, 'DES3CBCIVEncryptedString'};
get_builtin_type('AESCFB128IVEncryptedString') ->
    {?CONFD_URI, 'AESCFB128IVEncryptedString'};

get_builtin_type(int8) ->
    get_builtin_type(byte);
get_builtin_type(int16) ->
    get_builtin_type(short);
get_builtin_type(int32) ->
    get_builtin_type(int);
get_builtin_type(int64) ->
    get_builtin_type(long);
get_builtin_type(uint8) ->
    get_builtin_type(unsignedByte);
get_builtin_type(uint16) ->
    get_builtin_type(unsignedShort);
get_builtin_type(uint32) ->
    get_builtin_type(unsignedInt);
get_builtin_type(uint64) ->
    get_builtin_type(unsignedLong);
get_builtin_type(binary) ->
    get_builtin_type(base64Binary);
get_builtin_type('instance-identifier') ->
    get_builtin_type('objectRef');
get_builtin_type('hex-list') ->
    get_builtin_type(hexList);
get_builtin_type('octet-list') ->
    get_builtin_type(octetList);
get_builtin_type('md5-digest-string') ->
    get_builtin_type('MD5DigestString');
get_builtin_type('sha-256-digest-string') ->
    get_builtin_type('SHA256DigestString');
get_builtin_type('sha-512-digest-string') ->
    get_builtin_type('SHA512DigestString');
get_builtin_type('des3-cbc-encrypted-string') ->
    get_builtin_type('DES3CBCIVEncryptedString');
get_builtin_type('aes-cfb-128-encrypted-string') ->
    get_builtin_type('AESCFB128IVEncryptedString');

get_builtin_type(_) -> not_found.

