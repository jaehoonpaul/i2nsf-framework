-ifndef(CONFD_INTERNAL_HRL).
-define(CONFD_INTERNAL_HRL, true).

-include("generated.hrl").

-define(a2l, atom_to_list).
-define(l2a, list_to_atom).
-define(l2ea, list_to_existing_atom).
-define(b2l, binary_to_list).
-define(l2b, list_to_binary).
-define(i2l, integer_to_list).
-define(l2i, list_to_integer).
-define(l2f, list_to_float).
-define(f2l, float_to_list).
-define(l2t, list_to_tuple).
-define(t2l, tuple_to_list).
-define(t2b, term_to_binary).
-define(b2t, binary_to_term).
-define(a2b(Atom), ?l2b(?a2l(Atom))).
-define(i2b(Atom), ?l2b(?i2l(Atom))).
-define(b2a(Bin), ?l2a(?b2l(Bin))).
-define(s2l(Str), if is_atom(Str) -> ?a2l(Str);
                     is_binary(Str) -> ?b2l(Str);
                     is_list(Str) -> Str
                  end).
-define(s2io(Str), if is_atom(Str) -> ?a2l(Str);
                      is_binary(Str) -> Str;
                      is_list(Str) -> Str
                  end).
-define(s2b(Str), if is_atom(Str) -> ?a2b(Str);
                     is_binary(Str) -> Str;
                     is_list(Str) -> ?l2b(Str)
                  end).
-define(s2a(Str), if is_atom(Str) -> Str;
                     is_binary(Str) -> list_to_atom(binary_to_list(Str));
                     is_list(Str) -> ?l2a(Str)
                  end).

-define (CLIENT_CAPI,         3).
-define (CLIENT_CDB,          5).
-define (CLIENT_MAAPI,        7).
-define (CLIENT_STREAM,       8).
-define (CLIENT_EVENT_MGR,   11).
-define (CLIENT_HA,          17).

-define(wsock(Tctx), (Tctx#confd_trans_ctx.dx)#confd_daemon_ctx.worker).


%% we can send the following orders over the ha socket
-define(CONFD_HA_ORDER_BEMASTER,   1).
-define(CONFD_HA_ORDER_BESLAVE,    2).
-define(CONFD_HA_ORDER_BENONE,     3).
-define(CONFD_HA_ORDER_GETSTATUS,  4).
-define(CONFD_HA_ORDER_SLAVE_DEAD, 5).
-define(CONFD_HA_ORDER_BERELAY,    6).



%% use outside catch
-define(error_msg(Fmt, Args),
        error_logger:error_msg("~w:~w: " ++ Fmt ++ "~p\n",
                               [?MODULE, ?LINE | Args] ++ [?stack()])).
%% use this inside catch
-define(cerror_msg(Fmt, Args),
        error_logger:error_msg("~w:~w: " ++ Fmt ++ "~p\n",
                               [?MODULE, ?LINE | Args] ++
                               [erlang:get_stacktrace()])).



%% Typically used in error printouts as in:
%% error_logger:format("Err ~p at ~p~n", [Reason, ?stack()])
-define(stack(), try throw(1) catch _:_ -> erlang:get_stacktrace() end).


%%%--------------------------------------------------------------------
%%% econfd_schema
%%%--------------------------------------------------------------------

-define(W3C_SCHEMA_URI, 'http://www.w3.org/2001/XMLSchema').
-define(CONFD_URI, 'http://tail-f.com/ns/confd/1.0').

-record(cs_error, {
          from,           %% 'undefined' | 'db' | 'cdb'
          type,           %% atom()
          code,           %% atom()
          ikeypath,       %% undefined | none | cs:ikeypath()
          apptag,         %% undefined | exs_type:qtag()
          str,            %% undefined | binary() | iolist()
          opaque,         %% term()
          made = false    %% boolean()
         }).

%% simplified #fxs_header{}
-record(ns_header, {
          %% always ns_header - (single instance)
          key = ns_header,
          %% atom()
          uri,
          %% string() - xmlns prefix
          prefix,
          %% integer()
          id_hash_value,
          %% yang revision
          revision,
          %% yang module name
          module
         }).

%% from exs.hrl

-record(exs_type, {
          %% atom()
          name,
          %% primitive | derived | user_defined
          type = derived,
          %% (Bytes::string()) ->
          %%     {ok, LexicalValue::string()} | {error, Reason}
          lexical_value_fun,
          %% (LexicalValue::string()) -> {ok, Value} | {error, Reason}
          %% Value = xsd:types()
          value_fun,
          %% (UntaggedValue::term()) -> {ok, Value} | {error, Reason}
          %% Value = xsd:types()
          value2value_fun,
          %% (Value) -> {ok, LexicalValue::string()} | {error, Reason}
          %% Value = xsd:types()
          string_fun,
          %% (Value) -> {ok, LexicalValue::string()} | {error, Reason}
          %% Value = xsd:types()
          string_cli_fun,
          %% #restriction{} | #list{} | #union{} | #bits{} | undefined
          derivation,
          %% annotation/documentation
          %% binary() | []
          desc = [],
          %% undefined
          %% (UntaggedValue::term()) -> ok | {error, Reason}
          check_value,
          %% [{key::atom(), value::term()}]
          extra = [],
          %% ?F_EXS_TYPE*
          flags = 0
         }).

-record(restriction, {
          %% TypeName::atom() | {Ns::atom(), TypeName::atom()}
          base,
          %% [#enumeration{} | #fraction_digits{} | #length{} | #unique_list{} |
          %%  #max_exclusive{} | #max_inclusive{} | #max_length{} |
          %%  #min_exclusive{} | #min_inclusive{} | #min_length{} |
          %%  #pattern{} | #total_digits{} | #white_space{} | #ignore_facet{} |
          %%  #range_facet{}]
          facets = []
         }).

-record(list, {
          %% TypeName::atom() | {Ns::atom(), TypeName::atom()}
          item_type
         }).

-record(union, {
          %% [TypeName::atom() | {Ns::atom(), TypeName::atom()}]
          member_types
         }).

-record(bits, {
          %% [{Bit::integer(), Label::string()}]
          fields = [],
          %% 32 | 64
          size
         }).

-record(enumeration, {
          %% string()
          value,
          %% bstring() || false
          code_name = false,
          %% integer()
          hash_value
         }).

-record(fraction_digits, {
          %% integer()
          value,
          %% boolean()
          fixed = false,
          %% binary() | []
          error_message = [],
          %% binary() | []
          error_app_tag = []
         }).

-record(base, {
          %% atom() | {Ns::atom(), atom()}
          identity
         }).

-record(length, {
          %% integer()
          value,
          %% boolean()
          fixed = false,
          %% binary() | []
          error_message = [],
          %% binary() | []
          error_app_tag = []
         }).

-record(unique_list, {
          %% <NOT USED>
          value,
          %% boolean()
          fixed = false,
          %% binary() | []
          error_message = [],
          %% binary() | []
          error_app_tag = [],
          %% integer() | undefined
          min_occurs,
          %% integer() | undefined
          max_occurs
         }).

-record(max_exclusive, {
          %% integer() | float()
          value,
          %% boolean()
          fixed = false,
          %% binary() | []
          error_message = [],
          %% binary() | []
          error_app_tag = []
         }).

-record(max_inclusive, {
          %% integer() | float()
          value,
          %% boolean()
          fixed = false,
          %% binary() | []
          error_message = [],
          %% binary() | []
          error_app_tag = []
         }).

-record(max_length, {
          %% integer()
          value,
          %% bollean()
          fixed = false,
          %% binary() | []
          error_message = [],
          %% binary() | []
          error_app_tag = []
         }).

-record(min_exclusive, {
          %% integer() | float()
          value,
          %% boolean()
          fixed = false,
          %% binary() | []
          error_message = [],
          %% binary() | []
          error_app_tag = []
         }).

-record(min_inclusive, {
          %% integer() | float()
          value,
          %% boolean()
          fixed = false,
          %% binary() | []
          error_message = [],
          %% binary() | []
          error_app_tag = []
         }).

-record(min_length, {
          %% integer()
          value,
          %% boolean()
          fixed = false,
          %% binary() | []
          error_message = [],
          %% binary() | []
          error_app_tag = []
         }).

-record(pattern, {
          %% string()
          value,
          %% boolean()
          fixed = false,
          %% binary() | []
          error_message = [],
          %% binary() | []
          error_app_tag = []}).

-record(display_hint, {
          %% string()
          value,
          compiled,
          %% boolean()
          fixed = false,
          %% binary() | []
          error_message = [],
          %% binary() | []
          error_app_tag = []}).

-record(total_digits, {
          %% integer()
          value,
          %% boolean()
          fixed = false,
          %% binary() | []
          error_message = [],
          %% binary() | []
          error_app_tag = []
         }).

-record(white_space, {
          %% collapse | replace | preserve
          value,
          %% boolean()
          fixed = false,
          %% binary() | []
          error_message = [],
          %% binary() | []
          error_app_tag = []
         }).

%% This facet can be used feed extra information to the value_fun
%% field in #exs_type{}. This way extra type restrictions applied to
%% types, i.e. evaluated whenever exs_type:string2value/5 is called.
-record(ignore_facet, {
          %% [{atom(), term()}]
          value,
          %% boolean()
          fixed = false,
          %% binary() | []
          error_message = [],
          %% binary() | []
          error_app_tag = []
         }).

-record(range_facet, {
          %% string()
          value,
          %% boolean()
          fixed = false,
          %% binary() | []
          error_message = [],
          %% binary() | []
          error_app_tag = [],
          %% float()
          step = 0.0}).

%% This record is *not* saved on disk, only used for
%% arguments to the #exs_type{} funs.
-record(exs_ctx, {
          %% saxlite:ns_stack()
          ns_stack = [],
          %% TypeStack
          type_stack = []
         }).

%% Applicable facets

-define(collapsed_string_facets,
        [ignore_facet, enumeration, length, max_length, min_length, pattern]).

-define(string_facets, [white_space|?collapsed_string_facets]).

-define(float_facets,
        [ignore_facet, enumeration, max_exclusive, max_inclusive, min_exclusive,
         min_inclusive, pattern, range_facet]).

-define(date_facets, ?float_facets).

-define(integer_facets, [total_digits|?float_facets]).

-define(decimal_facets, [fraction_digits|?integer_facets]).

-define(boolean_facets, [ignore_facet, pattern]).

-define(list_facets,
        [ignore_facet, length, max_length, min_length, enumeration, white_space,
         unique_list]).

-define(union_facets, [ignore_facet, pattern, enumeration]).

-define(identityref_facets, [ignore_facet|?collapsed_string_facets]).

%% limits.hrl

-define(UBYTE_MIN, 0). % duh!
-define(UBYTE_MAX, 255).
-define(UINT8_MIN, 0).
-define(UINT8_MAX, 255).

-define(BYTE_MIN, (-128)).
-define(BYTE_MAX, 127).
-define(INT8_MIN, (-128)).
-define(INT8_MAX, 127).

-define(SHRT_MIN, (-32768)).
-define(SHRT_MAX, 32767).
-define(INT16_MIN, (-32768)).
-define(INT16_MAX, 32767).

-define(USHRT_MIN, 0). % duh!
-define(USHRT_MAX, 65535).
-define(UINT16_MIN, 0). % duh!
-define(UINT16_MAX, 65535).

-define(INT_MIN, (-?INT_MAX - 1)).
-define(INT_MAX, 2147483647).
-define(INT32_MIN, (-?INT32_MAX - 1)).
-define(INT32_MAX, 2147483647).

-define(UINT_MIN, 0). % duh!
-define(UINT_MAX, 4294967295).
-define(UINT32_MIN, 0). % duh!
-define(UINT32_MAX, 4294967295).

-define(LONG_MAX, 9223372036854775807).
-define(LONG_MIN, (-?LONG_MAX - 1)).
-define(INT64_MAX, 9223372036854775807).
-define(INT64_MIN, (-?INT64_MAX - 1)).

-define(ULONG_MIN, 0). % duh!
-define(ULONG_MAX, 18446744073709551615).
-define(UINT64_MIN, 0). % duh!
-define(UINT64_MAX, 18446744073709551615).

%% from xsd.erl

%% The number of digits in the 'decimal' type, i.e. the (low)
%% approximate number of significant digits in a float (C
%% 'double').
-define(DECIMAL_DIGITS, 15).

%% The number of significant digits we give in the string
%% representation of xs:float and xs:double.
-define(FLOAT_DIGITS, "16").

-endif.


