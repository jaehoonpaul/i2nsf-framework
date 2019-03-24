%%%-------------------------------------------------------------------
%%% @copyright 2006 Tail-F Systems AB
%%% @version {$Id$}
%%% @doc An Erlang interface equivalent to the confd_lib_dp C-API
%%% (documented in confd_lib_dp(3)).
%%%
%%% This module is used to connect to ConfD and provide callback
%%% functions so that ConfD can populate its northbound agent
%%% interfaces with external data.  Thus the library consists of a
%%% number of API functions whose purpose is to install different
%%% callback functions at different points in the XML tree which is
%%% the representation of the device configuration. Read more about
%%% callpoints in the ConfD User Guide.
%%% @end
%%%-------------------------------------------------------------------

-module(econfd).
-export([start/0,
         init_daemon/6,
         set_debug/3,
         set_daemon_d_opaque/2,
         set_daemon_flags/2,
         stop_daemon/1,
         register_trans_cb/2,
         register_trans_validate_cb/2,
         register_db_cbs/2,
         register_data_cb/2,
         register_range_data_cb/5,
         register_valpoint_cb/2,
         register_action_cb/2,
         register_authentication_cb/2,
         register_service_cb/2,
         register_nano_service_cb/4,
         register_done/1,
         pp_value/1,
         pp_kpath/1,
         decrypt/1,
         controlling_process/2,
         bitbig_set_bit/2,
         bitbig_clr_bit/2,
         bitbig_bit_is_set/2]).

-export([data_reply_ok/1,
         data_reply_value/2,
         data_reply_next_key/3,
         data_reply_not_found/1,
         data_reply_value_array/2,
         data_reply_tag_value_array/2,
         data_reply_next_object_value_array/3,
         data_reply_next_object_tag_value_array/3,
         data_reply_next_object_value_arrays/3,
         data_reply_found/1,
         data_reply_error/2,
         data_set_timeout/2,
         action_set_timeout/2]).

-export([notification_send/3,
         notification_replay_complete/1,
         notification_replay_failed/2,
         register_notification_stream/2]).

-export([log/2,
         log/3,
         log/4]).

-import(econfd_internal,
        [
         term_write/2,
         term_write/3
        ]).

%% application internal exports, used by other econfd modules
-export([mk_uinfo/1, mk_uinfo/2, report_err/4]).
-export([trans_put/3, trans_get/3, trans_erase/3]).

%% used by the Makefile to do edoc
-export([doc_application/1]).

-include("../include/econfd.hrl").
-include("../include/econfd_errors.hrl").
-include("econfd_internal.hrl").
-include("econfd_proto.hrl").

%%% types

%% When running econfd internally the socket is a tuple.
-type socket() :: port() | int_ipc:sock().
-export_type([socket/0]).

-type ip() :: tuple().
%% 4-tuples for IP v4 addresses and 8-tuples for IP v6 addresses.

-type value() :: binary() | ip() | float() | boolean() | integer() | qtag() |
                 {Tag::integer(), Value::term()} | [value()] | not_found.
%% This type is central for this library. Values are returned from the CDB
%% functions, they are used to read and write in the MAAPI module and they
%% are also used as keys in ikeypath().
%%
%% We have the following value representation for the data model types
%%<ul>
%% <li> string - Always represented as a single binary. </li>
%% <li> int32  - This is represented as a single integer. </li>
%% <li> int8   - {?C_INT8, Val} </li>
%% <li> int16  - {?C_INT16, Val} </li>
%% <li> int64  - {?C_INT64, Val} </li>
%% <li> uint8  - {?C_UINT8, Val} </li>
%% <li> uint16 - {?C_UINT16, Val} </li>
%% <li> uint32 - {?C_UINT32, Val} </li>
%% <li> uint64 - {?C_UINT64, Val} </li>
%% <li> inet:ipv4-address - 4-tuple </li>
%% <li> inet:ipv4-address-no-zone - 4-tuple </li>
%% <li> inet:ipv6-address - 8-tuple </li>
%% <li> inet:ipv6-address-no-zone - 8-tuple </li>
%% <li> boolean - The atoms 'true' or 'false' </li>
%% <li> xs:float() and xs:double() - Erlang floats </li>
%% <li> leaf-list - An erlang list of values. </li>
%% <li> binary, yang:hex-string, tailf:hex-list (etc) -
%%      {?C_BINARY, binary()} </li>
%% <li> yang:date-and-time - {?C_DATETIME, datetime()} </li>
%% <li> xs:duration - {?C_DURATION, {Y,M,D,H,M,S,Mcr}} </li>
%% <li> instance-identifier - {?C_OBJECTREF, econfd:ikeypath()} </li>
%% <li> yang:object-identifier - {?C_OID, Int32Binary}, where Int32Binary is a
%%      binary with OID compontents as 32-bit integers in the default
%%      big endianness. </li>
%% <li> yang:dotted-quad - {?C_DQUAD, binary()} </li>
%% <li> yang:hex-string  - {?C_HEXSTR, binary()} </li>
%% <li> inet:ipv4-prefix - {?C_IPV4PREFIX, {{A,B,C,D}, PrefixLen}} </li>
%% <li> inet:ipv6-prefix - {?C_IPV6PREFIX, {{A,B,C,D,E,F,G,H}, PrefixLen}} </li>
%% <li> tailf:ipv4-address-and-prefix-length -
%%      {?C_IPV4_AND_PLEN, {{A,B,C,D}, PrefixLen}} </li>
%% <li> tailf:ipv6-address-and-prefix-length -
%%      {?C_IPV6_AND_PLEN, {{A,B,C,D,E,F,G,H}, PrefixLen}} </li>
%% <li> decimal64 - {?C_DECIMAL64, {Int64, FractionDigits}} </li>
%% <li> identityref - {?C_IDENTITYREF, {NsHash, IdentityHash}} </li>
%% <li> bits - {?C_BIT32, Bits::integer()}, {?C_BIT64, Bits::integer()}, or
%%      {?C_BITBIG, Bits:binary()}
%%      depending on the highest bit position assigned </li>
%% <li> enumeration - {?C_ENUM_VALUE, IntVal}, where IntVal is the integer
%%      value for a given "enum" statement according to the YANG specification.
%%      When we have compiled a YANG module into a .fxs file, we can use the
%%      --emit-hrl option to confdc(1) to create a .hrl file with macro
%%      definitions for the enum values. </li>
%%</ul>
%%
%% There is also a "pseudo type" that indicates a non-existing value,
%% which is represented as the atom 'not_found'.
%% Finally there is a "pseudo type" to indicate that a leaf with a default
%% value defined in the data model does not have a value set - this is
%% represented as the atom 'default'.
%%
%% For all of the abovementioned (non-"pseudo") types we have the corresponding
%% macro in econfd.hrl. We strongly suggest that the ?CONFD_xxx macros are used
%% whenever we either want to construct a value or match towards a value:
%% Thus we write code as:
%% <pre>
%%  case econfd_cdb:get_elem(...) of
%%      {ok, ?CONFD_INT64(42)} ->
%%          foo;
%%
%% or
%%  econfd_cdb:set_elem(... ?CONFD_INT64(777), ...)
%%
%% or
%%  {ok, ?CONFD_INT64(I)} = econfd_cdb:get_elem(...)
%%
%%</pre>

%% @type datetime() = {C_DATETIME::integer(), DateAndTime}
%%      DateAndTime = {Year::integer(), Month::integer(), Day::integer(),
%%                     Hour::integer(), Minute::integer(), Second::integer(),
%%                     MicroSecond::integer(),
%%                     TZ::integer(), TZMinutes::integer()}.
%% The value representation for yang:date-and-time, also used in the
%% API functions for notification streams.

-type key() :: {value()} | [Index::integer()].
%% Keys are parts of ikeypath(). In the YANG data model we define how many
%% keys a list node has. If we have 1 key, the key is an arity-1
%% tuple, 2 keys - an arity-2 tuple and so forth.
%% The [Index] notation is only valid for keys in ikeypaths when we use CDB.

-type cons(T1, T2) :: nonempty_improper_list(T1, T2). % too wide definition
-type namespace() :: atom().
-type tag() :: atom().
-type qtag() :: tag() | cons(namespace(), tag()).
%% A "qualified tag" is either a single tag or a pair of a namespace and a
%% tag.  An example could be 'interface' or
%% ['http://example.com/ns/interfaces/2.1' | interface]
-export_type([namespace/0]).

-type ikeypath() :: [qtag() | key()].
%% An ikeypath() is a list describing a path down into the data tree.
%% The Ikeypaths are used to denote specific objects in the XML instance
%% document.  The list is in backwards order, thus the head of the list
%% is the leaf element.  All the data callbacks defined in
%% #confd_data_cbs{} receive ikeypath() lists as an argument.  The last
%% (top) element of the list is a pair `[NS|XmlTag]' where NS is
%% the atom defining the XML namespace of the XmlTag and
%% XmlTag is an XmlTag::atom() denoting the toplevel XML element.
%% Elements in the list that have a different namespace than their parent
%% are also qualified through such a pair with the element's namespace,
%% but all other elements are represented by their unqualified tag() atom.
%% Thus an ikeypath() uniquely addresses an instance of an element in
%% the configuration XML tree.  List entries are
%% identified by an element in the ikeypath() list expressed as {Key}
%% or, when we are using CDB, as [Integer]. During an individual CDB
%% session all the elements are implictly numbered, thus we can through
%% a call to econfd_cdb:num_instances/2 retrieve how many entries (N)
%% for a given list that we have, and then retrieve those entries
%% (0 - (N-1)) inserting [I] as the key.

%% @type tagpath() = [qtag()].
%% A tagpath() is a list describing a path down into the schema tree.
%% I.e. as opposed to an ikeypath(), it has no instance information.
%% Additionally the last (top) element is not `[NS|XmlTag]' as in
%% ikeypath(), but only `XmlTag' - i.e. it needs to be combined with
%% a namespace to uniquely identify a schema node. The other elements
%% in the path are qualified - or not - exactly as for ikeypath().

%% @type tagval() =
%%   {qtag(), value() | start | stop | leaf | {start, Index::integer()}}.
%% This is used to represent XML elements together with their values,
%% typically in a list representing an XML subtree as in the arguments
%% and result of the 'action' callback. Typeless elements have the
%% special "values" 'start' (opening container or list element), 'stop'
%% (closing container or list element), 'leaf' (leaf with type "empty"),
%% or {start, Index} (opening list element with CDB Index instead of key
%% value(s) - only valid for CDB access). The qtag() tuple element may
%% have the namespace()-less form (i.e. tag()) for XML elements in the
%% "current" namespace. For a detailed description of how to represent
%% XML as a list of tagval() elements, please refer to the "Tagged Value
%% Array" specification in the XML STRUCTURES section of the
%% confd_types(3) manual page.

%% @type type() = term().
%% Identifies a type definition in the schema.

-type error_reason() :: binary() | #confd_error{} | tuple().
%% The callback functions may return errors either as a plain string
%% or via a #confd_error{} record - see econfd.hrl and the section EXTENDED
%% ERROR REPORTING in confd_lib_lib(3) (tuple() is only for internal
%% ConfD/NCS use). {error, String} is equivalent to
%% {error, #confd_error{ code = application, str = String }}.
%%

%% @type cb_exists_optional() =
%%   (T::#confd_trans_ctx{}, KP::ikeypath()) ->
%%     {ok, Reply} | {ok, Reply, #confd_trans_ctx{}} |
%%     {error, error_reason()} | delayed_response
%% Reply = bool().
%% This is the callback for #confd_data_cbs.exists_optional.
%% The exists_optional callback must be present
%% if our YANG model has presence containers or leafs of type empty.

%% @type cb_get_elem() =
%%   (T::#confd_trans_ctx{}, KP::ikeypath()) ->
%%     {ok, Reply} | {ok, Reply, #confd_trans_ctx{}} |
%%     {error, error_reason()} | delayed_response
%% Reply = V::value() | not_found.
%% This is the callback for #confd_data_cbs.get_elem.

%% @type cb_get_next() =
%%   (T::#confd_trans_ctx{}, KP::ikeypath(), Prev::term()) ->
%%     {ok, Reply} | {ok, Reply, #confd_trans_ctx{}} |
%%     {error, error_reason()} | delayed_response
%% Reply = {Key::key(), Next::term()} | {false, undefined}.
%% This is the callback for #confd_data_cbs.get_next.  Prev is
%% the integer -1 on the first call.

%% @type cb_find_next() =
%%   (T::#confd_trans_ctx{}, KP::ikeypath(), FindNextType::integer(),
%%    PrevKey::key()) ->
%%     {ok, Reply} | {ok, Reply, #confd_trans_ctx{}} |
%%     {error, error_reason()} | delayed_response
%% Reply = {Key::key(), Next::term()} | {false, undefined}.
%% This is the callback for #confd_data_cbs.find_next.

%% @type cb_num_instances() =
%%    (T::#confd_trans_ctx{}, KP::ikeypath()) ->
%%     {ok, Reply} | {ok, Reply, #confd_trans_ctx{}} |
%%     {error, error_reason()} | delayed_response
%% Reply = Number::integer().
%% Optional callback, if it doesn't exist it will be emulated
%% by consecutive calls to get_next().
%% It is the callback for #confd_data_cbs.num_instances.

%% @type cb_get_object() =
%%   (T::#confd_trans_ctx{}, KP::ikeypath()) ->
%%     {ok, Reply} | {ok, Reply, #confd_trans_ctx{}} |
%%     {error, error_reason()} | delayed_response
%% Reply = [V::value()] | {exml, [TV::tagval()]} | not_found.
%% Optional callback which is used to return an entire object.
%% It is the callback for #confd_data_cbs.get_object.
%% For a detailed description of the two forms of the value list,
%% please refer to the "Value Array" and "Tag Value Array" specifications,
%% respectively, in the XML STRUCTURES section of the confd_types(3)
%% manual page.

%% @type cb_get_next_object() =
%%   (T::#confd_trans_ctx{}, KP::ikeypath(), Prev::term()) ->
%%     {ok, Reply} | {ok, Reply, #confd_trans_ctx{}} |
%%     {ok, Objects, TimeoutMillisecs::integer()} |
%%     {ok, Objects, TimeoutMillisecs::integer(), #confd_trans_ctx{}} |
%%     {error, error_reason()} | delayed_response
%% Reply = ValObject | TagValObject | {false, undefined}
%% Objects = [ValObject | TagValObject | false]
%% ValObject = {[V::value()], Next::term()}
%% TagValObject = {{exml, [TV::tagval()]}, Next::term()}.
%% Optional callback which combines the functionality of
%% get_next() and get_object(), and adds the possibility
%% to return multiple objects.
%% It is the callback for #confd_data_cbs.get_next_object.
%% For a detailed description of the two forms of the value list,
%% please refer to the "Value Array" and "Tag Value Array" specifications,
%% respectively, in the XML STRUCTURES section of the confd_types(3)
%% manual page.

%% @type cb_find_next_object() =
%%   (T::#confd_trans_ctx{}, KP::ikeypath(), FindNextType::integer(),
%%    PrevKey::key()) ->
%%     {ok, Reply} | {ok, Reply, #confd_trans_ctx{}} |
%%     {ok, Objects, TimeoutMillisecs::integer()} |
%%     {ok, Objects, TimeoutMillisecs::integer(), #confd_trans_ctx{}} |
%%     {error, error_reason()} | delayed_response
%% Reply = ValObject | TagValObject | {false, undefined}
%% Objects = [ValObject | TagValObject | false]
%% ValObject = {[V::value()], Next::term()}
%% TagValObject = {{exml, [TV::tagval()]}, Next::term()}.
%% Optional callback which combines the functionality of
%% find_next() and get_object(), and adds the possibility
%% to return multiple objects.
%% It is the callback for #confd_data_cbs.find_next_object.
%% For a detailed description of the two forms of the value list,
%% please refer to the "Value Array" and "Tag Value Array" specifications,
%% respectively, in the XML STRUCTURES section of the confd_types(3)
%% manual page.

%% @type cb_set_elem() =
%%   (T::#confd_trans_ctx{}, KP::ikeypath(),Value::value()) ->
%%     ok | {error, error_reason()} | delayed_response.
%% It is the callback for #confd_data_cbs.set_elem. Only used
%% when we use external database config data, e.g. not for statistics.

%% @type cb_create() =
%%   (T::#confd_trans_ctx{}, KP::ikeypath()) ->
%%     ok | {error, error_reason()} | delayed_response.
%% It is the callback for #confd_data_cbs.create. Only used
%% when we use external database config data, e.g. not for statistics.

%% @type cb_remove() =
%%   (T::#confd_trans_ctx{}, KP::ikeypath()) ->
%%     ok | {error, error_reason()} | delayed_response.
%% It is the callback for #confd_data_cbs.remove. Only used
%% when we use external database config data, e.g. not for statistics.

-type cb_get_case() ::
        fun((T::#confd_trans_ctx{}, KP::ikeypath(), ChoicePath::[qtag()]) ->
                   {ok, Case::qtag()} | {ok, not_found} |
                   {error, error_reason()} | delayed_response).
%% This is the callback for #confd_data_cbs.get_case. Only used when we
%% use 'choice' in the data model.
%% Normally ChoicePath is just a single element with the name of the
%% choice, but if we have nested choices without intermediate data nodes,
%% it will be similar to an ikeypath, i.e. a reversed list of choice and
%% case names giving the path through the nested choices.

-type cb_set_case() ::
        fun((T::#confd_trans_ctx{}, KP::ikeypath(),
             ChoicePath::[qtag()], Case :: qtag() | '$none') ->
                   ok | {error, error_reason()} | delayed_response).
%% This is the callback for #confd_data_cbs.set_case. Only used when we
%% use 'choice' in the data model. Case == '$none'
%% means that no case is chosen (i.e. all have been deleted).
%% Normally ChoicePath is just a single element with the name of the
%% choice, but if we have nested choices without intermediate data nodes,
%% it will be similar to an ikeypath, i.e. a reversed list of choice and
%% case names giving the path through the nested choices.

%% @type cb_get_attrs() =
%%   (T::#confd_trans_ctx{}, KP::ikeypath(), [Attr::integer()]) ->
%%     {ok, [{Attr::integer(), V::value()}]} | {ok, not_found} |
%%     {error, error_reason()} | delayed_response.
%% This is the callback for #confd_data_cbs.get_attrs.

%% @type cb_set_attr() =
%%   (T::#confd_trans_ctx{}, KP::ikeypath(), Attr::integer(), Value) ->
%%     ok | {error, error_reason()} | delayed_response
%%  Value = value() | undefined.
%% This is the callback for #confd_data_cbs.set_attr. Value == undefined
%% means that the attribute should be deleted.

%% @type cb_move_after() =
%%   (T::#confd_trans_ctx{}, KP::ikeypath(), PrevKeys::{value()}) ->
%%     ok | {error, error_reason()} | delayed_response.
%% This is the callback for #confd_data_cbs.move_after. PrevKeys == {}
%% means that the list entry should become the first one.

%% @type cb_write_all() =
%%   (T::#confd_trans_ctx{}, KP::ikeypath()) ->
%%     ok | {error, error_reason()} | delayed_response.
%% This is the callback for #confd_data_cbs.write_all. The KP argument
%% is currently always [], since the callback does not pertain to any
%% particular data node.


%% @type cb_validate() =
%% (T::#confd_trans_ctx{}, KP::ikeypath(), Newval::value()) ->
%%  ok | {error, error_reason()} | {validation_warn, Reason::binary()}.
%% It is the callback for #confd_valpoint_cb.validate.


%% @type cb_action() = cb_action_act() | cb_action_cmd().
%% It is the callback for #confd_action_cb.action

%% @type cb_action_act() =
%% (U::#confd_user_info{}, Name::qtag(), KP::ikeypath(), [Param::tagval()]) ->
%%  ok | {ok, [Result::tagval()]} | {error, error_reason()}.
%% It is the callback for #confd_action_cb.action when invoked as an
%% action request.

%% @type cb_action_cmd() =
%% (U::#confd_user_info{}, Name::binary(), Path::binary(), [Arg::binary()]) ->
%%  ok | {ok, [Result::binary()]} | {error, error_reason()}.
%% It is the callback for #confd_action_cb.action when invoked as a
%% CLI command callback.

%% @type completion_action() =
%% (U::#confd_user_info{}, CliStyle::int(), Token::binary(),
%%  CompletionChar::int(), IKP::ikeypath(), CmdPath::binary(),
%%  Id::binary(), TP::term(), Extra::term()) ->
%% [string() | {info, string()} | {desc, string()} | default].

%%  ok | {ok, [Result::tagval()]} | {error, error_reason()}.
%% It is the callback for #confd_action_cb.action when invoked as an
%% action request.


%% @type cb_ctx() =
%%  (#confd_trans_ctx{}) -> ok | {ok, #confd_trans_ctx{}} |
%%                          {error, error_reason()}.
%% The callback for #confd_trans_validate_cbs.init and
%% #confd_trans_cbs.init as well as
%% several other callbacks in #confd_trans_cbs{}

%% @type cb_trans_lock() =
%%  (#confd_trans_ctx{}) -> ok | {ok, #confd_trans_ctx{}} |
%%                          {error, error_reason()} | confd_already_locked.
%% The callback for #confd_trans_cbs.trans_lock. The confd_already_locked
%% return value is equivalent to {error, #confd_error{ code = in_use }}.

%% @type cb_write() =
%%  (#confd_trans_ctx{}) -> ok | {ok, #confd_trans_ctx{}} |
%%                          {error, error_reason()} | confd_in_use.
%% The callback for #confd_trans_cbs.write_start and #confd_trans_cbs.prepare.
%% The confd_in_use return value is equivalent to
%% {error, #confd_error{ code = in_use }}.

%% @type cb_ok() =
%%  (#confd_trans_ctx{}) -> ok | {error, error_reason()}.
%% The callback for #confd_trans_cbs.finish and
%% #confd_trans_validate_cbs.stop


%% @type cb_candidate_commit() =
%%  (#confd_db_ctx{}, Timeout::integer()) -> ok | {error, error_reason()}.
%% The callback for #confd_db_cbs.candidate_commit

%% @type cb_db() =
%%  (#confd_db_ctx{}, DbName::integer()) -> ok | {error, error_reason()}.
%% The callback for #confd_db_cbs.lock, #confd_db_cbs.unlock, and
%% #confd_db_cbs.delete_config

%% @type cb_lock_partial() =
%%  (#confd_db_ctx{}, DbName::integer(), LockId::integer(), [ikeypath()]) ->
%%    ok | {error, error_reason()}.
%% The callback for #confd_db_cbs.lock_partial

%% @type cb_unlock_partial() =
%%  (#confd_db_ctx{}, DbName::integer(), LockId::integer()) ->
%%    ok | {error, error_reason()}.
%% The callback for #confd_db_cbs.unlock_partial

%% @type cb_ok_db() =
%%  (#confd_db_ctx{}) -> ok | {error, error_reason()}.
%% The callback for #confd_db_cbs.candidate_confirming_commit
%% and several other callbacks in #confd_db_cbs{}


%% @type cb_authentication() =
%%  (#confd_authentication_ctx{}) -> ok | error | {error, binary()}.
%% The callback for #confd_authentication_cb.auth


%% @type cb_get_log_times() =  (#confd_notification_ctx{}) ->
%%      {ok, {Created::datetime(), Aged::datetime() | not_found}} |
%%      {error, error_reason()}.
%% The callback for #confd_notification_stream_cbs.get_log_times

%% @type cb_replay() = (#confd_notification_ctx{},
%%                      Start::datetime(), Stop::datetime() | undefined) ->
%%       ok | {error, error_reason()}.
%% The callback for #confd_notification_stream_cbs.replay


%% @type cb_str_to_val() = (TypeCtx::term(), String::string()) ->
%%      {ok, Value::value()} | error | {error, Reason::binary()} | none().
%% The callback for #confd_type_cbs.str_to_val. The TypeCtx argument is
%% currently unused (passed as 'undefined'). The function may fail - this
%% is equivalent to returning 'error'.

%% @type cb_val_to_str() = (TypeCtx::term(), Value::value()) ->
%%      {ok, String::string()} | error | {error, Reason::binary()} | none().
%% The callback for #confd_type_cbs.val_to_str. The TypeCtx argument is
%% currently unused (passed as 'undefined'). The function may fail - this
%% is equivalent to returning 'error'.

%% @type cb_validate_value() = (TypeCtx::term(), Value::value()) ->
%%      ok | error | {error, Reason::binary()} | none().
%% The callback for #confd_type_cbs.validate. The TypeCtx argument is
%% currently unused (passed as 'undefined'). The function may fail - this
%% is equivalent to returning 'error'.




%%%--------------------------------------------------------------------
%%% External functions
%%%--------------------------------------------------------------------

%% @spec start() -> ok | {error, Reason::term()}
%% @doc Starts the econfd application.
start() ->
    application:start(econfd).

%% @spec init_daemon(Name::atom(), DebugLevel::integer(),
%%                   Estream::io_device(),
%%                   Dopaque::term(), Ip::ip(), Port::integer()) ->
%%       {ok, Pid::pid()} | {error, Reason::term()}
%% @doc Starts and links to a gen_server which connects to ConfD.
%% This gen_server holds two sockets to ConfD, one so called control
%% socket and one worker socket (See confd_lib_dp(3) for an explanation
%% of those sockets.)
%%
%% The gen_server is used to install sets of callback Funs.  The
%% gen_server state is a #confd_daemon_ctx{}. This structure is passed
%% to all the callback functions.
%%
%% The daemon context includes a d_opaque element holding the Dopaque
%% term - this can be used by the application to pass application
%% specific data into the callback functions.
%%
%% The Name::atom() parameter is used in various debug printouts and
%% is also used to uniquely identify the daemon.
%%
%% The  DebugLevel parameter is used to control the
%% debug level. The following levels are available:
%%
%% <ul><li>?CONFD_SILENT
%%        No debug printouts whatsoever are produced by the library.
%%</li><li>
%%?CONFD_DEBUG
%%       Various printouts will occur for various error conditions.
%%</li><li>
%%?CONFD_TRACE
%%       The execution of callback functions will be traced.
%%</li></ul>
%%       The Estream parameter is used by all printouts from the
%%       library.
init_daemon(Name, DebugLevel, Estream, Dopaque, Ip, Port) when
                                                     is_atom(Name),
                                                     is_integer(DebugLevel) ->
    econfd_daemon:start_link(Name, DebugLevel, Estream, Dopaque,Ip,Port).

%% @spec set_debug(Daemon::pid(),
%%                 DebugLevel::integer(), Estream::io_device()) -> ok
%% @doc Change the DebugLevel and/or Estream for a running daemon
set_debug(Daemon, DebugLevel, Estream) ->
    gen_server:call(Daemon, {set_debug, DebugLevel, Estream}).

%% @spec set_daemon_d_opaque(Daemon::pid(),
%%                           Dopaque::term()) -> ok
%% @doc Set the d_opaque field in the daemon which is typically used by
%% the callbacks
set_daemon_d_opaque(Daemon, Dopaque) ->
    gen_server:cast(Daemon, {set_d_opaque, Dopaque}).

-spec set_daemon_flags(Daemon :: pid(), Flags :: integer()) -> ok.
%% @doc Change the flag settings for a daemon. See ?CONFD_DAEMON_FLAG_XXX
%% in econfd.hrl for the available flags. This function should be called
%% immediately after creating the daemon context with init_daemon/6.
set_daemon_flags(Daemon, Flags) ->
    gen_server:call(Daemon, {set_flags, Flags}).

%% @spec stop_daemon(Daemon::pid()) -> ok
%% @doc Silently stop a daemon
stop_daemon(Daemon) ->
    econfd_daemon:stop(Daemon).

%% @spec register_trans_cb(Daemon::pid(), TransCbs::#confd_trans_cbs{}) ->
%% ok | {error, Reason::term()}
%% @doc Register transaction phase callbacks.
%% See confd_lib_dp(3) for a thorough description of the transaction phases.
%% The record #confd_trans_cbs{} contains callbacks for all of the
%% phases for a transaction. If we use this external data api only for
%% statistics data only the init() and the finish() callbacks should be
%% used.  The init() callback must return 'ok', {error, String}, or {ok, Tctx}
%% where Tctx is the same #confd_trans_ctx that was supplied to the
%% init callback but possibly with the opaque field filled in. This field
%% is meant to be used by the user to manage user data.

register_trans_cb(Daemon, TransCbs)
  when is_record(TransCbs, confd_trans_cbs) ->
    gen_server:call(Daemon, {register_trans_cb, TransCbs}).

%% @spec register_trans_validate_cb(Daemon::pid(),
%%                                  ValidateCbs::#confd_trans_validate_cbs{}) ->
%% ok | {error, Reason::term()}
%% @doc Register validation transaction callback.
%% This function maps an init and a finish function for validations.
%% See seme function in confd_lib_dp(3)
%% The init() callback must return 'ok', {error, String}, or {ok, Tctx}
%% where Tctx is the same #confd_trans_ctx that was supplied to the
%% init callback but possibly with the opaque field filled in.

register_trans_validate_cb(Daemon, ValidateCbs) when
 is_record(ValidateCbs, confd_trans_validate_cbs) ->
    gen_server:call(Daemon, {register_trans_validate_cbs, ValidateCbs}).

%% @spec register_db_cbs(Daemon::pid(), DbCbs::#confd_db_cbs{}) ->
%% ok | {error, Reason::term()}
%% @doc Register extern db callbacks.
register_db_cbs(Daemon, DbCbs) when is_record(DbCbs, confd_db_cbs) ->
     gen_server:call(Daemon, {register_db_cb, DbCbs}).

%% @spec register_data_cb(Daemon::pid(), DbCbs::#confd_data_cbs{})  ->
%% ok | {error, Reason::term()}
%% @doc Register the data callbacks.
register_data_cb(Daemon, DataCbs)
  when is_atom(DataCbs#confd_data_cbs.callpoint)  ->
    gen_server:call(Daemon, {register_data_cbs, DataCbs}).


%% @spec register_range_data_cb(Daemon::pid(), DataCbs::#confd_data_cbs{},
%%                              [Lower::value()], [Higher::value()],
%%                              IKP::ikeypath()) ->
%%     ok | {error, Reason::term()}
%% @doc Register data callbacks for a range of keys.
register_range_data_cb(Daemon, DataCbs, Lower, Higher, IKP)->
    gen_server:call(Daemon, {confd_register_range_data_cb,
                             DataCbs, Lower, Higher, IKP}).

%% @spec register_valpoint_cb(Daemon::pid(),
%%                            ValpointCbs::#confd_valpoint_cb{}) ->
%%     ok | {error, Reason::term()}
%% @doc Register validation callback on a valpoint
register_valpoint_cb(Daemon, ValpointCbs) ->
     gen_server:call(Daemon, {register_valpoint_cbs, ValpointCbs}).

%% @spec register_action_cb(Daemon::pid(),
%%                          ActionCbs::#confd_action_cb{}) ->
%%     ok | {error, Reason::term()}
%% @doc Register action callback on an actionpoint
register_action_cb(Daemon, ActionCb) ->
     gen_server:call(Daemon, {register_action_cb, ActionCb}).

%% @spec register_authentication_cb(Daemon::pid(),
%%                            AuthenticationCb::#confd_authentication_cb{}) ->
%%     ok | {error, Reason::term()}
%% @doc Register authentication callback.
%% Note, this can not be used to *perform* the authentication.
register_authentication_cb(Daemon, AuthenticationCb) ->
     gen_server:call(Daemon, {register_authentication_cb, AuthenticationCb}).

%% @spec register_notification_stream(Daemon::pid(),
%%                          NotifCbs::#confd_notification_stream_cbs{}) ->
%%     {ok, #confd_notification_ctx{}} | {error, Reason::term()}
%% @doc Register notif callbacks on an streamname
register_notification_stream(_Daemon, NotifCb)
  when ((NotifCb#confd_notification_stream_cbs.get_log_times == undefined)
        and
        (NotifCb#confd_notification_stream_cbs.replay /= undefined)) ->
    {error, badarg};  %% must have either both or none
register_notification_stream(_Daemon, NotifCb)
  when ((NotifCb#confd_notification_stream_cbs.get_log_times /= undefined)
        and
        (NotifCb#confd_notification_stream_cbs.replay == undefined)) ->
    {error, badarg};  %% must have eitehr both or none

register_notification_stream(Daemon, NotifCb) ->
    gen_server:call(Daemon, {register_notification_stream, NotifCb}).

%% @spec register_done(Daemon::pid()) ->
%%     ok | {error, Reason::term()}
%% @doc This function must be called when all callback registrations are done
register_done(Daemon) ->
     gen_server:call(Daemon, register_done).

%% @spec data_reply_ok(Tctx::#confd_trans_ctx{}) ->
%% ok | {error, Reason::term()}
%% @doc Reply 'ok' for delayed_response.
%% This function can be used explicitly by the erlang application
%% if a data callback returns the atom delayed_response. In that
%% case it is the responsibility of the application to later
%% invoke one of the data_reply_xxx() functions. If delayed_response is
%% not used, none of the explicit data replying functions need to be used.
data_reply_ok(Tctx) ->
    data_reply_value(Tctx, 0).

%% @spec data_reply_value(Tctx::#confd_trans_ctx{}, V::value()) ->
%% ok | {error, Reason::term()}
%% @doc Reply a value for delayed_response.
%% This function can be used explicitly by the erlang application
%% if a data callback returns the atom delayed_response. In that
%% case it is the responsibility of the application to later
%% invoke one of the data_reply_xxx() functions. If delayed_response is
%% not used, none of the explicit data replying functions need to be used.
data_reply_value(Tctx, V) ->
    Dx = Tctx#confd_trans_ctx.dx,
    if Tctx#confd_trans_ctx.lastop == ?CONFD_PROTO_CALLBACK,
        Tctx#confd_trans_ctx.last_proto_op == ?CONFD_VALIDATE_VALUE ->
            trans_put(Dx, thvalidate, Tctx);
       true ->
            trans_put(Dx, th, Tctx)
    end,
    R = {?CONFD_PROTO_CALLBACK, Tctx#confd_trans_ctx.query_ref,
         Dx#confd_daemon_ctx.daemon_id, V},
    term_write(?wsock(Tctx),R).

%% @spec data_reply_next_key(Tctx::#confd_trans_ctx{}, Key,
%%                           Next::term()) ->
%% ok | {error, Reason::term()}
%% Key = key() | false
%% @doc Reply with next key for delayed_response.
%% Like data_reply_value() - only used in combination with delayed_response.
data_reply_next_key(Tctx, Key, Next)  ->
    %% FIXME in_num_instances
    Dx = Tctx#confd_trans_ctx.dx,
    trans_put(Dx, th, Tctx),
    ReplyVal =
        if Key == false ->
                false;
           Next == -1 ->
                {-1, Key};
           true ->
                NextInt = ets:update_counter(confd_next_map_from_int, incr, 1),
                Ekey = {Tctx#confd_trans_ctx.thandle, NextInt},
                ets:insert(confd_next_map_from_int, {Ekey, Next, [NextInt]}),
                {NextInt, Key}
        end,
    R = {?CONFD_PROTO_CALLBACK, Tctx#confd_trans_ctx.query_ref,
         Dx#confd_daemon_ctx.daemon_id, ReplyVal},
    term_write(?wsock(Tctx),R).

%% @spec data_reply_not_found(Tctx::#confd_trans_ctx{}) ->
%% ok | {error, Reason::term()}
%% @doc Reply 'not found' for delayed_response.
%% Like data_reply_value() - only used in combination with delayed_response.
data_reply_not_found(Tctx) ->
    data_reply_value(Tctx, not_found).

%% @spec data_reply_found(Tctx::#confd_trans_ctx{}) ->
%% ok | {error, Reason::term()}
%% @doc Reply 'found' for delayed_response.
%% Like data_reply_value() - only used in combination with delayed_response.
data_reply_found(Tctx) ->
    data_reply_value(Tctx, 1).

%% @spec data_reply_value_array(Tctx::#confd_trans_ctx{}, Values) ->
%% ok | {error, Reason::term()}
%% Values = [V::value()] | {exml, [TV::tagval()]}
%% @doc Reply a list of values for delayed_response.
%% Like data_reply_value() - only used in combination with delayed_response,
%% and get_object() callback.
data_reply_value_array(Tctx,Values) ->
    Dx = Tctx#confd_trans_ctx.dx,
    trans_put(Dx, th, Tctx),
    R = {?CONFD_PROTO_CALLBACK, Tctx#confd_trans_ctx.query_ref,
         Dx#confd_daemon_ctx.daemon_id,Values},
    term_write(?wsock(Tctx),R).

%% @spec data_reply_tag_value_array(Tctx::#confd_trans_ctx{},
%%                                  TagVals::[tagval()]) ->
%% ok | {error, Reason::term()}
%% @doc Reply a list of tagged values for delayed_response.
%% Like data_reply_value() - only used in combination with delayed_response,
%% and get_object() callback.
data_reply_tag_value_array(Tctx,Values) ->
    data_reply_value_array(Tctx,{exml,Values}).


%% @spec data_reply_next_object_value_array(Tctx::#confd_trans_ctx{}, Values,
%%                                          Next::term()) ->
%% ok | {error, Reason::term()}
%% Values = [V::value()] | {exml, [TV::tagval()]} | false
%% @doc Reply with values and next key for delayed_response.
%% Like data_reply_value() - only used in combination with delayed_response,
%% and get_next_object() callback.
data_reply_next_object_value_array(Tctx, Values, Next)  ->
    %% FIXME in_num_instances
    Dx = Tctx#confd_trans_ctx.dx,
    trans_put(Dx, th, Tctx),
    ReplyVal =
        if Values == false ->
                false;
           Next == -1 ->
                {-1, Values};
           true ->
                NextInt = ets:update_counter(confd_next_map_from_int, incr, 1),
                Ekey = {Tctx#confd_trans_ctx.thandle, NextInt},
                ets:insert(confd_next_map_from_int, {Ekey, Next, [NextInt]}),
                {NextInt, Values}
        end,
    R = {?CONFD_PROTO_CALLBACK, Tctx#confd_trans_ctx.query_ref,
         Dx#confd_daemon_ctx.daemon_id, ReplyVal},
    term_write(?wsock(Tctx),R).

%% @spec data_reply_next_object_tag_value_array(Tctx::#confd_trans_ctx{},
%%                                              [TV::tagval()], Next::term()) ->
%% ok | {error, Reason::term()}
%% @doc Reply with tagged values and next key for delayed_response.
%% Like data_reply_value() - only used in combination with delayed_response,
%% and get_next_object() callback.
data_reply_next_object_tag_value_array(Tctx, Values, Next)  ->
    data_reply_next_object_value_array(Tctx, {exml, Values}, Next).

%% @spec data_reply_next_object_value_arrays(Tctx::#confd_trans_ctx{}, Objects,
%%                                           TimeoutMillisecs::integer()) ->
%% ok | {error, Reason::term()}
%% Objects = [ValObject | TagValObject | false]
%% ValObject = {[V::value], Next::term()}
%% TagValObject = {{exml, [TV::tagval()]}, Next::term()}
%% @doc Reply with multiple objects, each with values and next key, plus
%% cache timeout, for delayed_response.
%% Like data_reply_value() - only used in combination with delayed_response,
%% and get_next_object() callback.
data_reply_next_object_value_arrays(Tctx, Objects, TimeoutMillisecs)  ->
    %% FIXME in_num_instances
    Dx = Tctx#confd_trans_ctx.dx,
    trans_put(Dx, th, Tctx),
    ReplyVal =
        if Objects == false ->
                false;
           true ->
                TH = Tctx#confd_trans_ctx.thandle,
                {_, ReplyObjects} = process_next_objects(Objects, [], TH),
                {ReplyObjects, TimeoutMillisecs}
        end,
    R = {?CONFD_PROTO_CALLBACK, Tctx#confd_trans_ctx.query_ref,
         Dx#confd_daemon_ctx.daemon_id, ReplyVal},
    term_write(?wsock(Tctx),R).

process_next_objects([{Values, -1}|Rest], Ints0, TH) ->
    {Ints, ReplyObjects} = process_next_objects(Rest, Ints0, TH),
    {Ints, [{-1, Values}|ReplyObjects]};
process_next_objects([{Values, Next}|Rest], Ints0, TH) ->
    NextInt = ets:update_counter(confd_next_map_from_int, incr, 1),
    {Ints, ReplyObjects} = process_next_objects(Rest, [NextInt|Ints0], TH),
    Ekey = {TH, NextInt},
    ets:insert(confd_next_map_from_int, {Ekey, Next, Ints}),
    {Ints, [{NextInt, Values}|ReplyObjects]};
%% only last object may be 'false'
process_next_objects([false], Ints, _TH) ->
    {Ints, [false]};
process_next_objects([], Ints, _TH) ->
    {Ints, []}.

%% @spec data_reply_error(Tctx::#confd_trans_ctx{}, Error::error_reason()) ->
%% ok | {error, Reason::term()}
%% @doc Reply an error for delayed_response.
%% Like data_reply_value() - only used in combination with delayed_response.
data_reply_error(Tctx, Error) ->
    Dx = Tctx#confd_trans_ctx.dx,
    Fd = if (Tctx#confd_trans_ctx.lastop == ?CONFD_PROTO_NEW_TRANS) or
            (Tctx#confd_trans_ctx.lastop == ?CONFD_PROTO_NEW_VALIDATE) ->
                 Dx#confd_daemon_ctx.ctl;
            true ->
                 Dx#confd_daemon_ctx.worker
         end,
    R = {Tctx#confd_trans_ctx.lastop,
         Tctx#confd_trans_ctx.query_ref,
         Dx#confd_daemon_ctx.daemon_id,
         error,
         econfd_daemon:mk_error(Error)},
    term_write(Fd, R).

%% @spec data_set_timeout(Tctx::#confd_trans_ctx{}, Seconds::integer()) ->
%% ok | {error, Reason::term()}
%% @doc Extend (or shorten) the timeout for the current callback invocation.
%% The timeout is given in seconds from the point in time when the function
%% is called.
data_set_timeout(Tctx, Seconds) ->
    Dx=Tctx#confd_trans_ctx.dx,
    R = {?CONFD_PROTO_CALLBACK_TIMEOUT, Tctx#confd_trans_ctx.query_ref,
         Dx#confd_daemon_ctx.daemon_id, Seconds},
    term_write(?wsock(Tctx), R).


%% @spec action_set_timeout(Uinfo::#confd_user_info{}, Seconds::integer()) ->
%% ok | {error, Reason::term()}
%% @doc Extend (or shorten) the timeout for the current action callback
%% invocation. The timeout is given in seconds from the point in time when
%% the function is called.
action_set_timeout(Uinfo, Seconds) ->
    Actx = Uinfo#confd_user_info.actx,
    Dx = Actx#confd_action_ctx.dx,
    R = {?CONFD_PROTO_CALLBACK_TIMEOUT, Actx#confd_action_ctx.query_ref,
         Dx#confd_daemon_ctx.daemon_id, Seconds},
    term_write(Dx#confd_daemon_ctx.worker, R).


%% @spec notification_send(Nctx::#confd_notification_ctx{},
%%                         DateTime::datetime(),
%%                         TagVals::[tagval()]) ->
%% ok | {error, Reason::term()}
%% @doc Send an XML notification.
notification_send(Nctx, DateTime, ValueList) ->
    Tup = {exml, ValueList},
    Term = {?CONFD_PROTO_NOTIF_SEND,
            Nctx#confd_notification_ctx.streamname,
            case Nctx#confd_notification_ctx.subid of
                0 -> undefined;
                _ -> Nctx#confd_notification_ctx.subid
            end,
            DateTime,
            Nctx#confd_notification_ctx.flags,
            Tup},
    Sock = Nctx#confd_notification_ctx.notif_worker,
    term_write(Sock, Term, ?CONFD_PROTO_REQUEST).

%% @spec notification_replay_complete(
%%     Nctx::#confd_notification_ctx{}) ->
%% ok | {error, Reason::term()}
%% @doc Call this function when replay is done
notification_replay_complete(Nctx) ->
    T = {?CONFD_PROTO_NOTIF_REPLAY_COMPLETE,
         Nctx#confd_notification_ctx.streamname,
         Nctx#confd_notification_ctx.subid},
    Sock = Nctx#confd_notification_ctx.notif_worker,
    term_write(Sock, T, ?CONFD_PROTO_REQUEST).

%% @spec notification_replay_failed(
%%     Nctx::#confd_notification_ctx{}, ErrorString::binary()) ->
%% ok | {error, Reason::term()}
%% @doc Call this function when replay has failed for some reason
notification_replay_failed(Nctx, ErrorString) when is_binary(ErrorString) ->
    T = {?CONFD_PROTO_NOTIF_REPLAY_FAILED,
         Nctx#confd_notification_ctx.streamname,
         Nctx#confd_notification_ctx.subid,
         ErrorString
        },
    Sock = Nctx#confd_notification_ctx.notif_worker,
    term_write(Sock, T, ?CONFD_PROTO_REQUEST).


%% @spec pp_value(V::value()) -> io_list()
%% @doc Pretty print a value.
pp_value(T) when is_tuple(T) and
                 ((size(T) == 4) or (size(T)==8)) ->
    inet_parse:ntoa(T);
pp_value(B) when is_binary(B) -> io_lib:format("~s", [?b2l(B)]);
pp_value({_Tag,Val}) ->
    io_lib:format("~p",[Val]);  %% FIXME all datetaypes .. etc
pp_value(V) ->
    io_lib:format("~p",[V]).

%% @spec pp_kpath(IKP::ikeypath()) -> io_list()
%% @doc Pretty print an ikeypath.
pp_kpath(IKP) ->
    [[_Ns|V] | Rest] = lists:reverse(IKP),
    lists:flatten(pp_kpath2([V|Rest])).

pp_kpath2([Last]) ->
    pp_path_value(Last);
pp_kpath2([V|Vs]) ->
    [pp_path_value(V) | pp_kpath2(Vs)].
pp_path_value([_Ns|Val])-> pp_path_value(Val);
pp_path_value(V) when is_tuple(V) ->
    StringForm = [pp_value(Key) || Key <- ?t2l(V)],
    Keys = string:join(StringForm, " "),
    "{" ++ Keys ++ "}";
pp_path_value(V) when is_atom(V) -> "/" ++ atom_to_list(V).


-spec decrypt(binary()) ->
                     {ok, binary()} |
                     {error, {Ecode :: integer(), Reason :: binary()}}.
%% @doc Decrypts a value of type tailf:des3-cbc-encrypted-string
%% or tailf:aes-cfb-128-encrypted-string. Requires that
%% econfd_maapi:install_crypto_keys/1 has been called in the node.
decrypt(<<"$7$", Base64EncryptedBinary/binary>>) ->
    case ets:lookup(confd_installed_crypto_keys, des3) of
        [{_, Key1, Key2, Key3, _DummyIVec}] ->
            <<IVec:8/binary, EncryptedBinary/binary>> =
                base64:decode(Base64EncryptedBinary),
            IoData = crypto:block_decrypt(des3_cbc, [Key1, Key2, Key3], IVec,
                                          EncryptedBinary),
            {ok, unpad(iolist_to_binary(IoData))};
        _ ->
            {error, {?CONFD_ERR_NOEXISTS, <<"No des3-cbc keys available">>}}
    end;
decrypt(<<"$8$", Base64EncryptedBinary/binary>>) ->
    case ets:lookup(confd_installed_crypto_keys, aes128) of
        [{_, Key, _DummyIVec}] ->
            <<IVec:16/binary, EncryptedBinary/binary>> =
                base64:decode(Base64EncryptedBinary),
            IoData = crypto:block_decrypt(aes_cfb128, Key, IVec,
                                          EncryptedBinary),
            {ok, unpad(iolist_to_binary(IoData))};
        _ ->
            {error, {?CONFD_ERR_NOEXISTS, <<"No aes-cfb-128 keys available">>}}
    end;
decrypt(<<"$3$", Base64EncryptedBinary/binary>>) ->
    %% old-style with fixed ivec, "shouldn't happen"
    case ets:lookup(confd_installed_crypto_keys, des3) of
        [{_, Key1, Key2, Key3, IVec}] ->
            EncryptedBinary = base64:decode(Base64EncryptedBinary),
            IoData = crypto:block_decrypt(des3_cbc, [Key1, Key2, Key3], IVec,
                                          EncryptedBinary),
            {ok, unpad(iolist_to_binary(IoData))};
        _ ->
            {error, {?CONFD_ERR_NOEXISTS, <<"No des3-cbc keys available">>}}
    end;
decrypt(<<"$4$", Base64EncryptedBinary/binary>>) ->
    %% old-style with fixed ivec, "shouldn't happen"
    case ets:lookup(confd_installed_crypto_keys, aes128) of
        [{_, Key, IVec}] ->
            EncryptedBinary = base64:decode(Base64EncryptedBinary),
            IoData = crypto:block_decrypt(aes_cfb128, Key, IVec,
                                          EncryptedBinary),
            {ok, unpad(iolist_to_binary(IoData))};
        _ ->
            {error, {?CONFD_ERR_NOEXISTS, <<"No aes-cfb-128 keys available">>}}
    end.

unpad(<<0, _/binary>>) ->
    <<>>;
unpad(<<>>) ->
    <<>>;
unpad(<<H, T/binary>>) ->
    UT = unpad(T),
    <<H, UT/binary>>.


-spec bitbig_set_bit(binary(), integer()) -> binary().
%% @doc Set a bit in a C_BITBIG binary.
bitbig_set_bit(Binary, Position) ->
    Bitmask = bitbig_bin2bm(Binary) bor (1 bsl Position),
    bitbig_pad(bitbig_bm2bin(Bitmask), size(Binary)).

-spec bitbig_clr_bit(binary(), integer()) -> binary().
%% @doc Clear a bit in a C_BITBIG binary.
bitbig_clr_bit(Binary, Position) ->
    Bitmask = bitbig_bin2bm(Binary) band (bnot (1 bsl Position)),
    bitbig_pad(bitbig_bm2bin(Bitmask), size(Binary)).

-spec bitbig_bit_is_set(binary(), integer()) -> boolean().
%% @doc Test a bit in a C_BITBIG binary.
bitbig_bit_is_set(Binary, Position) ->
    (bitbig_bin2bm(Binary) band (1 bsl Position)) =/= 0.

bitbig_pad(Binary, Size) when size(Binary) < Size ->
    %% pad to original size
    <<Binary/binary, 0:((Size - size(Binary)) * 8)>>;
bitbig_pad(Binary, _Size) ->
    Binary.

bitbig_bin2bm(Binary) ->
    binary:decode_unsigned(Binary, little).

bitbig_bm2bin(Bitmask) ->
    binary:encode_unsigned(Bitmask, little).


-spec log(Level::integer(), Fmt::string()) -> ok.
%% @doc Logs Fmt to devel.log if running internal, otherwise to
%% standard out. Level can be one of ?CONFD_LEVEL_ERROR |
%% ?CONFD_LEVEL_INFO | ?CONFD_LEVEL_TRACE
log(Level, Fmt) ->
    log(Level, Fmt, []).

-spec log(Level::integer(), Fmt::string(), Args::list()) -> ok.
%% @doc Logs Fmt with Args to devel.log if running internal,
%% otherwise to standard out. Level can be one of
%% ?CONFD_LEVEL_ERROR | ?CONFD_LEVEL_INFO | ?CONFD_LEVEL_TRACE
log(Level, Fmt, Args) ->
    econfd_internal:log(Level, Fmt, Args).

-spec log(IoDevice::io:device(), Level::integer(),
          Fmt::string(), Args::list()) -> ok.
%% @doc Logs Fmt with Args to devel.log if running internal,
%% otherwise to IoDevice. Level can be one of
%% ?CONFD_LEVEL_ERROR | ?CONFD_LEVEL_INFO | ?CONFD_LEVEL_TRACE
log(IoDevice, Level, Fmt, Args) ->
    econfd_internal:log(IoDevice, Level, Fmt, Args).

%% @spec controlling_process(Socket::term(), Pid::pid()) ->
%%               ok | {error, Reason::term()}
%% @doc Assigns a new controlling process Pid to Socket
controlling_process(Socket, Pid) ->
    econfd_internal:controlling_process(Socket, Pid).


%% @spec register_service_cb(Daemon::pid(), ServiceCbs::#ncs_service_cbs{})  ->
%% ok | {error, Reason::term()}
%% @private Register NCS service callbacks.
register_service_cb(Daemon, ServiceCbs)
  when is_atom(ServiceCbs#ncs_service_cbs.servicepoint)  ->
    gen_server:call(Daemon, {register_service_cb, ServiceCbs}).


%% @spec register_nano_service_cb(Daemon::pid(),
%%                                Component::binary(), State::binary(),
%%                                ServiceCbs::#ncs_service_cbs{})  ->
%% ok | {error, Reason::term()}
%% @private Register NCS service callbacks.
register_nano_service_cb(Daemon, Component, State, NanoServiceCbs)
  when is_atom(NanoServiceCbs#ncs_nano_service_cbs.servicepoint)  ->
    gen_server:call(Daemon,
                    {register_nano_service_cb,
                     Component, State, NanoServiceCbs}).


%% @spec mk_uinfo(tuple()) -> #confd_user_info{}
%% @doc convert user info tuple received from ConfD
%% @private
mk_uinfo({USid, User, Pass, Ctx, Proto, Ip, Port,
          LoginTime, SnmpV3Ctx, Flags}) ->
    mk_uinfo({USid, User, Pass, Ctx, Proto, Ip, Port,
              LoginTime, SnmpV3Ctx, Flags}, #confd_user_info{}).

%% @private
mk_uinfo({USid, User, Pass, Ctx, Proto, Ip, Port,
          LoginTime, SnmpV3Ctx, Flags}, OldUinfo) ->
    OldUinfo#confd_user_info{usid = USid, username = User, clearpass = Pass,
                             context = Ctx, proto = Proto, ip = Ip, port = Port,
                             logintime = LoginTime, snmp_v3_ctx = SnmpV3Ctx,
                             flags = Flags}.


%% @private
trans_put(Dx, Type, Tctx) ->
    %% don't keep a copy of the daemon_ctx in each trans_ctx
    ets:insert(Dx#confd_daemon_ctx.transactions,
               {{Type, Tctx#confd_trans_ctx.thandle},
                Tctx#confd_trans_ctx{dx = undefined}}).

%% @private
trans_get(Dx, Type, TH) ->
    %% set daemon_ctx in fetched trans_ctx
    case ets:lookup(Dx#confd_daemon_ctx.transactions, {Type, TH}) of
        [{_, Tctx}] -> Tctx#confd_trans_ctx{dx = Dx};
        _           -> undefined
    end.

%% @private
trans_erase(Dx, Type, TH) ->
    ets:delete(Dx#confd_daemon_ctx.transactions, {Type, TH}).


%% @private
report_err(_Dx, _Level, Fmt, Args) ->
    error_logger:format(Fmt, Args).

%% @hidden
doc_application(Args) ->
    Ret = edoc_run:application(Args),
    case Ret of
        ok -> halt(0);
        error -> halt(1)
    end.
