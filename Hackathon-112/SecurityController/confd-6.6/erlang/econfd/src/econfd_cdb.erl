%%%-------------------------------------------------------------------
%%% @copyright 2006 Tail-F Systems AB
%%% @version {$Id$}
%%% @doc An Erlang interface equivalent to the CDB C-API (documented
%%% in confd_lib_cdb(3)).
%%%
%%% The econfd_cdb library is used to connect to the  ConfD  built  in
%%% XML  database,  CDB. The purpose of this API to provide a read and
%%% subscription API to CDB.
%%%
%%% CDB owns and stores the configuration data and  the  user  of  the  API
%%% wants  to read that configuration data and also get notified when someone
%%% through either NETCONF, the CLI, the Web UI, or MAAPI  modifies
%%% the data so that the application can re-read the configuration data and
%%% act accordingly.
%%%
%%% == Paths ==
%%%
%%% In the C lib a path is a string. Assume the following
%%% YANG fragment:
%%%
%%% ```
%%%       container hosts {
%%%         list host {
%%%           key name;
%%%           leaf name {
%%%             type string;
%%%           }
%%%           leaf domain {
%%%             type string;
%%%           }
%%%           leaf defgw {
%%%             type inet:ip-address;
%%%           }
%%%           container interfaces {
%%%             list interface {
%%%               key name;
%%%               leaf name {
%%%                 type string;
%%%               }
%%%               leaf ip {
%%%                 type inet:ip-address;
%%%               }
%%%               leaf mask {
%%%                 type inet:ip-address;
%%%               }
%%%               leaf enabled {
%%%                 type boolean;
%%%               }
%%%             }
%%%           }
%%%         }
%%%       }
%%% '''
%%% Furthermore assume the database is populated with the following data
%%% ```
%%%           <hosts xmlns="http://acme.com/ns/hst/1.0">
%%%              <host>
%%%                <name>buzz</name>
%%%                <domain>tail-f.com</domain>
%%%                <defgw>192.168.1.1</defgw>
%%%                <interfaces>
%%%                  <interface>
%%%                    <name>eth0</name>
%%%                    <ip>192.168.1.61</ip>
%%%                    <mask>255.255.255.0</mask>
%%%                    <enabled>true</enabled>
%%%                  </interface>
%%%                  <interface>
%%%                    <name>eth1</name>
%%%                    <ip>10.77.1.44</ip>
%%%                    <mask>255.255.0.0</mask>
%%%                    <enabled>false</enabled>
%%%                  </interface>
%%%                </interfaces>
%%%              </host>
%%%            </hosts>
%%% '''
%%%
%%% The format path "/hosts/host{buzz}/defgw" refers to the leaf element
%%% called defgw of the host whose key (name element) is buzz.
%%%
%%% The format path "/hosts/host{buzz}/interfaces/interface{eth0}/ip"
%%% refers to the leaf element called "ip" in the "eth0" interface
%%% of the host called "buzz".
%%%
%%% In the Erlang CDB and MAAPI interfaces we use ikeypath() lists instead
%%% to address individual objects in the XML tree. The IkeyPath is backwards,
%%% thus the two above paths are expressed as
%%% ```
%%% [defgw, {<<"buzz">>}, host, [NS|hosts]]
%%% [ip, {<<"eth0">>}, interface, interfaces, {<<"buzz">>}, host, [NS|hosts]]
%%%
%%% '''
%%%
%%%  It is possible loop through all entries in a list as in:
%%% <pre>
%%% N = econfd_cdb:num_instances(CDB, [host,[NS|hosts]]),
%%% lists:map(fun(I) ->
%%%             econfd_cdb:get_elem(CDB, [defgw,[I],host,[NS|hosts]]), .......
%%%           end, lists:seq(0, N-1))
%%% </pre>
%%% Thus in the list with length N [Index] is an implicit key during the
%%% life of a CDB read session.
%%% @end
%%%-------------------------------------------------------------------

-module(econfd_cdb).

%%% external exports

-export([connect/0, connect/1, connect/2, connect/3,
         wait_start/1,
         get_phase/1,
         new_session/2,
         new_session/3,
         subscribe_session/1,
         end_session/1,
         close/1]).

-export([
         get_elem/2,
         num_instances/2,
         next_index/2,
         index/2,
         exists/2,
         get_case/3,
         get_object/2,
         get_objects/4,
         get_values/3,
         subscribe/3,
         subscribe/4,
         subscribe/5,
         subscribe/6,
         subscribe_done/1,
         wait/3,
         diff_iterate/5, cli_diff_iterate/5,
         get_modifications_cli/2, get_modifications_cli/3,
         trigger_subscriptions/1, trigger_subscriptions/2,
         trigger_oper_subscriptions/1, trigger_oper_subscriptions/2,
         trigger_oper_subscriptions/3,
         get_txid/1
        ]).


%% cdb operational data API
-export([set_elem/3,
         set_elem2/3,
         create/2,
         delete/2,
         set_object/3,
         set_values/3,
         set_case/4]).

-export([mop_2_str/1]).

%%% Internal exports
-export([choice_path/1]).

-import(lists, [reverse/1]).

-include("econfd_cdb_api.hrl").
-include("econfd_cdb.hrl").
-include("econfd_internal.hrl").
-include("../include/econfd.hrl").
-include("../include/econfd_errors.hrl").

%%% types

-type cdb_sess() :: #cdb_session{}.
%% A datastructure which is used as a handle to all the of the access
%% functions

-type subscription_sync_type() :: ?CDB_DONE_PRIORITY | ?CDB_DONE_SOCKET |
                                  ?CDB_DONE_TRANSACTION | ?CDB_DONE_OPERATIONAL.
%% Return value from the fun passed to wait/3,
%% indicating what to do with further notifications coming from
%% this transaction. These ints are defined in econfd.hrl

-type dbtype() :: ?CDB_RUNNING | ?CDB_STARTUP |
                  ?CDB_OPERATIONAL | ?CDB_PRE_COMMIT_RUNNING.
%% When we open CDB sessions we must choose which database to read
%% or write from/to. These ints are defined in econfd.hrl

-type err() :: {error, {integer(), binary()}} |
               {error, closed}.
%% Errors can be either
%% <ul><li> {error, Ecode::integer(), Reason::binary()} where Ecode is
%% one of the error codes defined in econfd_errors.hrl, and Reason is
%% (possibly empty) textual description </li>
%% <li> {error, closed} if the socket gets closed </li></ul>

%%%--------------------------------------------------------------------
%%% External functions
%%%--------------------------------------------------------------------

%%% Setup and tear down

%% @spec connect() -> {ok, Socket::term()} | {error, Reason::term()}
%% @equiv connect({127,0,0,1})
connect() ->
    connect({127,0,0,1}).

%% @spec connect(Address::ip()) ->
%%     {ok, Socket::term()} | {error, Reason::term()}
%% @doc Connect to CDB on the host with address Address
connect(Address) ->
    connect(Address, ?CONFD_PORT).

%% @spec connect(Address::ip(), Port::integer()) ->
%%     {ok, Socket::term()} | {error, Reason::term()}
%% @doc Connect to CDB on the host with address Address:Port
%%
%% If the port is changed it must also be changed in confd.conf
connect(Address, Port) ->
    ClientName = list_to_binary(io_lib:format("econfd ~w", [self()])),
    connect(Address, Port, ClientName).

%% @spec connect(Address::ip(), Port::integer(), ClientName::binary()) ->
%%     {ok, Socket::term()} | {error, Reason::term()}
%% @doc Connect to CDB on the host with address Address:Port
%%
%% If the port is changed it must also be changed in confd.conf
%%  A  call  to  cdb_connect()  is  typically  followed by a call to either
%%        new_session() for a reading session or a call to subscribe_session()
%%        for a subscription socket or calls to any of the write API functions
%% for a data socket.
%% ClientName is a string which confd will use as an identifier when
%% e.g. reporting status.
connect(Address, Port, ClientName) when is_binary(ClientName) ->
    case econfd_internal:connect(Address, Port, ?CLIENT_CDB, []) of
        {ok, Socket} ->
            %% Pass a name to ConfD for use when reporting status
            econfd_internal:bin_write(Socket,
                                      <<?OP_CLIENT_NAME:32,
                                        ClientName/binary>>),
            {ok, Socket};
        Error ->
            Error
    end.

%% @spec (Socket::term()) -> ok | err()
%% @doc Wait for CDB to become available (reach start-phase one)
wait_start(Socket) ->
    request(#cdb_session{ socket = Socket }, ?OP_WAIT_START).

%% @spec (Socket::term()) ->
%%       {ok, {0|1|2, false|init|upgrade}} | err()
%% @doc Get CDB start-phase
get_phase(Socket) ->
    request(#cdb_session{ socket = Socket }, ?OP_GET_PHASE).

%% @spec (Socket::term()) ->
%%       {ok, {MasterNode::term(), Now::tuple()}} | {ok, Now::tuple()}
%% @doc Get CDB transaction id.
%% When we are a cdb client, and ConfD restarts, we can use this function
%% to retrieve the last CDB transaction id. If it the same as earlier
%% we don't need re-read the CDB data. This is also useful when we're
%% a CDB client in a HA setup.

get_txid(Socket) ->
     request(#cdb_session{ socket = Socket }, ?OP_GET_TXID).


-spec trigger_subscriptions(Socket::term()) -> ok | err().
%% @equiv trigger_subscriptions(Socket, all)
trigger_subscriptions(Socket) ->
    trigger_subscriptions(Socket, all).

-spec trigger_subscriptions(Socket::term(), [SubPoint::integer()] | 'all') ->
                                   ok | err().
%% @doc Trigger CDB subscribers as if an update in the configuration
%%      had been done.
trigger_subscriptions(Socket, all) ->
    trigger_subscriptions(Socket, []);
trigger_subscriptions(Socket, SubPoints) ->
    request(#cdb_session{ socket = Socket }, ?OP_TRIGGER_SUBS, SubPoints).

-spec trigger_oper_subscriptions(Socket :: term()) -> ok | err().
%% @equiv trigger_oper_subscriptions(Socket, all)
trigger_oper_subscriptions(Socket) ->
    trigger_oper_subscriptions(Socket, all).

-spec trigger_oper_subscriptions(Socket :: term(),
                                 Points :: [SubPoint::integer()] | 'all') ->
                                        ok | err().
%% @equiv trigger_oper_subscriptions(Socket, Points, 0)
trigger_oper_subscriptions(Socket, Points) ->
    trigger_oper_subscriptions(Socket, Points, 0).

-spec trigger_oper_subscriptions(Socket :: term(),
                                 [SubPoint::integer()] | 'all',
                                 Flags :: integer()) ->
                                        ok | err().
%% @doc Trigger CDB operational subscribers as if an update in oper data
%%      had been done. Flags can be given as ?CDB_LOCK_WAIT to have the
%%      call wait until the subscription lock becomes available, otherwise
%%      it should be 0.
trigger_oper_subscriptions(Socket, all, Flags) ->
    trigger_oper_subscriptions(Socket, [], Flags);
trigger_oper_subscriptions(Socket, SubPoints, Flags) ->
    request(#cdb_session{ socket = Socket }, ?OP_TRIGGER_OPER_SUBS,
            {SubPoints, Flags}).

-spec new_session(Socket::term(), Db::dbtype()) ->
                         {ok, cdb_sess()} | err().
%% @doc Initiate a new session using the socket returned by
%% connect/1 or connect/2.
new_session(Socket, DbType) ->
    if (DbType == ?CDB_RUNNING) orelse
       (DbType == ?CDB_STARTUP) ->
            new_session(Socket, DbType, ?CDB_LOCK_SESSION);
       (DbType == ?CDB_OPERATIONAL) orelse
       (DbType == ?CDB_PRE_COMMIT_RUNNING) ->
            new_session(Socket, DbType, 0)
    end.

-spec new_session(Socket::term(), Db::dbtype(), Flags::integer()) ->
                         {ok, cdb_sess()} | err().
%% @doc Initiate a new session using the socket returned by
%% connect/1 or connect/2, with detailed control via the Flags argument.
new_session(Socket, DbType, Flags) ->
    CDB = #cdb_session{ socket = Socket },
    Opts = [useikp] ++
        if (Flags band ?CDB_LOCK_SESSION) =/= 0 -> [lock_session];
           true                                 -> []
        end ++
        if (Flags band ?CDB_LOCK_REQUEST) =/= 0 -> [lock_request];
           true                                 -> []
        end ++
        if (Flags band ?CDB_LOCK_PARTIAL) =/= 0 -> [lock_partial];
           true                                 -> []
        end ++
        if (Flags band ?CDB_LOCK_WAIT) =/= 0    -> [lock_wait];
           true                                 -> []
        end,
    case request(CDB, ?OP_NEW_SESSION, {DbType, Opts}) of
        ok ->
            {ok, CDB};
        Err ->
            Err
    end.

%% @spec subscribe_session(Socket::term()) ->
%%         {ok, cdb_sess()} | err()
%% @doc Initialize a subscription socket.
%% This is a socket that is used to receive notifications about
%%                  updates to the database. A subscription socket
%% is used in the subscribe() function.
subscribe_session(Socket) ->
    {ok, #cdb_session{ socket = Socket }}.

%% @spec end_session(cdb_sess()) -> {ok, Socket::term()}
%% @doc Terminate the session. This releases the lock on CDB which is active
%%      during a read session.
%%  Returns a socket that can be re-used in new_session/2
%% We  use  connect()  to  establish a read socket to CDB. When the
%%   socket is closed, the read session is ended. We can reuse  the  same
%%   socket  for  another  read session, but we must then end the session
%%   and create another session using new_session/2.
%% %%
%%   While we have a live CDB read session, CDB is  locked  for  writing.
%%   Thus  all external entities trying to modify CDB are blocked as long
%%   as we have an open CDB read session. It is very  important  that  we
%%   remember  to  either  end_session()  or close() once we have
%%   read what we wish to read.
end_session(CDB) ->
    ok = request(CDB, ?OP_END_SESSION),
    {ok, CDB#cdb_session.socket}.

%% @spec close(Socket::term()|cdb_sess()) -> ok | {error, Reason::term()}
%% @doc End the session and close the socket
close(#cdb_session{ socket = Socket }) when Socket /= undefined ->
    close(Socket);
close(Socket) when Socket /= undefined ->
    econfd_internal:close(Socket).


%% @spec get_elem(cdb_sess(), econfd:ikeypath()) ->
%% {ok, Value::econfd:value()} | err()
%% @doc Read an element.
%% Note, the C interface has separate get
%%      functions for different types.
get_elem(CDB, IKP) ->
    request(CDB, ?OP_GET, reverse(IKP)).

-spec exists(cdb_sess(), econfd:ikeypath()) ->
                    {ok, boolean()} | err().
%% @doc Checks existense of an object.
%% Leafs in the data model may be optional, and presence containers and
%% list entries may or may not exist. This function checks whether a node
%% exists in CDB, returning Int == 1 if it exists, Int == 0 if not.
exists(CDB, IKP) ->
    ibool(request(CDB, ?OP_EXISTS, reverse(IKP))).

%% @spec num_instances(cdb_sess(), econfd:ikeypath()) ->
%%              {ok, integer()} | err()
%% @doc Returns the number of  entries in  a  list.
num_instances(CDB, IKP) ->
    request(CDB, ?OP_NUM_INSTANCES, reverse(IKP)).

%% @spec (cdb_sess(), econfd:ikeypath()) ->
%%              {ok, integer()} | err()
%% @doc Returns the position (starting at 0) of the list entry after
%% the given path (which can be non-existing, and if multiple keys the
%% last keys can be '*').
next_index(CDB, IKP) ->
    request(CDB, ?OP_NXT_INDEX, reverse(IKP)).

%% @spec (cdb_sess(), econfd:ikeypath()) ->
%%              {ok, integer()} | err()
%% @doc Returns the position (starting at 0) of the list entry in path.
index(CDB, IKP) ->
    request(CDB, ?OP_NXT_INDEX, reverse(IKP)).

-spec get_case(cdb_sess(), econfd:ikeypath(),
               econfd:qtag() | [econfd:qtag()]) ->
                      {ok, econfd:qtag()} | err().
%% @doc Returns the current case for a choice.
get_case(CDB, IKP, Choice) ->
    request(CDB, ?OP_GET_CASE, {choice_path(Choice), reverse(IKP)}).

%% @spec get_object(cdb_sess(), econfd:ikeypath()) ->
%%              {ok, [econfd:value()]} | err()
%% @doc Returns all the values in a container or list entry.
get_object(CDB, IKP) ->
    request(CDB, ?OP_GET_OBJECT, reverse(IKP)).

%% @spec get_objects(cdb_sess(), econfd:ikeypath(), integer(), integer()) ->
%%              {ok, [[econfd:value()]]} | err()
%% @doc Returns all the values for NumEntries list entries,
%% starting at index StartIndex. The return value has one Erlang list for
%% each YANG list entry, i.e. it is a list of NumEntries lists.
get_objects(CDB, IKP, StartIndex, NumEntries) ->
    request(CDB, ?OP_GET_OBJECTS, {reverse(IKP), StartIndex, NumEntries}).

%% @spec get_values(cdb_sess(), econfd:ikeypath(), [econfd:tagval()]) ->
%%              {ok, [econfd:tagval()]} | err()
%% @doc Returns the values for the leafs that have the "value" 'not_found'
%% in the Values list. This can be used to read an arbitrary set of
%% sub-elements of a container or list entry. The return value is a list
%% of the same length as Values, i.e. the requested leafs are in the same
%% position in the returned list as in the Values argument. The elements
%% in the returned list are always "canonical" though, i.e. of the form
%% {[Ns|Tag], value() | start | stop | leaf}.
get_values(CDB, IKP, Values) ->
    request(CDB, ?OP_GET_VALUES, {Values, reverse(IKP)}).


%% @spec set_elem(cdb_sess(), econfd:value(), econfd:ikeypath()) ->
%%              ok | err()
%% @doc Only for CDB operational data:
%% Write Value into CDB.
set_elem(CDB, Value, IKP) ->
    request(CDB, ?OP_SET_ELEM, {Value, reverse(IKP)}).

%% @spec set_elem2(cdb_sess(), binary(), econfd:ikeypath()) ->
%%              ok | err()
%% @doc Only for CDB operational data:
%% Write ValueBin into CDB. ValueBin is the textual value representation.
set_elem2(CDB, ValueBin, IKP) ->
    request(CDB, ?OP_SET_ELEM2, {ValueBin, reverse(IKP)}).

%% @spec create(cdb_sess(), econfd:ikeypath()) ->
%%              ok | err()
%% @doc Only for CDB operational data:
%% Create the element denoted by IKP.
create(CDB, IKP) ->
    request(CDB, ?OP_CREATE, reverse(IKP)).

%% @spec delete(cdb_sess(), econfd:ikeypath()) ->
%%              ok | err()
%% @doc Only for CDB operational data:
%% Delete the element denoted by IKP.
delete(CDB, IKP) ->
    request(CDB, ?OP_DELETE, reverse(IKP)).

%% @spec set_object(cdb_sess(), [econfd:value()], econfd:ikeypath()) ->
%%              ok | err()
%% @doc Only for CDB operational data: Write an entire object,
%% i.e. YANG list entry or container.
set_object(CDB, ValueList, IKP) ->
    request(CDB, ?OP_SET_OBJECT, {ValueList, reverse(IKP)}).

%% @spec set_values(cdb_sess(), [econfd:tagval()],
%%                  econfd:ikeypath()) ->
%%              ok | err()
%% @doc Only for CDB operational data: Write a list of tagged values.
%% This function is an alternative to
%% set_object/3, and allows for writing more complex structures
%% (e.g. multiple entries in a list).
set_values(CDB, ValueList, IKP) ->
    request(CDB, ?OP_SET_VALUES, {ValueList, reverse(IKP)}).

-spec set_case(cdb_sess(), econfd:ikeypath(),
               econfd:qtag() | [econfd:qtag()], econfd:qtag()) ->
                      ok | err().
%% @doc Only for CDB operational data: Set the case for a choice.
set_case(CDB, IKP, Choice, Case) ->
    request(CDB, ?OP_SET_CASE, {{choice_path(Choice), Case}, reverse(IKP)}).



%%% Subscription Interface

-type sub_ns() :: econfd:namespace() | ''.
%% A namespace or use '' as wildcard (any namespace)

-spec subscribe(cdb_sess(), Prio::integer(),
                sub_ns(), MatchKeyString::string()) ->
                       {ok, PointNumber::integer()} | err().
%% @doc Set up a CDB configuration subscription.
%%
%% A CDB subscription means that we are notified when CDB changes.
%% We can have multiple subscription points. Each subscription point
%% is defined through a path corresponding to the paths we use for read
%% operations, however they are in string form and allow formats that
%% aren't possible in a proper ikeypath(). It is possible to indicate
%% namespaces in the path with a prefix notation (see last example) -
%% this is only necessary if there are multiple elements with the same
%% name (in different namespaces) at some level in the path, though.
%%
%% We can subscribe either to specific leaf elements or entire
%% subtrees. Subscribing to list entries can be done using fully
%% qualified paths, or tagpaths to match multiple entries. A
%% path which isn't a leaf element automatically matches the subtree
%% below that path. When specifying keys to a list entry it is
%% possible to use the wildcard character * which will match any key
%% value.
%%
%% Some examples:
%%
%% <ul>
%% <li>
%%   /hosts
%%  <p>
%%   Means that we subscribe to any changes in the subtree - rooted at
%%   "/hosts". This includes additions or removals of "host" entries as well
%%   as changes to already existing "host" entries.
%%  </p>
%% </li>
%% <li>
%%   /hosts/host{www}/interfaces/interface{eth0}/ip
%%  <p>
%%   Means we are notified when host "www" changes its IP address on
%%   "eth0".
%%  </p>
%% </li>
%% <li>
%%   /hosts/host/interfaces/interface/ip
%%  <p>
%%   Means we are notified when any host changes any of its IP addresses.
%%  </p>
%% </li>
%% <li>
%%   /hosts/host/interfaces
%%  <p>
%%   Means we are notified when either an interface
%%   is added/removed or when an individual leaf element in an
%%   existing interface is changed.
%%  </p>
%% </li>
%% <li>
%%   /hosts/host/types:data
%%  <p>
%%   Means we are notified when any host changes the contents of its
%%   "data" element, where "data" is an element from a namespace with
%%   the prefix "types". The prefix is normally not necessary, see above.
%%  </p>
%% </li>
%% </ul>
%%
%% The priority value is an integer. When CDB  is  changed,  the
%% change  is  performed  inside  a transaction. Either a commit
%% operation from the CLI or  a  candidate-commit  operation  in
%% NETCONF  means  that  the  running database is changed. These
%% changes occur inside a ConfD transaction. CDB will handle the
%% subscriptions  in  lock-step  priority  order. First all
%% subscribers at the lowest priority are handled,  once  they  all
%% have synchronized via the return value from the fun passed to
%% wait/3, the next set - at the next priority  level -
%% is handled by CDB.
%%
%% The namespace argument specifies the toplevel namespace, i.e.
%% the namespace for the first element in the path. The namespace is
%% optional, 0 can be used as "don't care" value.
%%
%% subscribe()  returns  a  subscription point which is an integer.
%% This integer value is used later in wait/3 to identify this
%% particular subscription.
%%
subscribe(CDB, Prio, Ns, MatchKeyString) ->
    MIKP = parse_keystring(MatchKeyString),
    request(CDB, ?OP_SUBSCRIBE, {Prio, Ns, {MIKP, true}}).


-spec subscribe(cdb_sess(), Prio::integer(), MatchKeyString::string()) ->
                       {ok, PointNumber::integer()} | err().
%% @equiv subscribe(CDB, Prio, 0, MatchKeyString)
subscribe(CDB, Prio, MatchKeyString) ->
    subscribe(CDB, Prio, '', MatchKeyString).

-type sub_type() :: ?CDB_SUB_RUNNING | ?CDB_SUB_RUNNING_TWOPHASE |
                    ?CDB_SUB_OPERATIONAL.
%% Subscription type
%% <ul><li>
%% ?CDB_SUB_RUNNING - commit subscription.
%% </li><li>
%% ?CDB_SUB_RUNNING_TWOPHASE - two phase subscription, i.e. notification
%% will be received for prepare, commit, and possibly abort.
%% </li><li>
%% ?CDB_SUB_OPERATIONAL - subscription for changes to CDB operational data.
%% </li></ul>


-spec subscribe(cdb_sess(), Type::sub_type(), Prio::integer(),
                sub_ns(), MatchKeyString::string()) ->
                       {ok, PointNumber::integer()} | err().
%% @equiv subscribe(CDB, Type, 0, Prio, Ns, MatchKeyString)
subscribe(CDB, Type, Prio, Ns, MatchKeyString) ->
    subscribe(CDB, Type, 0, Prio, Ns, MatchKeyString).

-spec subscribe(cdb_sess(), Type::sub_type(), Flags::non_neg_integer(),
                Prio::integer(), sub_ns(), MatchKeyString::string()) ->
                       {ok, PointNumber::integer()} | err().
%% @doc Generalized subscription, where Type is one of
%% <ul><li>
%% ?CDB_SUB_RUNNING - traditional commit subscription, same as subscribe/4.
%% </li><li>
%% ?CDB_SUB_RUNNING_TWOPHASE - two phase subscription, i.e. notification
%% will be received for prepare, commit, and possibly abort.
%% </li><li>
%% ?CDB_SUB_OPERATIONAL - subscription for changes to CDB operational data.
%% </li></ul>
%% Flags is either 0 or:
%% <ul><li>
%% ?CDB_SUB_WANT_ABORT_ON_ABORT - normally if a subscriber is the one
%%   to abort a transaction it will not receive an abort
%%   notification. This flags means that this subscriber wants an
%%   abort notification even if it originated the abort.
%% </li></ul>
subscribe(CDB, Type, Flags, Prio, Ns, MatchKeyString) ->
    MIKP = parse_keystring(MatchKeyString),
    request(CDB, ?OP_SUBSCRIBE, {Type, Flags, 0, Prio, Ns, {MIKP, true}}).

%% @spec (cdb_sess()) -> ok | err()
%% @doc After a subscriber is done with all subscriptions and ready to
%%      receive updates this subscribe_done/1 must be called. Until it
%%      is no notifications will be delivered.
subscribe_done(CDB) ->
    request(CDB, ?OP_SUBSCRIBE_DONE).


-spec wait(cdb_sess(), TimeOut :: integer() | infinity,
           fun((Points :: [integer()]) ->
                      close | subscription_sync_type()) |
           fun((Type :: integer(), Flags :: integer(), Points :: [integer()]) ->
                      close | subscription_sync_type() |
                      {error, binary()} | {error, #confd_error{}} |
                      {error, tuple()})) ->
                  ok | {error, timeout} | err().
%% @doc Wait for subscription events.
%%
%% The fun will be given a list of the subscription points that
%% triggered, and in the arity-3 case also Type and Flags for the
%% notification. There can be several points if we have issued several
%% subscriptions at the same priority.
%%
%% Type is one of:
%% <ul><li>
%%    ?CDB_SUB_PREPARE - notification for the prepare phase
%% </li><li>
%%    ?CDB_SUB_COMMIT  - notification for the commit phase
%% </li><li>
%%    ?CDB_SUB_ABORT   - notification for abort when prepare failed
%% </li><li>
%%    ?CDB_SUB_OPER    - notification for changes to CDB operational data
%% </li></ul>
%%
%% Flags is the 'bor' of zero or more of:
%% <ul><li>
%%    ?CDB_SUB_FLAG_IS_LAST - the last notification of its type for this session
%% </li><li>
%%    ?CDB_SUB_FLAG_TRIGGER - the notification was artificially triggered
%% </li><li>
%%    ?CDB_SUB_FLAG_REVERT - the notification is due to revert of a confirmed
%%                           commit
%% </li><li>
%%    ?CDB_SUB_FLAG_HA_SYNC -  the cause of the subscription
%%                             notification is initial synchronization
%%                             of a HA slave from CDB on the master.
%% </li><li>
%%    ?CDB_SUB_FLAG_HA_IS_SLAVE - the system is currently in HA slave mode.
%% </li></ul>
%%
%% The fun can return the atom 'close' if we wish to close the socket and
%% return from wait/3. Otherwise there are three different types of
%% synchronization replies the application can use as return values from
%% either the arity-1 or the arity-3 fun:
%% <ul><li>
%%    ?CDB_DONE_PRIORITY
%%    This means that the application has acted on the subscription
%%    notification and CDB can continue to deliver further  notifications.
%% </li><li>
%%    ?CDB_DONE_SOCKET
%%    This  means that we are done. But regardless of priority, CDB
%%    shall not send any further notifications to us on our  socket
%%    that are related to the currently executing transaction.
%% </li><li>
%%    ?CDB_DONE_TRANSACTION
%%    This means that CDB should not send any further notifications
%%    to any subscribers - including ourselves  -  related  to  the
%%    currently executing transaction.
%% </li><li>
%%    ?CDB_DONE_OPERATIONAL
%%    This should be used when a subscription notification for
%%    operational data has been read. It is the only type that should
%%    be used in this case, since the operational data does not have
%%    transactions and the notifications do not have priorities.
%% </li></ul>
%% Finally the arity-3 fun can, when Type == ?CDB_SUB_PREPARE,
%% return an error either as <tt>{error, binary()}</tt> or as
%% <tt>{error, #confd_error{}}</tt>
%% ({error, tuple()} is only for internal ConfD/NCS use). This will
%% cause the commit of the current transaction to be aborted.
%%
%% CDB is locked for writing while config subscriptions are delivered.
%%
%% When wait/3 returns <tt>{error, timeout}</tt> the connection (and its
%% subscriptions) is still active and the application needs to call
%% wait/3 again. But if wait/3 returns <tt>ok</tt> or
%% <tt>{error, Reason}</tt> the connection to ConfD is closed and all
%% subscription points associated with it are cleared.
wait(CDB, TimeOut, Fun) ->
    case econfd_internal:term_read(CDB#cdb_session.socket,
                                   ?OP_SUB_EVENT, TimeOut) of
        {ok, {Type, Flags, PointList}} ->
            Ret = if is_function(Fun, 3) ->
                          (catch Fun(Type, Flags, PointList));
                     true ->
                          (catch Fun(PointList))
                  end,
            case Ret of
                _ when Ret == ?CDB_DONE_PRIORITY ;
                       Ret == ?CDB_DONE_SOCKET ;
                       Ret == ?CDB_DONE_TRANSACTION;
                       Ret == ?CDB_DONE_OPERATIONAL ->
                    sync_subscription_socket(CDB, Ret, TimeOut, Fun);
                {error, Reason} when Type == ?CDB_SUB_PREPARE ->
                    Abort = {?SYNC_ABORT, econfd_daemon:mk_error(Reason)},
                    sync_subscription_socket(CDB, Abort, TimeOut, Fun);
                close ->
                    {ok, Socket} = end_session(CDB),
                    close(Socket),
                    ok;
                _ ->
                    error_logger:format("bad retval from subscription "
                                        "fun: ~p~n", [Ret]),
                    close(CDB),
                    {error, badretval}
            end;
        {error, timeout} ->
            {error, timeout};
        Err ->
            close(CDB),
            Err
    end.

sync_subscription_socket(CDB, SyncType, TimeOut, Fun) ->
    case request(CDB, ?OP_SYNC_SUB, SyncType) of
        ok ->
            wait(CDB, TimeOut, Fun);
        Err ->
            error_logger:format("CDB sub_sync req: ~p~n", [Err]),
            close(CDB),
            Err
    end.

%% @spec (cdb_sess(), Point::integer(),
%%        Fun::(IKP::econfd:ikeypath(), Op::integer(),
%%              Oval, Eval,
%%              State::term()) ->
%%          {ok, Ret::integer(), State2::term()} | {error, term()},
%%        Flags::integer(),
%%        InitState::term()) -> {ok, State::term()} | {error, term()}
%%    Oval = econfd:value() | undefined
%%    Eval = econfd:value() | undefined | econfd:key() | {}
%% @doc Iterate over changes in CDB after a subscription triggers.
%% This function can be called from within the fun passed to wait/3. When
%% called it will invoke Fun for each change that matched the Point. If
%% Flags is ?CDB_ITER_WANT_PREV, Oval will be the previous value (if
%% available). When Oval or Eval is not available (or requested) they will
%% be the atom 'undefined'.
%% When Op == ?MOP_MOVED_AFTER (only for "ordered-by user" list entry),
%% Eval == {} means that the entry was moved first in the list, otherwise
%% Eval is a econfd:key() tuple that identifies the entry it was moved after.
diff_iterate(CDB, Point, Fun, Flags, InitState) ->
    Socket = CDB#cdb_session.socket,
    econfd_internal:bin_write(Socket,
                              <<?OP_SUB_ITERATE:32, Point:32, Flags:32>>),
    econfd_maapi:iterate_loop(Socket, Fun, InitState).

-spec cli_diff_iterate(
        cdb_sess(), Point :: integer(),
        fun((IKP :: econfd:ikeypath(), Op :: integer(),
             Oval :: econfd:value() | undefined,
             Eval :: econfd:value() | undefined | econfd:key() | {},
             CliStr :: binary(),
             CliTokens :: [{econfd:value(), binary()}],
             State :: term()) ->
                   {ok, Ret :: integer(), State2 :: term()} |
                   {error, term()}),
        Flags :: integer(),
        InitState :: term()) ->
                              {ok, State :: term()} | {error, term()}.
%% @doc Iterate over changes in CDB after a subscription triggers.
%% This function works just like {@link diff_iterate/5}, except the fun
%% will be invoked with two additional arguments, CliStr and CliTokens.
%% CliStr is a string containing the (C-style) rendering of the CLI
%% commands equivalent to the current keypath/operation. CliTokens is
%% a list representing the CLI string broken down by token.
cli_diff_iterate(CDB, Point, Fun, Flags, InitState) ->
    Socket = CDB#cdb_session.socket,
    Flags1 = Flags bor ?CDB_ITER_WANT_CLI_STR,
    econfd_internal:bin_write(Socket,
                              <<?OP_SUB_ITERATE:32, Point:32, Flags1:32>>),
    econfd_maapi:iterate_loop(Socket, Fun, InitState).

-spec get_modifications_cli(cdb_sess(),
                            Point :: integer(), Flags :: integer()) ->
                                   {ok, CliString::binary()} |
                                   {error, term()}.
%% @doc Return Return a string with the CLI commands that corresponds
%% to the changes that triggered subscription.
get_modifications_cli(CDB, Point, Flags) ->
    request(CDB, ?OP_GET_CLI, {Point, Flags}).

get_modifications_cli(CDB, Point) ->
    get_modifications_cli(CDB, Point, 0).


%%%--------------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------

request(CDB, Op) ->
    econfd_internal:confd_call_bin(CDB#cdb_session.socket, <<>>, Op).

request(CDB, Op, Arg) ->
    econfd_internal:confd_call(CDB#cdb_session.socket, Arg, Op).


%% @spec parse_keystring(Str::string()) -> [term()]
%% @private Internal function.
%% @doc It parses a keystring and returns a list that is somewhat
%% similar to an ikeypath() - it is only used for CDB subscriptions.
parse_keystring(Str) ->
    {ok, Res} = parse_keystring0(Str),
    Res.

parse_keystring0(Str) ->
    %% First split at "/"
    SlashSeparated = string:tokens(Str, "/"),

    %% Then go through each element and look for "{}", and "[]"
    IKP = lists:foldl(
                fun (E, Acc) ->
                        xx(skip_ws(E), Acc)
                end, [], SlashSeparated),
    {ok, lists:reverse(IKP)}.


xx(Str, Acc) -> xx(Str, [], Acc).

xx([H|T], Sofar, Acc) when (H == ${) or (H == $[) ->
    %% Sofar is path element
    E = mk_elem(Sofar),
    {'', Y} = yy([H|T]),
    [Y, E | Acc];
xx([H|T], Sofar, Acc) ->
    xx(T, [H|Sofar], Acc);
xx([], Sofar, Acc) ->
    [mk_elem(Sofar)|Acc].


%% yy(Str)
%%   "foo{bar}" -> {foo, {<<"bar">>}}
%%   "{bar}     -> {'',  {<<"bar">>}}
%%   "foo
%%   "foo"      -> foo
yy(Str) -> yy(Str, []).

yy([], Sofar) ->
    mk_elem(Sofar);
yy([$[|T], Sofar) ->
    E = mk_elem(Sofar),
    Z = collect_until(T, $]),
    {E, [list_to_integer(Z)]};
yy([${|T], Sofar) ->
    E = mk_elem(Sofar),
    Z = collect_until(T, $}),
    {E, {list_to_binary(Z)}};
yy([H|T], Sofar) ->
    yy(T, [H|Sofar]).

%% @spec collect_until(string(), Stop::char()) -> string()
%% @doc Skips initial white space, returns input string up to, but
%%      excluding Stop.
collect_until(Str, Stop) ->
    collect_until(skip_ws(Str), Stop, []).

collect_until([Stop|_], Stop, Sofar) ->
    lists:reverse(skip_ws(Sofar));
collect_until([C|T], Stop, Sofar) ->
    collect_until(T, Stop, [C|Sofar]);
collect_until([], _Stop, Sofar) ->
    %% We are lax
    lists:reverse(skip_ws(Sofar)).

%% @spec skip_ws(string()) -> string()
%% @doc  Return string without leading white space.
skip_ws([Ws|T]) when Ws =< 32 ->
    skip_ws(T);
skip_ws(Str) ->
    Str.

mk_elem([]) ->
    '';
mk_elem(List) ->
    case string:tokens(lists:reverse(skip_ws(List)), ":") of
        [Ns, Tag] ->
            [list_to_atom(Ns)|list_to_atom(Tag)];
        [Tag] ->
            list_to_atom(Tag)
    end.

choice_path(Tag) when is_atom(Tag) ->
    [Tag];
choice_path([Ns|Tag]) when is_atom(Ns), is_atom(Tag) ->
    [[Ns|Tag]];
choice_path(Path) ->
    reverse(Path).

ibool({ok, 1}) -> {ok, true};
ibool({ok, 0}) -> {ok, false};
ibool(X) -> X.

mop_2_str(?MOP_CREATED) -> "MOP_CREATED";
mop_2_str(?MOP_DELETED) -> "MOP_DELETED";
mop_2_str(?MOP_MODIFIED) -> "MOP_MODIFIED";
mop_2_str(?MOP_VALUE_SET) -> "MOP_VALUE_SET";
mop_2_str(?MOP_MOVED_AFTER) -> "MOP_MOVED_AFTER".
