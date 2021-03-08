-module(ec_users_aaa).

-include("econfd.hrl").
-include("econfd_errors.hrl").
-include("users.hrl").
-include("tailf-aaa.hrl").
-include("ietf-netconf-acm.hrl").

-define(AAA_USER, [user, users, authentication, [?aaa__ns_uri|aaa]]).
-define(AAA_GROUP, [group, groups, [?nacm__ns_uri|nacm]]).

-on_load(on_load/0).

on_load() ->
    proc_lib:spawn(fun start/0),
    ok.

start() ->
    %% for supervision
    process_flag(trap_exit, true),
    start(0).

start(Restarts) ->
    Trans = #confd_trans_cbs{init   = fun init_transformation/1,
                             finish = fun stop_transformation/1},
    Data = #confd_data_cbs{get_elem = fun get_elem/2,
                           get_next = fun get_next/3,
                           set_elem = fun set_elem/3,
                           create   = fun create/2,
                           remove   = fun remove/2,
                           callpoint = ?users__callpointid_simple_aaa},

    {ok, MaapiSock} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    {ok, Daemon} = econfd:init_daemon(users_aaa, ?CONFD_SILENT, user,
                                      MaapiSock, {127,0,0,1}, ?CONFD_PORT),
    ok = econfd:register_trans_cb(Daemon, Trans),
    ok = econfd:register_data_cb(Daemon, Data),
    ok = econfd:register_done(Daemon),
    supervise(Restarts, MaapiSock, Daemon).


%% simple supervision
supervise(Restarts, MaapiSock, Daemon) ->
    receive
        {'EXIT', Daemon, _} when Restarts < 3 ->
            econfd_maapi:close(MaapiSock),
            start(Restarts + 1);
        {'EXIT', Daemon, _} ->
            exit(too_many_restarts)
    after
        timeout(Restarts) ->
            supervise(0, MaapiSock, Daemon)
    end.

timeout(0)         -> infinity;
timeout(_Restarts) -> 10000.


%% Transaction callbacks

init_transformation(Tctx) ->
    MaapiSock = (Tctx#confd_trans_ctx.dx)#confd_daemon_ctx.d_opaque,
    ok = econfd_maapi:attach(MaapiSock, 0, Tctx),
    {ok, Tctx#confd_trans_ctx{opaque = MaapiSock}}.

stop_transformation(Tctx) ->
    econfd_maapi:detach(Tctx#confd_trans_ctx.opaque,
                        Tctx#confd_trans_ctx.thandle),
    ok.


%% Data callbacks

get_elem(Tctx, [name, Key | _Tail]) ->
    maapi_get_elem(Tctx, [name, Key | ?AAA_USER]);
get_elem(Tctx, [password, Key | _Tail]) ->
    maapi_get_elem(Tctx, [password, Key | ?AAA_USER]);
get_elem(Tctx, [role, {?CONFD_BUF(User)} | _Tail]) ->
    NotMemberF = fun ({Group, _Role}) ->
                         not is_group_member(Tctx, User, Group)
                 end,
    GroupRoles = [{<<"admin">>, ?users_admin}, {<<"oper">>, ?users_oper}],
    case lists:dropwhile(NotMemberF, GroupRoles) of
        [{_Group, Role}|_] ->
            {ok, ?CONFD_ENUM_VALUE(Role)};
        _ ->
            {ok, not_found}
    end;
get_elem(_Tctx, [Tag | _Tail]) ->
    internal_error("unexpected get_elem tag ~p", [Tag]).

set_elem(Tctx, [password, Key | _Tail], Value) ->
    maapi_set_elem(Tctx, [password, Key | ?AAA_USER], Value);
set_elem(Tctx, [role, {?CONFD_BUF(User)} = Key | _Tail], Value) ->
    case Value of
        ?CONFD_ENUM_VALUE(?users_admin) ->
            ok([maapi_set_elem2(Tctx, [uid, Key | ?AAA_USER], <<"0">>),
                maapi_set_elem2(Tctx, [gid, Key | ?AAA_USER], <<"0">>),
                add_user_to_group(Tctx, User, <<"admin">>),
                del_user_from_group(Tctx, User, <<"oper">>)]);
        ?CONFD_ENUM_VALUE(?users_oper) ->
            ok([maapi_set_elem2(Tctx, [uid, Key | ?AAA_USER], <<"20">>),
                maapi_set_elem2(Tctx, [gid, Key | ?AAA_USER], <<"20">>),
                add_user_to_group(Tctx, User, <<"oper">>),
                del_user_from_group(Tctx, User, <<"admin">>)]);
        ?CONFD_ENUM_VALUE(Enum) ->
            internal_error("unexpected role enum ~p", [Enum])
    end;
set_elem(_Tctx, [Tag | _Tail], _Value) ->
    internal_error("unexpected set_elem tag ~p", [Tag]).

get_next(Tctx, _IKeypath, -1) ->
    MaapiCursor = maapi_init_cursor(Tctx, ?AAA_USER),
    maapi_get_next(MaapiCursor);
get_next(_Tctx, _IKeypath, MaapiCursor) ->
    maapi_get_next(MaapiCursor).

remove(Tctx,  [{?CONFD_BUF(User)} = Key | _Tail]) ->
    ok([maapi_delete(Tctx, [Key | ?AAA_USER]),
        del_user_from_group(Tctx, User, <<"oper">>),
        del_user_from_group(Tctx, User, <<"admin">>)]).

create(Tctx,  [{?CONFD_BUF(User)} = Key | _Tail]) ->
    ok([maapi_create(Tctx, [Key | ?AAA_USER]),
        maapi_set_elem2(Tctx, [homedir, Key | ?AAA_USER],
                        <<"/var/confd/homes/", User/binary>>),
        maapi_set_elem2(Tctx, [ssh_keydir, Key | ?AAA_USER],
                        <<"/var/confd/homes/", User/binary, "/.ssh">>)]).


%% Utility functions

ok([ok|Tail])     -> ok(Tail);
ok([Error|_Tail]) -> Error;
ok([])            -> ok.

add_user_to_group(Tctx, User, Group) ->
    IKeypath = group_members_path(Group),
    case maapi_get_elem(Tctx, IKeypath) of
        {ok, ?CONFD_LIST(Users)} when is_list(Users) ->
            ok;
        _ ->
            Users = []
    end,
    case lists:member(User, Users) of
        true ->
            ok;
        false ->
            maapi_set_elem(Tctx, IKeypath, ?CONFD_LIST([User|Users]))
    end.

del_user_from_group(Tctx, User, Group) ->
    IKeypath = group_members_path(Group),
    case maapi_get_elem(Tctx, IKeypath) of
        {ok, ?CONFD_LIST(Users)} when is_list(Users) ->
            ok;
        _ ->
            Users = []
    end,
    case lists:member(User, Users) of
        true ->
            maapi_set_elem(Tctx, IKeypath,
                           ?CONFD_LIST(lists:delete(User, Users)));
        false ->
            ok
    end.

is_group_member(Tctx, User, Group) ->
    case maapi_get_elem(Tctx, group_members_path(Group)) of
        {ok, ?CONFD_LIST(Users)} when is_list(Users) ->
            lists:member(User, Users);
        _ ->
            false
    end.

group_members_path(Group) ->
    ['user-name', {?CONFD_BUF(Group)} | ?AAA_GROUP].

maapi_get_elem(#confd_trans_ctx{opaque = MaapiSock, thandle = Thandle},
               IKeypath) ->
    case econfd_maapi:get_elem(MaapiSock, Thandle, IKeypath) of
        {ok, Value} ->
            {ok, Value};
        {error, {?CONFD_ERR_NOEXISTS, _}} ->
            {ok, not_found};
        {error, Error} ->
            internal_error("maapi get_elem returned error ~p", [Error])
    end.

maapi_set_elem(Tctx, IKeypath, Value) ->
    maapi_set(Tctx, set_elem, IKeypath, Value).

maapi_set_elem2(Tctx, IKeypath, Value) ->
    maapi_set(Tctx, set_elem2, IKeypath, Value).

maapi_set(#confd_trans_ctx{opaque = MaapiSock, thandle = Thandle},
          SetF, IKeypath, Value) ->
    case econfd_maapi:SetF(MaapiSock, Thandle, IKeypath, Value) of
        ok ->
            ok;
        {error, Error} ->
            internal_error("maapi ~p returned error ~p", [SetF, Error])
    end.

maapi_init_cursor(#confd_trans_ctx{opaque = MaapiSock, thandle = Thandle},
                  IKeypath) ->
    econfd_maapi:init_cursor(MaapiSock, Thandle, IKeypath).

maapi_get_next(Cursor) ->
    case econfd_maapi:get_next(Cursor) of
        done ->
            {ok, {false, undefined}};
        {ok, Key, NewCursor} ->
            {ok, {Key, NewCursor}};
        {error, Error} ->
            internal_error("maapi get_next returned error ~p", [Error])
    end.

maapi_delete(#confd_trans_ctx{opaque = MaapiSock, thandle = Thandle},
             IKeypath) ->
    case econfd_maapi:delete(MaapiSock, Thandle, IKeypath) of
        ok ->
            ok;
        {error, Error} ->
            internal_error("maapi delete returned error ~p", [Error])
    end.

maapi_create(#confd_trans_ctx{opaque = MaapiSock, thandle = Thandle},
             IKeypath) ->
    case econfd_maapi:create(MaapiSock, Thandle, IKeypath) of
        ok ->
            ok;
        {error, Error} ->
            internal_error("maapi create returned error ~p", [Error])
    end.

internal_error(Fmt, Args) ->
    Msg = io_lib:format(Fmt, Args),
    {error, #confd_error{code = application_internal,
                         str = iolist_to_binary(Msg)}}.
