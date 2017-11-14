-module(swagger_user_api).

-export([create_user/1,
         create_users_with_array_input/1,
         create_users_with_list_input/1,
         delete_user/1,
         get_user_by_name/1,
         login_user/2,
         logout_user/0,
         update_user/2]).

-define(BASE_URL, <<"http://petstore.swagger.io/v2">>).

%% @doc Create user
%% This can only be done by the logged in user.
-spec create_user(swagger_user:swagger_user()) -> ok | {error, integer()}.
create_user(Body) ->
    Method = post,
    Path = ["/user"],
    QS = [],
    Headers = [],
    Body1 = Body,
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, _RespHeaders, _ClientRef} ->
             ok;
        {ok, Status, _RespHeaders, _ClientRef} ->
             {error, Status}
    end.

%% @doc Creates list of users with given input array
-spec create_users_with_array_input(list()) -> ok | {error, integer()}.
create_users_with_array_input(Body) ->
    Method = post,
    Path = ["/user/createWithArray"],
    QS = [],
    Headers = [],
    Body1 = Body,
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, _RespHeaders, _ClientRef} ->
             ok;
        {ok, Status, _RespHeaders, _ClientRef} ->
             {error, Status}
    end.

%% @doc Creates list of users with given input array
-spec create_users_with_list_input(list()) -> ok | {error, integer()}.
create_users_with_list_input(Body) ->
    Method = post,
    Path = ["/user/createWithList"],
    QS = [],
    Headers = [],
    Body1 = Body,
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, _RespHeaders, _ClientRef} ->
             ok;
        {ok, Status, _RespHeaders, _ClientRef} ->
             {error, Status}
    end.

%% @doc Delete user
%% This can only be done by the logged in user.
-spec delete_user(binary()) -> ok | {error, integer()}.
delete_user(Username) ->
    Method = delete,
    Path = ["/user/", Username, ""],
    QS = [],
    Headers = [],
    Body1 = [],
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, _RespHeaders, _ClientRef} ->
             ok;
        {ok, Status, _RespHeaders, _ClientRef} ->
             {error, Status}
    end.

%% @doc Get user by user name
-spec get_user_by_name(binary()) -> {ok, list(), swagger_user:swagger_user()} | {error, string()}.
get_user_by_name(Username) ->
    Method = get,
    Path = ["/user/", Username, ""],
    QS = [],
    Headers = [],
    Body1 = [],
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, RespHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            {ok, RespHeaders, jsx:decode(Body, [returns_maps, {labels, attempt_atom}])}; 
        {ok, 400, _RespHeaders, _ClientRef} ->
            {error, "Invalid username supplied"}; 
        {ok, 404, _RespHeaders, _ClientRef} ->
            {error, "User not found"}
    end.

%% @doc Logs user into the system
-spec login_user(binary(), binary()) -> {ok, list(), binary()} | {error, string()}.
login_user(Username, Password) ->
    Method = get,
    Path = ["/user/login"],
    QS = lists:flatten([{<<"username">>, Username}, {<<"password">>, Password}]),
    Headers = [],
    Body1 = [],
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, RespHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            {ok, RespHeaders, jsx:decode(Body, [returns_maps, {labels, attempt_atom}])}; 
        {ok, 400, _RespHeaders, _ClientRef} ->
            {error, "Invalid username/password supplied"}
    end.

%% @doc Logs out current logged in user session
-spec logout_user() -> ok | {error, integer()}.
logout_user() ->
    Method = get,
    Path = ["/user/logout"],
    QS = [],
    Headers = [],
    Body1 = [],
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, _RespHeaders, _ClientRef} ->
             ok;
        {ok, Status, _RespHeaders, _ClientRef} ->
             {error, Status}
    end.

%% @doc Updated user
%% This can only be done by the logged in user.
-spec update_user(binary(), swagger_user:swagger_user()) -> ok | {error, integer()}.
update_user(Username, Body) ->
    Method = put,
    Path = ["/user/", Username, ""],
    QS = [],
    Headers = [],
    Body1 = Body,
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, _RespHeaders, _ClientRef} ->
             ok;
        {ok, Status, _RespHeaders, _ClientRef} ->
             {error, Status}
    end.


