-module(swagger_user_api).

-export([create_user/1, create_user/2,
         create_users_with_array_input/1, create_users_with_array_input/2,
         create_users_with_list_input/1, create_users_with_list_input/2,
         delete_user/1, delete_user/2,
         get_user_by_name/1, get_user_by_name/2,
         login_user/2, login_user/3,
         logout_user/0, logout_user/1,
         update_user/2, update_user/3]).

-define(BASE_URL, <<"http://petstore.swagger.io/v2">>).

%% @doc Create user
%% This can only be done by the logged in user.
-spec create_user(swagger_user:swagger_user(), term()) -> ok | {error, integer()}.
create_user(Body) ->
    create_user(Body, Body, #{}).

-spec create_user(swagger_user:swagger_user(), term(), maps:map()) -> ok | {error, integer()}.
create_user(Body, Body, _Optional) ->
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
-spec create_users_with_array_input(list(), term()) -> ok | {error, integer()}.
create_users_with_array_input(Body) ->
    create_users_with_array_input(Body, Body, #{}).

-spec create_users_with_array_input(list(), term(), maps:map()) -> ok | {error, integer()}.
create_users_with_array_input(Body, Body, _Optional) ->
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
-spec create_users_with_list_input(list(), term()) -> ok | {error, integer()}.
create_users_with_list_input(Body) ->
    create_users_with_list_input(Body, Body, #{}).

-spec create_users_with_list_input(list(), term(), maps:map()) -> ok | {error, integer()}.
create_users_with_list_input(Body, Body, _Optional) ->
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
    delete_user(Username, , #{}).

-spec delete_user(binary(), maps:map()) -> ok | {error, integer()}.
delete_user(Username, _Optional) ->
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
    get_user_by_name(Username, , #{}).

-spec get_user_by_name(binary(), maps:map()) -> {ok, list(), swagger_user:swagger_user()} | {error, string()}.
get_user_by_name(Username, _Optional) ->
    Method = get,
    Path = ["/user/", Username, ""],
    QS = [],
    Headers = [],
    Body1 = [],
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, RespHeaders, ClientRef} ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            {ok, RespHeaders, jsx:decode(ResponseBody, [return_maps])}; 
        {ok, 400, _RespHeaders, _ClientRef} ->
            {error, "Invalid username supplied"}; 
        {ok, 404, _RespHeaders, _ClientRef} ->
            {error, "User not found"}
    end.

%% @doc Logs user into the system
-spec login_user(binary(), binary()) -> {ok, list(), binary()} | {error, string()}.
login_user(Username, Password) ->
    login_user(Username, Password, , #{}).

-spec login_user(binary(), binary(), maps:map()) -> {ok, list(), binary()} | {error, string()}.
login_user(Username, Password, _Optional) ->
    Method = get,
    Path = ["/user/login"],
    QS = lists:flatten([{<<"username">>, Username}, {<<"password">>, Password}])++[{X, maps:get(X, _Optional)} || X <- [], maps:is_key(X, _Optional)],
    Headers = [],
    Body1 = [],
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, RespHeaders, ClientRef} ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            {ok, RespHeaders, jsx:decode(ResponseBody, [return_maps])}; 
        {ok, 400, _RespHeaders, _ClientRef} ->
            {error, "Invalid username/password supplied"}
    end.

%% @doc Logs out current logged in user session
-spec logout_user() -> ok | {error, integer()}.
logout_user() ->
    logout_user(, #{}).

-spec logout_user(maps:map()) -> ok | {error, integer()}.
logout_user(_Optional) ->
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
-spec update_user(binary(), swagger_user:swagger_user(), term()) -> ok | {error, integer()}.
update_user(Username, Body) ->
    update_user(Username, Body, Body, #{}).

-spec update_user(binary(), swagger_user:swagger_user(), term(), maps:map()) -> ok | {error, integer()}.
update_user(Username, Body, Body, _Optional) ->
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


