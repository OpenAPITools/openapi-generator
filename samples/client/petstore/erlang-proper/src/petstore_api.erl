-module(petstore_api).

-export([ create_user/1
        , create_users_with_array_input/1
        , create_users_with_list_input/1
        , delete_user/1
        , get_user_by_name/1
        , login_user/2
        , logout_user/0
        , update_user/2
        ]).

-define(BASE_URL, "/v2").

%% @doc Create user
%% This can only be done by the logged in user.
-spec create_user(petstore_user:petstore_user()) ->
  petstore_utils:response().
create_user(PetstoreUser) ->
  Method      = post,
  Host        = application:get_env(petstore, host, "http://localhost:8080"),
  Path        = ["/user"],
  Body        = PetstoreUser,
  ContentType = hd(["application/json"]),

  petstore_utils:request(Method, [Host, ?BASE_URL, Path], jsx:encode(Body), ContentType).

%% @doc Creates list of users with given input array
%% 
-spec create_users_with_array_input(list(petstore_user:petstore_user())) ->
  petstore_utils:response().
create_users_with_array_input(PetstoreUserArray) ->
  Method      = post,
  Host        = application:get_env(petstore, host, "http://localhost:8080"),
  Path        = ["/user/createWithArray"],
  Body        = PetstoreUserArray,
  ContentType = hd(["application/json"]),

  petstore_utils:request(Method, [Host, ?BASE_URL, Path], jsx:encode(Body), ContentType).

%% @doc Creates list of users with given input array
%% 
-spec create_users_with_list_input(list(petstore_user:petstore_user())) ->
  petstore_utils:response().
create_users_with_list_input(PetstoreUserArray) ->
  Method      = post,
  Host        = application:get_env(petstore, host, "http://localhost:8080"),
  Path        = ["/user/createWithList"],
  Body        = PetstoreUserArray,
  ContentType = hd(["application/json"]),

  petstore_utils:request(Method, [Host, ?BASE_URL, Path], jsx:encode(Body), ContentType).

%% @doc Delete user
%% This can only be done by the logged in user.
-spec delete_user(binary()) ->
  petstore_utils:response().
delete_user(Username) ->
  Method      = delete,
  Host        = application:get_env(petstore, host, "http://localhost:8080"),
  Path        = ["/user/", Username, ""],

  petstore_utils:request(Method, [Host, ?BASE_URL, Path]).

%% @doc Get user by user name
%% 
-spec get_user_by_name(binary()) ->
  petstore_utils:response().
get_user_by_name(Username) ->
  Method      = get,
  Host        = application:get_env(petstore, host, "http://localhost:8080"),
  Path        = ["/user/", Username, ""],

  petstore_utils:request(Method, [Host, ?BASE_URL, Path]).

%% @doc Logs user into the system
%% 
-spec login_user(binary(), binary()) ->
  petstore_utils:response().
login_user(Username, Password) ->
  Method      = get,
  Host        = application:get_env(petstore, host, "http://localhost:8080"),
  Path        = ["/user/login"],
  QueryString = [<<"username=">>, Username, <<"&">>, <<"password=">>, Password, <<"&">>],

  petstore_utils:request(Method, [Host, ?BASE_URL, Path, <<"?">>, QueryString]).

%% @doc Logs out current logged in user session
%% 
-spec logout_user() ->
  petstore_utils:response().
logout_user() ->
  Method      = get,
  Host        = application:get_env(petstore, host, "http://localhost:8080"),
  Path        = ["/user/logout"],

  petstore_utils:request(Method, [Host, ?BASE_URL, Path]).

%% @doc Updated user
%% This can only be done by the logged in user.
-spec update_user(binary(), petstore_user:petstore_user()) ->
  petstore_utils:response().
update_user(Username, PetstoreUser) ->
  Method      = put,
  Host        = application:get_env(petstore, host, "http://localhost:8080"),
  Path        = ["/user/", Username, ""],
  Body        = PetstoreUser,
  ContentType = hd(["application/json"]),

  petstore_utils:request(Method, [Host, ?BASE_URL, Path], jsx:encode(Body), ContentType).

