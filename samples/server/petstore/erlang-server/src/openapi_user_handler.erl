-module(openapi_user_handler).
-moduledoc """
Exposes the following operation IDs:

- `POST` to `/user`, OperationId: `createUser`:
Create user.
This can only be done by the logged in user.

- `POST` to `/user/createWithArray`, OperationId: `createUsersWithArrayInput`:
Creates list of users with given input array.


- `POST` to `/user/createWithList`, OperationId: `createUsersWithListInput`:
Creates list of users with given input array.


- `DELETE` to `/user/:username`, OperationId: `deleteUser`:
Delete user.
This can only be done by the logged in user.

- `GET` to `/user/:username`, OperationId: `getUserByName`:
Get user by user name.


- `GET` to `/user/login`, OperationId: `loginUser`:
Logs user into the system.


- `GET` to `/user/logout`, OperationId: `logoutUser`:
Logs out current logged in user session.


- `PUT` to `/user/:username`, OperationId: `updateUser`:
Updated user.
This can only be done by the logged in user.

""".

-behaviour(cowboy_rest).

-include_lib("kernel/include/logger.hrl").

%% Cowboy REST callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([delete_resource/2]).
-export([is_authorized/2]).
-export([valid_content_headers/2]).
-export([handle_type_accepted/2, handle_type_provided/2]).

-ignore_xref([handle_type_accepted/2, handle_type_provided/2]).

-export_type([class/0, operation_id/0]).

-type class() :: 'user'.

-type operation_id() ::
    'createUser' %% Create user
    | 'createUsersWithArrayInput' %% Creates list of users with given input array
    | 'createUsersWithListInput' %% Creates list of users with given input array
    | 'deleteUser' %% Delete user
    | 'getUserByName' %% Get user by user name
    | 'loginUser' %% Logs user into the system
    | 'logoutUser' %% Logs out current logged in user session
    | 'updateUser'. %% Updated user


-record(state,
        {operation_id :: operation_id(),
         accept_callback :: openapi_logic_handler:accept_callback(),
         provide_callback :: openapi_logic_handler:provide_callback(),
         api_key_callback :: openapi_logic_handler:api_key_callback(),
         context = #{} :: openapi_logic_handler:context()}).

-type state() :: #state{}.

-spec init(cowboy_req:req(), openapi_router:init_opts()) ->
    {cowboy_rest, cowboy_req:req(), state()}.
init(Req, {Operations, Module}) ->
    Method = cowboy_req:method(Req),
    OperationID = maps:get(Method, Operations, undefined),
    ?LOG_INFO(#{what => "Attempt to process operation",
                method => Method,
                operation_id => OperationID}),
    State = #state{operation_id = OperationID,
                   accept_callback = fun Module:accept_callback/4,
                   provide_callback = fun Module:provide_callback/4,
                   api_key_callback = fun Module:api_key_callback/2},
    {cowboy_rest, Req, State}.

-spec allowed_methods(cowboy_req:req(), state()) ->
    {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, #state{operation_id = 'createUser'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'createUsersWithArrayInput'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'createUsersWithListInput'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'deleteUser'} = State) ->
    {[<<"DELETE">>], Req, State};
allowed_methods(Req, #state{operation_id = 'getUserByName'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'loginUser'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'logoutUser'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'updateUser'} = State) ->
    {[<<"PUT">>], Req, State};
allowed_methods(Req, State) ->
    {[], Req, State}.

-spec is_authorized(cowboy_req:req(), state()) ->
    {true | {false, iodata()}, cowboy_req:req(), state()}.
is_authorized(Req0,
              #state{operation_id = 'createUser' = OperationID,
                     api_key_callback = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'createUsersWithArrayInput' = OperationID,
                     api_key_callback = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'createUsersWithListInput' = OperationID,
                     api_key_callback = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'deleteUser' = OperationID,
                     api_key_callback = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'logoutUser' = OperationID,
                     api_key_callback = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'updateUser' = OperationID,
                     api_key_callback = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req, State) ->
    {true, Req, State}.

-spec content_types_accepted(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, #state{operation_id = 'createUser'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'createUsersWithArrayInput'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'createUsersWithListInput'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'deleteUser'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'getUserByName'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'loginUser'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'logoutUser'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'updateUser'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, State) ->
    {[], Req, State}.

-spec valid_content_headers(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
valid_content_headers(Req, #state{operation_id = 'createUser'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'createUsersWithArrayInput'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'createUsersWithListInput'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'deleteUser'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'getUserByName'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'loginUser'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'logoutUser'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'updateUser'} = State) ->
    {true, Req, State};
valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, #state{operation_id = 'createUser'} = State) ->
    {[], Req, State};
content_types_provided(Req, #state{operation_id = 'createUsersWithArrayInput'} = State) ->
    {[], Req, State};
content_types_provided(Req, #state{operation_id = 'createUsersWithListInput'} = State) ->
    {[], Req, State};
content_types_provided(Req, #state{operation_id = 'deleteUser'} = State) ->
    {[], Req, State};
content_types_provided(Req, #state{operation_id = 'getUserByName'} = State) ->
    {[
      {<<"application/xml">>, handle_type_provided},
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'loginUser'} = State) ->
    {[
      {<<"application/xml">>, handle_type_provided},
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'logoutUser'} = State) ->
    {[], Req, State};
content_types_provided(Req, #state{operation_id = 'updateUser'} = State) ->
    {[], Req, State};
content_types_provided(Req, State) ->
    {[], Req, State}.

-spec delete_resource(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
delete_resource(Req, State) ->
    {Res, Req1, State1} = handle_type_accepted(Req, State),
    {true =:= Res, Req1, State1}.

-spec handle_type_accepted(cowboy_req:req(), state()) ->
    { openapi_logic_handler:accept_callback_return(), cowboy_req:req(), state()}.
handle_type_accepted(Req, #state{operation_id = OperationID,
                                 accept_callback = Handler,
                                 context = Context} = State) ->
    {Res, Req1, Context1} = Handler(user, OperationID, Req, Context),
    {Res, Req1, State#state{context = Context1}}.

-spec handle_type_provided(cowboy_req:req(), state()) ->
    { openapi_logic_handler:provide_callback_return(), cowboy_req:req(), state()}.
handle_type_provided(Req, #state{operation_id = OperationID,
                                 provide_callback = Handler,
                                 context = Context} = State) ->
    {Res, Req1, Context1} = Handler(user, OperationID, Req, Context),
    {Res, Req1, State#state{context = Context1}}.
