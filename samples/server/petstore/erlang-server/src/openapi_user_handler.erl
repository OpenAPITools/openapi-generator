%% basic handler
-module(openapi_user_handler).

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

-record(state,
        {operation_id :: openapi_api:operation_id(),
         accept_callback :: openapi_logic_handler:accept_callback(),
         provide_callback :: openapi_logic_handler:provide_callback(),
         api_key_handler :: openapi_logic_handler:api_key_callback(),
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
                   api_key_handler = fun Module:authorize_api_key/2},
    {cowboy_rest, Req, State}.

-spec allowed_methods(cowboy_req:req(), state()) ->
    {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, #state{operation_id = 'CreateUser'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'CreateUsersWithArrayInput'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'CreateUsersWithListInput'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'DeleteUser'} = State) ->
    {[<<"DELETE">>], Req, State};
allowed_methods(Req, #state{operation_id = 'GetUserByName'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'LoginUser'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'LogoutUser'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'UpdateUser'} = State) ->
    {[<<"PUT">>], Req, State};
allowed_methods(Req, State) ->
    {[], Req, State}.

-spec is_authorized(cowboy_req:req(), state()) ->
    {true | {false, iodata()}, cowboy_req:req(), state()}.
is_authorized(Req0,
              #state{operation_id = 'CreateUser' = OperationID,
                     api_key_handler = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, "authorization", Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'CreateUsersWithArrayInput' = OperationID,
                     api_key_handler = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, "authorization", Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'CreateUsersWithListInput' = OperationID,
                     api_key_handler = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, "authorization", Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'DeleteUser' = OperationID,
                     api_key_handler = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, "authorization", Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'LogoutUser' = OperationID,
                     api_key_handler = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, "authorization", Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'UpdateUser' = OperationID,
                     api_key_handler = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, "authorization", Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req, State) ->
    {true, Req, State}.

-spec content_types_accepted(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, #state{operation_id = 'CreateUser'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'CreateUsersWithArrayInput'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'CreateUsersWithListInput'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'DeleteUser'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'GetUserByName'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'LoginUser'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'LogoutUser'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'UpdateUser'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, State) ->
    {[], Req, State}.

-spec valid_content_headers(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
valid_content_headers(Req, #state{operation_id = 'CreateUser'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'CreateUsersWithArrayInput'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'CreateUsersWithListInput'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'DeleteUser'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'GetUserByName'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'LoginUser'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'LogoutUser'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'UpdateUser'} = State) ->
    {true, Req, State};
valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, #state{operation_id = 'CreateUser'} = State) ->
    {[], Req, State};
content_types_provided(Req, #state{operation_id = 'CreateUsersWithArrayInput'} = State) ->
    {[], Req, State};
content_types_provided(Req, #state{operation_id = 'CreateUsersWithListInput'} = State) ->
    {[], Req, State};
content_types_provided(Req, #state{operation_id = 'DeleteUser'} = State) ->
    {[], Req, State};
content_types_provided(Req, #state{operation_id = 'GetUserByName'} = State) ->
    {[
      {<<"application/xml">>, handle_type_provided},
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'LoginUser'} = State) ->
    {[
      {<<"application/xml">>, handle_type_provided},
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'LogoutUser'} = State) ->
    {[], Req, State};
content_types_provided(Req, #state{operation_id = 'UpdateUser'} = State) ->
    {[], Req, State};
content_types_provided(Req, State) ->
    {[], Req, State}.

-spec delete_resource(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
delete_resource(Req, State) ->
    {Res, Req1, State} = handle_type_accepted(Req, State),
    {true =:= Res, Req1, State}.

-spec handle_type_accepted(cowboy_req:req(), state()) ->
    { openapi_logic_handler:accept_callback_return(), cowboy_req:req(), state()}.
handle_type_accepted(Req, #state{operation_id = OperationID,
                                 accept_callback = Handler,
                                 context = Context} = State) ->
    {Res, Req1, Context1} = Handler(user, OperationID, Req, Context),
    {Res, Req1, State#state{context = Context1}}.

-spec handle_type_provided(cowboy_req:req(), state()) ->
    {cowboy_req:resp_body(), cowboy_req:req(), state()}.
handle_type_provided(Req, #state{operation_id = OperationID,
                                 provide_callback = Handler,
                                 context = Context} = State) ->
    {Res, Req1, Context1} = Handler(user, OperationID, Req, Context),
    {Res, Req1, State#state{context = Context1}}.
