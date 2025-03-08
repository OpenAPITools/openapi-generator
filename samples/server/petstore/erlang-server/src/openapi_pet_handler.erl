-module(openapi_pet_handler).
-moduledoc """
Exposes the following operation IDs:

- `POST` to `/pet`, OperationId: `addPet`:
Add a new pet to the store.


- `DELETE` to `/pet/:petId`, OperationId: `deletePet`:
Deletes a pet.


- `GET` to `/pet/findByStatus`, OperationId: `findPetsByStatus`:
Finds Pets by status.
Multiple status values can be provided with comma separated strings

- `GET` to `/pet/findByTags`, OperationId: `findPetsByTags`:
Finds Pets by tags.
Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

- `GET` to `/pet/:petId`, OperationId: `getPetById`:
Find pet by ID.
Returns a single pet

- `PUT` to `/pet`, OperationId: `updatePet`:
Update an existing pet.


- `POST` to `/pet/:petId`, OperationId: `updatePetWithForm`:
Updates a pet in the store with form data.


- `POST` to `/pet/:petId/uploadImage`, OperationId: `uploadFile`:
uploads an image.


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

-type class() :: 'pet'.

-type operation_id() ::
    'addPet' %% Add a new pet to the store
    | 'deletePet' %% Deletes a pet
    | 'findPetsByStatus' %% Finds Pets by status
    | 'findPetsByTags' %% Finds Pets by tags
    | 'getPetById' %% Find pet by ID
    | 'updatePet' %% Update an existing pet
    | 'updatePetWithForm' %% Updates a pet in the store with form data
    | 'uploadFile'. %% uploads an image


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
allowed_methods(Req, #state{operation_id = 'addPet'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'deletePet'} = State) ->
    {[<<"DELETE">>], Req, State};
allowed_methods(Req, #state{operation_id = 'findPetsByStatus'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'findPetsByTags'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'getPetById'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'updatePet'} = State) ->
    {[<<"PUT">>], Req, State};
allowed_methods(Req, #state{operation_id = 'updatePetWithForm'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'uploadFile'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, State) ->
    {[], Req, State}.

-spec is_authorized(cowboy_req:req(), state()) ->
    {true | {false, iodata()}, cowboy_req:req(), state()}.
is_authorized(Req0,
              #state{operation_id = 'addPet' = OperationID,
                     api_key_callback = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'deletePet' = OperationID,
                     api_key_callback = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'findPetsByStatus' = OperationID,
                     api_key_callback = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'findPetsByTags' = OperationID,
                     api_key_callback = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'getPetById' = OperationID,
                     api_key_callback = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'updatePet' = OperationID,
                     api_key_callback = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'updatePetWithForm' = OperationID,
                     api_key_callback = Handler} = State) ->
    case openapi_auth:authorize_api_key(Handler, OperationID, header, <<"authorization">>, Req0) of
        {true, Context, Req} ->
            {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req0,
              #state{operation_id = 'uploadFile' = OperationID,
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
content_types_accepted(Req, #state{operation_id = 'addPet'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted},
      {<<"application/xml">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'deletePet'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'findPetsByStatus'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'findPetsByTags'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'getPetById'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'updatePet'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted},
      {<<"application/xml">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'updatePetWithForm'} = State) ->
    {[
      {<<"application/x-www-form-urlencoded">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'uploadFile'} = State) ->
    {[
      {<<"multipart/form-data">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, State) ->
    {[], Req, State}.

-spec valid_content_headers(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
valid_content_headers(Req, #state{operation_id = 'addPet'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'deletePet'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'findPetsByStatus'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'findPetsByTags'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'getPetById'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'updatePet'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'updatePetWithForm'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'uploadFile'} = State) ->
    {true, Req, State};
valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, #state{operation_id = 'addPet'} = State) ->
    {[
      {<<"application/xml">>, handle_type_provided},
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'deletePet'} = State) ->
    {[], Req, State};
content_types_provided(Req, #state{operation_id = 'findPetsByStatus'} = State) ->
    {[
      {<<"application/xml">>, handle_type_provided},
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'findPetsByTags'} = State) ->
    {[
      {<<"application/xml">>, handle_type_provided},
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'getPetById'} = State) ->
    {[
      {<<"application/xml">>, handle_type_provided},
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'updatePet'} = State) ->
    {[
      {<<"application/xml">>, handle_type_provided},
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'updatePetWithForm'} = State) ->
    {[], Req, State};
content_types_provided(Req, #state{operation_id = 'uploadFile'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
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
    {Res, Req1, Context1} = Handler(pet, OperationID, Req, Context),
    {Res, Req1, State#state{context = Context1}}.

-spec handle_type_provided(cowboy_req:req(), state()) ->
    { openapi_logic_handler:provide_callback_return(), cowboy_req:req(), state()}.
handle_type_provided(Req, #state{operation_id = OperationID,
                                 provide_callback = Handler,
                                 context = Context} = State) ->
    {Res, Req1, Context1} = Handler(pet, OperationID, Req, Context),
    {Res, Req1, State#state{context = Context1}}.
