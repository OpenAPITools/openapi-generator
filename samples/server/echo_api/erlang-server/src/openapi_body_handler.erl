-module(openapi_body_handler).
-moduledoc """
Exposes the following operation IDs:

- `POST` to `/binary/gif`, OperationId: `test/binary/gif`:
Test binary (gif) response body.
Test binary (gif) response body

- `POST` to `/body/application/octetstream/binary`, OperationId: `test/body/application/octetstream/binary`:
Test body parameter(s).
Test body parameter(s)

- `POST` to `/body/application/octetstream/array_of_binary`, OperationId: `test/body/multipart/formdata/array_of_binary`:
Test array of binary in multipart mime.
Test array of binary in multipart mime

- `POST` to `/body/application/octetstream/single_binary`, OperationId: `test/body/multipart/formdata/single_binary`:
Test single binary in multipart mime.
Test single binary in multipart mime

- `POST` to `/echo/body/allOf/Pet`, OperationId: `test/echo/body/allOf/Pet`:
Test body parameter(s).
Test body parameter(s)

- `POST` to `/echo/body/FreeFormObject/response_string`, OperationId: `test/echo/body/FreeFormObject/response_string`:
Test free form object.
Test free form object

- `POST` to `/echo/body/Pet`, OperationId: `test/echo/body/Pet`:
Test body parameter(s).
Test body parameter(s)

- `POST` to `/echo/body/Pet/response_string`, OperationId: `test/echo/body/Pet/response_string`:
Test empty response body.
Test empty response body

- `POST` to `/echo/body/string_enum`, OperationId: `test/echo/body/string_enum`:
Test string enum response body.
Test string enum response body

- `POST` to `/echo/body/Tag/response_string`, OperationId: `test/echo/body/Tag/response_string`:
Test empty json (request body).
Test empty json (request body)

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

-type class() :: 'body'.

-type operation_id() ::
    'test/binary/gif' %% Test binary (gif) response body
    | 'test/body/application/octetstream/binary' %% Test body parameter(s)
    | 'test/body/multipart/formdata/array_of_binary' %% Test array of binary in multipart mime
    | 'test/body/multipart/formdata/single_binary' %% Test single binary in multipart mime
    | 'test/echo/body/allOf/Pet' %% Test body parameter(s)
    | 'test/echo/body/FreeFormObject/response_string' %% Test free form object
    | 'test/echo/body/Pet' %% Test body parameter(s)
    | 'test/echo/body/Pet/response_string' %% Test empty response body
    | 'test/echo/body/string_enum' %% Test string enum response body
    | 'test/echo/body/Tag/response_string'. %% Test empty json (request body)


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
allowed_methods(Req, #state{operation_id = 'test/binary/gif'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/body/application/octetstream/binary'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/body/multipart/formdata/array_of_binary'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/body/multipart/formdata/single_binary'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/echo/body/allOf/Pet'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/echo/body/FreeFormObject/response_string'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/echo/body/Pet'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/echo/body/Pet/response_string'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/echo/body/string_enum'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/echo/body/Tag/response_string'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, State) ->
    {[], Req, State}.

-spec is_authorized(cowboy_req:req(), state()) ->
    {true | {false, iodata()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
    {true, Req, State}.

-spec content_types_accepted(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, #state{operation_id = 'test/binary/gif'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/body/application/octetstream/binary'} = State) ->
    {[
      {<<"application/octet-stream">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/body/multipart/formdata/array_of_binary'} = State) ->
    {[
      {<<"multipart/form-data">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/body/multipart/formdata/single_binary'} = State) ->
    {[
      {<<"multipart/form-data">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/echo/body/allOf/Pet'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/echo/body/FreeFormObject/response_string'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/echo/body/Pet'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/echo/body/Pet/response_string'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/echo/body/string_enum'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/echo/body/Tag/response_string'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, State) ->
    {[], Req, State}.

-spec valid_content_headers(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
valid_content_headers(Req, #state{operation_id = 'test/binary/gif'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/body/application/octetstream/binary'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/body/multipart/formdata/array_of_binary'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/body/multipart/formdata/single_binary'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/echo/body/allOf/Pet'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/echo/body/FreeFormObject/response_string'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/echo/body/Pet'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/echo/body/Pet/response_string'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/echo/body/string_enum'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/echo/body/Tag/response_string'} = State) ->
    {true, Req, State};
valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, #state{operation_id = 'test/binary/gif'} = State) ->
    {[
      {<<"image/gif">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/body/application/octetstream/binary'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/body/multipart/formdata/array_of_binary'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/body/multipart/formdata/single_binary'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/echo/body/allOf/Pet'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/echo/body/FreeFormObject/response_string'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/echo/body/Pet'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/echo/body/Pet/response_string'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/echo/body/string_enum'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/echo/body/Tag/response_string'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
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
    {Res, Req1, Context1} = Handler(body, OperationID, Req, Context),
    {Res, Req1, State#state{context = Context1}}.

-spec handle_type_provided(cowboy_req:req(), state()) ->
    { openapi_logic_handler:provide_callback_return(), cowboy_req:req(), state()}.
handle_type_provided(Req, #state{operation_id = OperationID,
                                 provide_callback = Handler,
                                 context = Context} = State) ->
    {Res, Req1, Context1} = Handler(body, OperationID, Req, Context),
    {Res, Req1, State#state{context = Context1}}.
