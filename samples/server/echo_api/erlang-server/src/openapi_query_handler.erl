-module(openapi_query_handler).
-moduledoc """
Exposes the following operation IDs:

- `GET` to `/query/enum_ref_string`, OperationId: `test/enum_ref_string`:
Test query parameter(s).
Test query parameter(s)

- `GET` to `/query/datetime/date/string`, OperationId: `test/query/datetime/date/string`:
Test query parameter(s).
Test query parameter(s)

- `GET` to `/query/integer/boolean/string`, OperationId: `test/query/integer/boolean/string`:
Test query parameter(s).
Test query parameter(s)

- `GET` to `/query/style_deepObject/explode_true/object`, OperationId: `test/query/style_deepObject/explode_true/object`:
Test query parameter(s).
Test query parameter(s)

- `GET` to `/query/style_deepObject/explode_true/object/allOf`, OperationId: `test/query/style_deepObject/explode_true/object/allOf`:
Test query parameter(s).
Test query parameter(s)

- `GET` to `/query/style_form/explode_false/array_integer`, OperationId: `test/query/style_form/explode_false/array_integer`:
Test query parameter(s).
Test query parameter(s)

- `GET` to `/query/style_form/explode_false/array_string`, OperationId: `test/query/style_form/explode_false/array_string`:
Test query parameter(s).
Test query parameter(s)

- `GET` to `/query/style_form/explode_true/array_string`, OperationId: `test/query/style_form/explode_true/array_string`:
Test query parameter(s).
Test query parameter(s)

- `GET` to `/query/style_form/explode_true/object`, OperationId: `test/query/style_form/explode_true/object`:
Test query parameter(s).
Test query parameter(s)

- `GET` to `/query/style_form/explode_true/object/allOf`, OperationId: `test/query/style_form/explode_true/object/allOf`:
Test query parameter(s).
Test query parameter(s)

- `GET` to `/query/style_jsonSerialization/object`, OperationId: `test/query/style_jsonSerialization/object`:
Test query parameter(s).
Test query parameter(s)

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

-type class() :: 'query'.

-type operation_id() ::
    'test/enum_ref_string' %% Test query parameter(s)
    | 'test/query/datetime/date/string' %% Test query parameter(s)
    | 'test/query/integer/boolean/string' %% Test query parameter(s)
    | 'test/query/style_deepObject/explode_true/object' %% Test query parameter(s)
    | 'test/query/style_deepObject/explode_true/object/allOf' %% Test query parameter(s)
    | 'test/query/style_form/explode_false/array_integer' %% Test query parameter(s)
    | 'test/query/style_form/explode_false/array_string' %% Test query parameter(s)
    | 'test/query/style_form/explode_true/array_string' %% Test query parameter(s)
    | 'test/query/style_form/explode_true/object' %% Test query parameter(s)
    | 'test/query/style_form/explode_true/object/allOf' %% Test query parameter(s)
    | 'test/query/style_jsonSerialization/object'. %% Test query parameter(s)


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
allowed_methods(Req, #state{operation_id = 'test/enum_ref_string'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/query/datetime/date/string'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/query/integer/boolean/string'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/query/style_deepObject/explode_true/object'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/query/style_deepObject/explode_true/object/allOf'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/query/style_form/explode_false/array_integer'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/query/style_form/explode_false/array_string'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/query/style_form/explode_true/array_string'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/query/style_form/explode_true/object'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/query/style_form/explode_true/object/allOf'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'test/query/style_jsonSerialization/object'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, State) ->
    {[], Req, State}.

-spec is_authorized(cowboy_req:req(), state()) ->
    {true | {false, iodata()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
    {true, Req, State}.

-spec content_types_accepted(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, #state{operation_id = 'test/enum_ref_string'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/query/datetime/date/string'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/query/integer/boolean/string'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/query/style_deepObject/explode_true/object'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/query/style_deepObject/explode_true/object/allOf'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/query/style_form/explode_false/array_integer'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/query/style_form/explode_false/array_string'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/query/style_form/explode_true/array_string'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/query/style_form/explode_true/object'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/query/style_form/explode_true/object/allOf'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'test/query/style_jsonSerialization/object'} = State) ->
    {[], Req, State};
content_types_accepted(Req, State) ->
    {[], Req, State}.

-spec valid_content_headers(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
valid_content_headers(Req, #state{operation_id = 'test/enum_ref_string'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/query/datetime/date/string'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/query/integer/boolean/string'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/query/style_deepObject/explode_true/object'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/query/style_deepObject/explode_true/object/allOf'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/query/style_form/explode_false/array_integer'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/query/style_form/explode_false/array_string'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/query/style_form/explode_true/array_string'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/query/style_form/explode_true/object'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/query/style_form/explode_true/object/allOf'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'test/query/style_jsonSerialization/object'} = State) ->
    {true, Req, State};
valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, #state{operation_id = 'test/enum_ref_string'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/query/datetime/date/string'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/query/integer/boolean/string'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/query/style_deepObject/explode_true/object'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/query/style_deepObject/explode_true/object/allOf'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/query/style_form/explode_false/array_integer'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/query/style_form/explode_false/array_string'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/query/style_form/explode_true/array_string'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/query/style_form/explode_true/object'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/query/style_form/explode_true/object/allOf'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'test/query/style_jsonSerialization/object'} = State) ->
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
    {Res, Req1, Context1} = Handler(query, OperationID, Req, Context),
    {Res, Req1, State#state{context = Context1}}.

-spec handle_type_provided(cowboy_req:req(), state()) ->
    { openapi_logic_handler:provide_callback_return(), cowboy_req:req(), state()}.
handle_type_provided(Req, #state{operation_id = OperationID,
                                 provide_callback = Handler,
                                 context = Context} = State) ->
    {Res, Req1, Context1} = Handler(query, OperationID, Req, Context),
    {Res, Req1, State#state{context = Context1}}.
