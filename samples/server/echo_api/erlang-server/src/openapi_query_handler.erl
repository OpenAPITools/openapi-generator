%% basic handler
-module(openapi_query_handler).

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
allowed_methods(Req, #state{operation_id = 'TestEnumRefString'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'TestQueryDatetimeDateString'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'TestQueryIntegerBooleanString'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'TestQueryStyleDeepObjectExplodeTrueObject'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'TestQueryStyleDeepObjectExplodeTrueObjectAllOf'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'TestQueryStyleFormExplodeFalseArrayInteger'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'TestQueryStyleFormExplodeFalseArrayString'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'TestQueryStyleFormExplodeTrueArrayString'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'TestQueryStyleFormExplodeTrueObject'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'TestQueryStyleFormExplodeTrueObjectAllOf'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, State) ->
    {[], Req, State}.

-spec is_authorized(cowboy_req:req(), state()) ->
    {true | {false, iodata()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
    {true, Req, State}.

-spec content_types_accepted(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, #state{operation_id = 'TestEnumRefString'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'TestQueryDatetimeDateString'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'TestQueryIntegerBooleanString'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'TestQueryStyleDeepObjectExplodeTrueObject'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'TestQueryStyleDeepObjectExplodeTrueObjectAllOf'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'TestQueryStyleFormExplodeFalseArrayInteger'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'TestQueryStyleFormExplodeFalseArrayString'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'TestQueryStyleFormExplodeTrueArrayString'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'TestQueryStyleFormExplodeTrueObject'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'TestQueryStyleFormExplodeTrueObjectAllOf'} = State) ->
    {[], Req, State};
content_types_accepted(Req, State) ->
    {[], Req, State}.

-spec valid_content_headers(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
valid_content_headers(Req, #state{operation_id = 'TestEnumRefString'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'TestQueryDatetimeDateString'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'TestQueryIntegerBooleanString'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'TestQueryStyleDeepObjectExplodeTrueObject'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'TestQueryStyleDeepObjectExplodeTrueObjectAllOf'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'TestQueryStyleFormExplodeFalseArrayInteger'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'TestQueryStyleFormExplodeFalseArrayString'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'TestQueryStyleFormExplodeTrueArrayString'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'TestQueryStyleFormExplodeTrueObject'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'TestQueryStyleFormExplodeTrueObjectAllOf'} = State) ->
    {true, Req, State};
valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, #state{operation_id = 'TestEnumRefString'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'TestQueryDatetimeDateString'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'TestQueryIntegerBooleanString'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'TestQueryStyleDeepObjectExplodeTrueObject'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'TestQueryStyleDeepObjectExplodeTrueObjectAllOf'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'TestQueryStyleFormExplodeFalseArrayInteger'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'TestQueryStyleFormExplodeFalseArrayString'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'TestQueryStyleFormExplodeTrueArrayString'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'TestQueryStyleFormExplodeTrueObject'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'TestQueryStyleFormExplodeTrueObjectAllOf'} = State) ->
    {[
      {<<"text/plain">>, handle_type_provided}
     ], Req, State};
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
    {Res, Req1, Context1} = Handler(query, OperationID, Req, Context),
    {Res, Req1, State#state{context = Context1}}.

-spec handle_type_provided(cowboy_req:req(), state()) ->
    {cowboy_req:resp_body(), cowboy_req:req(), state()}.
handle_type_provided(Req, #state{operation_id = OperationID,
                                 provide_callback = Handler,
                                 context = Context} = State) ->
    {Res, Req1, Context1} = Handler(query, OperationID, Req, Context),
    {Res, Req1, State#state{context = Context1}}.
