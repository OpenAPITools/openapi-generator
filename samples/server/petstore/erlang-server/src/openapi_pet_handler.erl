%% basic handler
-module(openapi_pet_handler).

%% Cowboy REST callbacks
-export([allowed_methods/2]).
-export([init/2]).
-export([allow_missing_post/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([delete_resource/2]).
-export([is_authorized/2]).
-export([known_content_type/2]).
-export([malformed_request/2]).
-export([valid_content_headers/2]).
-export([valid_entity_length/2]).

%% Handlers
-export([handle_request_json/2]).

-record(state, {
    operation_id :: openapi_api:operation_id(),
    logic_handler :: atom(),
    validator_state :: jesse_state:state(),
    context=#{} :: #{}
}).

-type state() :: state().

-spec init(Req :: cowboy_req:req(), Opts :: openapi_router:init_opts()) ->
    {cowboy_rest, Req :: cowboy_req:req(), State :: state()}.

init(Req, {Operations, LogicHandler, ValidatorMod}) ->
    Method = cowboy_req:method(Req),
    OperationID = maps:get(Method, Operations, undefined),

    ValidatorState = ValidatorMod:get_validator_state(),

    error_logger:info_msg("Attempt to process operation: ~p", [OperationID]),

    State = #state{
        operation_id = OperationID,
        logic_handler = LogicHandler,
        validator_state = ValidatorState
    },
    {cowboy_rest, Req, State}.

-spec allowed_methods(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: [binary()], Req :: cowboy_req:req(), State :: state()}.


allowed_methods(
    Req,
    State = #state{
        operation_id = 'AddPet'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'DeletePet'
    }
) ->
    {[<<"DELETE">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'FindPetsByStatus'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'FindPetsByTags'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'GetPetById'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'UpdatePet'
    }
) ->
    {[<<"PUT">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'UpdatePetWithForm'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'UploadFile'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(Req, State) ->
    {[], Req, State}.

-spec is_authorized(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: true | {false, AuthHeader :: iodata()},
        Req :: cowboy_req:req(),
        State :: state()
    }.
is_authorized(
    Req0,
    State = #state{
        operation_id = 'AddPet' = OperationID,
        logic_handler = LogicHandler
    }
) ->
    From = header,
    Result = openapi_auth:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        "Authorization",
        Req0
    ),
    case Result of
        {true, Context, Req} ->  {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->  {{false, AuthHeader}, Req, State}
    end;
is_authorized(
    Req0,
    State = #state{
        operation_id = 'DeletePet' = OperationID,
        logic_handler = LogicHandler
    }
) ->
    From = header,
    Result = openapi_auth:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        "Authorization",
        Req0
    ),
    case Result of
        {true, Context, Req} ->  {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->  {{false, AuthHeader}, Req, State}
    end;
is_authorized(
    Req0,
    State = #state{
        operation_id = 'FindPetsByStatus' = OperationID,
        logic_handler = LogicHandler
    }
) ->
    From = header,
    Result = openapi_auth:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        "Authorization",
        Req0
    ),
    case Result of
        {true, Context, Req} ->  {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->  {{false, AuthHeader}, Req, State}
    end;
is_authorized(
    Req0,
    State = #state{
        operation_id = 'FindPetsByTags' = OperationID,
        logic_handler = LogicHandler
    }
) ->
    From = header,
    Result = openapi_auth:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        "Authorization",
        Req0
    ),
    case Result of
        {true, Context, Req} ->  {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->  {{false, AuthHeader}, Req, State}
    end;
is_authorized(
    Req0,
    State = #state{
        operation_id = 'GetPetById' = OperationID,
        logic_handler = LogicHandler
    }
) ->
    From = header,
    Result = openapi_auth:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        "api_key",
        Req0
    ),
    case Result of
        {true, Context, Req} ->  {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->  {{false, AuthHeader}, Req, State}
    end;
is_authorized(
    Req0,
    State = #state{
        operation_id = 'UpdatePet' = OperationID,
        logic_handler = LogicHandler
    }
) ->
    From = header,
    Result = openapi_auth:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        "Authorization",
        Req0
    ),
    case Result of
        {true, Context, Req} ->  {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->  {{false, AuthHeader}, Req, State}
    end;
is_authorized(
    Req0,
    State = #state{
        operation_id = 'UpdatePetWithForm' = OperationID,
        logic_handler = LogicHandler
    }
) ->
    From = header,
    Result = openapi_auth:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        "Authorization",
        Req0
    ),
    case Result of
        {true, Context, Req} ->  {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->  {{false, AuthHeader}, Req, State}
    end;
is_authorized(
    Req0,
    State = #state{
        operation_id = 'UploadFile' = OperationID,
        logic_handler = LogicHandler
    }
) ->
    From = header,
    Result = openapi_auth:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        "Authorization",
        Req0
    ),
    case Result of
        {true, Context, Req} ->  {true, Req, State#state{context = Context}};
        {false, AuthHeader, Req} ->  {{false, AuthHeader}, Req, State}
    end;
is_authorized(Req, State) ->
    {{false, <<"">>}, Req, State}.
is_authorized(Req, State) ->
    {{false, <<"">>}, Req, State}.

-spec content_types_accepted(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: [{binary(), AcceptResource :: atom()}],
        Req :: cowboy_req:req(),
        State :: state()
    }.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, handle_request_json}
    ], Req, State}.

-spec valid_content_headers(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: boolean(), Req :: cowboy_req:req(), State :: state()}.

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'AddPet'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'DeletePet'
    }
) ->
    Headers = ["api_key"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'FindPetsByStatus'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'FindPetsByTags'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'GetPetById'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'UpdatePet'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'UpdatePetWithForm'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'UploadFile'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: [{binary(), ProvideResource :: atom()}],
        Req :: cowboy_req:req(),
        State :: state()
    }.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, handle_request_json}
    ], Req, State}.

-spec malformed_request(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: false, Req :: cowboy_req:req(), State :: state()}.

malformed_request(Req, State) ->
    {false, Req, State}.

-spec allow_missing_post(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: false, Req :: cowboy_req:req(), State :: state()}.

allow_missing_post(Req, State) ->
    {false, Req, State}.

-spec delete_resource(Req :: cowboy_req:req(), State :: state()) ->
    processed_response().

delete_resource(Req, State) ->
    handle_request_json(Req, State).

-spec known_content_type(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: true, Req :: cowboy_req:req(), State :: state()}.

known_content_type(Req, State) ->
    {true, Req, State}.

-spec valid_entity_length(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: true, Req :: cowboy_req:req(), State :: state()}.

valid_entity_length(Req, State) ->
    %% @TODO check the length
    {true, Req, State}.

%%%%
-type result_ok() :: {
    ok,
    {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: iodata()}
}.

-type result_error() :: {error, Reason :: any()}.

-type processed_response() :: {stop, cowboy_req:req(), state()}.

-spec process_response(result_ok() | result_error(), cowboy_req:req(), state()) ->
    processed_response().

process_response(Response, Req0, State = #state{operation_id = OperationID}) ->
    case Response of
        {ok, {Code, Headers, Body}} ->
            Req = cowboy_req:reply(Code, Headers, Body, Req0),
            {stop, Req, State};
        {error, Message} ->
            error_logger:error_msg("Unable to process request for ~p: ~p", [OperationID, Message]),

            Req = cowboy_req:reply(400, Req0),
            {stop, Req, State}
    end.

-spec handle_request_json(cowboy_req:req(), state()) -> processed_response().

handle_request_json(
    Req0,
    State = #state{
        operation_id = OperationID,
        logic_handler = LogicHandler,
        validator_state = ValidatorState
    }
) ->
    case openapi_api:populate_request(OperationID, Req0, ValidatorState) of
        {ok, Populated, Req1} ->
            {Code, Headers, Body} = openapi_logic_handler:handle_request(
                LogicHandler,
                OperationID,
                Req1,
                maps:merge(State#state.context, Populated)
            ),
            _ = openapi_api:validate_response(
                OperationID,
                Code,
                Body,
                ValidatorState
            ),
            PreparedBody = prepare_body(Code, Body),
            Response = {ok, {Code, Headers, PreparedBody}},
            process_response(Response, Req1, State);
        {error, Reason, Req1} ->
            process_response({error, Reason}, Req1, State)
    end.

validate_headers(_, Req) -> {true, Req}.

prepare_body(204, Body) when map_size(Body) == 0; length(Body) == 0 ->
    <<>>;
prepare_body(304, Body) when map_size(Body) == 0; length(Body) == 0 ->
    <<>>;
prepare_body(_Code, Body) ->
    jsx:encode(Body).
