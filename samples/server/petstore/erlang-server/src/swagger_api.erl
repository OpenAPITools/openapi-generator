-module(swagger_api).

-export([request_params/1]).
-export([request_param_info/2]).
-export([populate_request/3]).
-export([validate_response/4]).

-type operation_id() :: atom().
-type request_param() :: atom().

-export_type([operation_id/0]).

-spec request_params(OperationID :: operation_id()) -> [Param :: request_param()].


request_params('AddPet') ->
    [
        'Pet'
    ];

request_params('DeletePet') ->
    [
        'petId',
        'api_key'
    ];

request_params('FindPetsByStatus') ->
    [
        'status'
    ];

request_params('FindPetsByTags') ->
    [
        'tags'
    ];

request_params('GetPetById') ->
    [
        'petId'
    ];

request_params('UpdatePet') ->
    [
        'Pet'
    ];

request_params('UpdatePetWithForm') ->
    [
        'petId',
        'name',
        'status'
    ];

request_params('UploadFile') ->
    [
        'petId',
        'additionalMetadata',
        'file'
    ];


request_params('DeleteOrder') ->
    [
        'orderId'
    ];

request_params('GetInventory') ->
    [
    ];

request_params('GetOrderById') ->
    [
        'orderId'
    ];

request_params('PlaceOrder') ->
    [
        'Order'
    ];


request_params('CreateUser') ->
    [
        'User'
    ];

request_params('CreateUsersWithArrayInput') ->
    [
        'list'
    ];

request_params('CreateUsersWithListInput') ->
    [
        'list'
    ];

request_params('DeleteUser') ->
    [
        'username'
    ];

request_params('GetUserByName') ->
    [
        'username'
    ];

request_params('LoginUser') ->
    [
        'username',
        'password'
    ];

request_params('LogoutUser') ->
    [
    ];

request_params('UpdateUser') ->
    [
        'username',
        'User'
    ];

request_params(_) ->
    error(unknown_operation).

-type rule() ::
    {type, 'binary'} |
    {type, 'integer'} |
    {type, 'float'} |
    {type, 'binary'} |
    {type, 'boolean'} |
    {type, 'date'} |
    {type, 'datetime'} |
    {enum, [atom()]} |
    {max, Max :: number()} |
    {exclusive_max, Max :: number()} |
    {min, Min :: number()} |
    {exclusive_min, Min :: number()} |
    {max_length, MaxLength :: integer()} |
    {min_length, MaxLength :: integer()} |
    {pattern, Pattern :: string()} |
    schema |
    required |
    not_required.

-spec request_param_info(OperationID :: operation_id(), Name :: request_param()) -> #{
    source => qs_val | binding | header | body,
    rules => [rule()]
}.



request_param_info('AddPet', 'Pet') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('DeletePet', 'petId') ->
    #{
        source =>  binding ,
        rules => [
            {type, 'integer'},
            required
        ]
    };

request_param_info('DeletePet', 'api_key') ->
    #{
        source =>   header,
        rules => [
            {type, 'binary'},
            not_required
        ]
    };

request_param_info('FindPetsByStatus', 'status') ->
    #{
        source => qs_val  ,
        rules => [
            {enum, ['available', 'pending', 'sold'] },
            required
        ]
    };

request_param_info('FindPetsByTags', 'tags') ->
    #{
        source => qs_val  ,
        rules => [
            required
        ]
    };

request_param_info('GetPetById', 'petId') ->
    #{
        source =>  binding ,
        rules => [
            {type, 'integer'},
            required
        ]
    };

request_param_info('UpdatePet', 'Pet') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('UpdatePetWithForm', 'petId') ->
    #{
        source =>  binding ,
        rules => [
            {type, 'integer'},
            required
        ]
    };

request_param_info('UpdatePetWithForm', 'name') ->
    #{
        source =>   ,
        rules => [
            {type, 'binary'},
            not_required
        ]
    };

request_param_info('UpdatePetWithForm', 'status') ->
    #{
        source =>   ,
        rules => [
            {type, 'binary'},
            not_required
        ]
    };

request_param_info('UploadFile', 'petId') ->
    #{
        source =>  binding ,
        rules => [
            {type, 'integer'},
            required
        ]
    };

request_param_info('UploadFile', 'additionalMetadata') ->
    #{
        source =>   ,
        rules => [
            {type, 'binary'},
            not_required
        ]
    };

request_param_info('UploadFile', 'file') ->
    #{
        source =>   ,
        rules => [
            not_required
        ]
    };


request_param_info('DeleteOrder', 'orderId') ->
    #{
        source =>  binding ,
        rules => [
            {type, 'binary'},
            required
        ]
    };

request_param_info('GetOrderById', 'orderId') ->
    #{
        source =>  binding ,
        rules => [
            {type, 'integer'},
            {max, 5 }, 
            {min, 1 },
            required
        ]
    };

request_param_info('PlaceOrder', 'Order') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };


request_param_info('CreateUser', 'User') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('CreateUsersWithArrayInput', 'list') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('CreateUsersWithListInput', 'list') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('DeleteUser', 'username') ->
    #{
        source =>  binding ,
        rules => [
            {type, 'binary'},
            required
        ]
    };

request_param_info('GetUserByName', 'username') ->
    #{
        source =>  binding ,
        rules => [
            {type, 'binary'},
            required
        ]
    };

request_param_info('LoginUser', 'username') ->
    #{
        source => qs_val  ,
        rules => [
            {type, 'binary'},
            required
        ]
    };

request_param_info('LoginUser', 'password') ->
    #{
        source => qs_val  ,
        rules => [
            {type, 'binary'},
            required
        ]
    };

request_param_info('UpdateUser', 'username') ->
    #{
        source =>  binding ,
        rules => [
            {type, 'binary'},
            required
        ]
    };

request_param_info('UpdateUser', 'User') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info(OperationID, Name) ->
    error({unknown_param, OperationID, Name}).

-spec populate_request(
    OperationID :: operation_id(),
    Req :: cowboy_req:req(),
    ValidatorState :: jesse_state:state()
) ->
    {ok, Model :: #{}, Req :: cowboy_req:req()} |
    {error, Reason :: any(), Req :: cowboy_req:req()}.

populate_request(OperationID, Req, ValidatorState) ->
    Params = request_params(OperationID),
    populate_request_params(OperationID, Params, Req, ValidatorState, #{}).

populate_request_params(_, [], Req, _, Model) ->
    {ok, Model, Req};

populate_request_params(OperationID, [FieldParams | T], Req0, ValidatorState, Model) ->
    case populate_request_param(OperationID, FieldParams, Req0, ValidatorState) of
        {ok, K, V, Req} ->
            populate_request_params(OperationID, T, Req, ValidatorState, maps:put(K, V, Model));
        Error ->
            Error
    end.

populate_request_param(OperationID, Name, Req0, ValidatorState) ->
    #{rules := Rules, source := Source} = request_param_info(OperationID, Name),
    {Value, Req} = get_value(Source, Name, Req0),
    case prepare_param(Rules, Name, Value, ValidatorState) of
        {ok, Result} -> {ok, Name, Result, Req};
        {error, Reason} ->
            {error, Reason, Req}
    end.

-spec validate_response(
    OperationID :: operation_id(),
    Code :: 200..599,
    Body :: jesse:json_term(),
    ValidatorState :: jesse_state:state()
) -> ok | no_return().


validate_response('AddPet', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('DeletePet', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('FindPetsByStatus', 200, Body, ValidatorState) ->
    validate_response_body('list', 'Pet', Body, ValidatorState);
validate_response('FindPetsByStatus', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('FindPetsByTags', 200, Body, ValidatorState) ->
    validate_response_body('list', 'Pet', Body, ValidatorState);
validate_response('FindPetsByTags', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('GetPetById', 200, Body, ValidatorState) ->
    validate_response_body('Pet', 'Pet', Body, ValidatorState);
validate_response('GetPetById', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('GetPetById', 404, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('UpdatePet', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('UpdatePet', 404, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('UpdatePet', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('UpdatePetWithForm', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('UploadFile', 200, Body, ValidatorState) ->
    validate_response_body('ApiResponse', 'ApiResponse', Body, ValidatorState);


validate_response('DeleteOrder', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('DeleteOrder', 404, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('GetInventory', 200, Body, ValidatorState) ->
    validate_response_body('map', 'integer', Body, ValidatorState);

validate_response('GetOrderById', 200, Body, ValidatorState) ->
    validate_response_body('Order', 'Order', Body, ValidatorState);
validate_response('GetOrderById', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('GetOrderById', 404, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('PlaceOrder', 200, Body, ValidatorState) ->
    validate_response_body('Order', 'Order', Body, ValidatorState);
validate_response('PlaceOrder', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);


validate_response('CreateUser', 0, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('CreateUsersWithArrayInput', 0, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('CreateUsersWithListInput', 0, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('DeleteUser', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('DeleteUser', 404, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('GetUserByName', 200, Body, ValidatorState) ->
    validate_response_body('User', 'User', Body, ValidatorState);
validate_response('GetUserByName', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('GetUserByName', 404, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('LoginUser', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('LoginUser', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('LogoutUser', 0, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('UpdateUser', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('UpdateUser', 404, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);


validate_response(_OperationID, _Code, _Body, _ValidatorState) ->
    ok.

validate_response_body('list', ReturnBaseType, Body, ValidatorState) ->
    [
        validate(schema, ReturnBaseType, Item, ValidatorState)
    || Item <- Body];

validate_response_body(_, ReturnBaseType, Body, ValidatorState) ->
    validate(schema, ReturnBaseType, Body, ValidatorState).

%%%
validate(Rule = required, Name, Value, _ValidatorState) ->
    case Value of
        undefined -> validation_error(Rule, Name);
        _ -> ok
    end;

validate(not_required, _Name, _Value, _ValidatorState) ->
    ok;

validate(_, _Name, undefined, _ValidatorState) ->
    ok;

validate(Rule = {type, 'integer'}, Name, Value, _ValidatorState) ->
    try
        {ok, swagger_utils:to_int(Value)}
    catch
        error:badarg ->
            validation_error(Rule, Name)
    end;

validate(Rule = {type, 'float'}, Name, Value, _ValidatorState) ->
    try
        {ok, swagger_utils:to_float(Value)}
    catch
        error:badarg ->
            validation_error(Rule, Name)
    end;

validate(Rule = {type, 'binary'}, Name, Value, _ValidatorState) ->
    case is_binary(Value) of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(_Rule = {type, 'boolean'}, _Name, Value, _ValidatorState) when is_boolean(Value) ->
    {ok, Value};

validate(Rule = {type, 'boolean'}, Name, Value, _ValidatorState) ->
    V = binary_to_lower(Value),
    try
        case binary_to_existing_atom(V, utf8) of
            B when is_boolean(B) -> {ok, B};
            _ -> validation_error(Rule, Name)
        end
    catch
        error:badarg ->
            validation_error(Rule, Name)
    end;

validate(Rule = {type, 'date'}, Name, Value, _ValidatorState) ->
    case is_binary(Value) of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {type, 'datetime'}, Name, Value, _ValidatorState) ->
    case is_binary(Value) of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {enum, Values}, Name, Value, _ValidatorState) ->
    try
        FormattedValue = erlang:binary_to_existing_atom(Value, utf8),
        case lists:member(FormattedValue, Values) of
            true -> {ok, FormattedValue};
            false -> validation_error(Rule, Name)
        end
    catch
        error:badarg ->
            validation_error(Rule, Name)
    end;

validate(Rule = {max, Max}, Name, Value, _ValidatorState) ->
    case Value >= Max of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {exclusive_max, ExclusiveMax}, Name, Value, _ValidatorState) ->
    case Value > ExclusiveMax of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {min, Min}, Name, Value, _ValidatorState) ->
    case Value =< Min of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {exclusive_min, ExclusiveMin}, Name, Value, _ValidatorState) ->
    case Value =< ExclusiveMin of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {max_length, MaxLength}, Name, Value, _ValidatorState) ->
    case size(Value) =< MaxLength of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {min_length, MinLength}, Name, Value, _ValidatorState) ->
    case size(Value) >= MinLength of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {pattern, Pattern}, Name, Value, _ValidatorState) ->
    {ok, MP} = re:compile(Pattern),
    case re:run(Value, MP) of
        {match, _} -> ok;
        _ -> validation_error(Rule, Name)
    end;

validate(Rule = schema, Name, Value, ValidatorState) ->
    Definition =  list_to_binary("#/definitions/" ++ swagger_utils:to_list(Name)),
    try
        _ = validate_with_schema(Value, Definition, ValidatorState),
        ok
    catch
        throw:[{schema_invalid, _, Error} | _] ->
            Info = #{
                type => schema_invalid,
                error => Error
            },
            validation_error(Rule, Name, Info);
        throw:[{data_invalid, Schema, Error, _, Path} | _] ->
            Info = #{
                type => data_invalid,
                error => Error,
                schema => Schema,
                path => Path
            },
            validation_error(Rule, Name, Info)
    end;

validate(Rule, Name, _Value, _ValidatorState) ->
    error_logger:info_msg("Can't validate ~p with ~p", [Name, Rule]),
    error({unknown_validation_rule, Rule}).

-spec validation_error(Rule :: any(), Name :: any()) -> no_return().

validation_error(ViolatedRule, Name) ->
    validation_error(ViolatedRule, Name, #{}).

-spec validation_error(Rule :: any(), Name :: any(), Info :: #{}) -> no_return().

validation_error(ViolatedRule, Name, Info) ->
    throw({wrong_param, Name, ViolatedRule, Info}).

get_value(body, _Name, Req0) ->
    {ok, Body, Req} = cowboy_req:body(Req0),
    Value = prepare_body(Body),
    {Value, Req};

get_value(qs_val, Name, Req0) ->
    {QS, Req} = cowboy_req:qs_vals(Req0),
    Value = swagger_utils:get_opt(swagger_utils:to_qs(Name), QS),
    {Value, Req};

get_value(header, Name, Req0) ->
    {Headers, Req} = cowboy_req:headers(Req0),
    Value = swagger_utils:get_opt(swagger_utils:to_header(Name), Headers),
    {Value, Req};

get_value(binding, Name, Req0) ->
    {Bindings, Req} = cowboy_req:bindings(Req0),
    Value = swagger_utils:get_opt(swagger_utils:to_binding(Name), Bindings),
    {Value, Req}.

prepare_body(Body) ->
    case Body of
        <<"">> -> <<"">>;
        _ -> jsx:decode(Body, [return_maps])
    end.

validate_with_schema(Body, Definition, ValidatorState) ->
    jesse_schema_validator:validate_with_state(
        [{<<"$ref">>, Definition}],
        Body,
        ValidatorState
    ).

prepare_param(Rules, Name, Value, ValidatorState) ->
    try
        Result = lists:foldl(
            fun(Rule, Acc) ->
                case validate(Rule, Name, Acc, ValidatorState) of
                    ok -> Acc;
                    {ok, Prepared} -> Prepared
                end
            end,
            Value,
            Rules
        ),
        {ok, Result}
    catch
        throw:Reason ->
            {error, Reason}
    end.

binary_to_lower(V) when is_binary(V) ->
    list_to_binary(string:to_lower(swagger_utils:to_list(V))).
