-module(openapi_api).

-include_lib("kernel/include/logger.hrl").

-export([request_params/1]).
-export([request_param_info/2]).
-export([populate_request/3]).
-export([validate_response/4]).
%% exported to silence openapi complains
-export([get_value/3, validate_response_body/4]).

-type operation_id() :: atom().
-type request_param() :: atom().

-export_type([operation_id/0]).

-spec request_params(OperationID :: operation_id()) -> [Param :: request_param()].


request_params('TestAuthHttpBasic') ->
    [
    ];

request_params('TestAuthHttpBearer') ->
    [
    ];


request_params('TestBinaryGif') ->
    [
    ];

request_params('TestBodyApplicationOctetstreamBinary') ->
    [
        'file'
    ];

request_params('TestBodyMultipartFormdataArrayOfBinary') ->
    [
        'files'
    ];

request_params('TestBodyMultipartFormdataSingleBinary') ->
    [
        'my-file'
    ];

request_params('TestEchoBodyAllOfPet') ->
    [
        'Pet'
    ];

request_params('TestEchoBodyFreeFormObjectResponseString') ->
    [
        'object'
    ];

request_params('TestEchoBodyPet') ->
    [
        'Pet'
    ];

request_params('TestEchoBodyPetResponseString') ->
    [
        'Pet'
    ];

request_params('TestEchoBodyStringEnum') ->
    [
        'binary'
    ];

request_params('TestEchoBodyTagResponseString') ->
    [
        'Tag'
    ];


request_params('TestFormIntegerBooleanString') ->
    [
        'integer_form',
        'boolean_form',
        'string_form'
    ];

request_params('TestFormObjectMultipart') ->
    [
        'marker'
    ];

request_params('TestFormOneof') ->
    [
        'form1',
        'form2',
        'form3',
        'form4',
        'id',
        'name'
    ];


request_params('TestHeaderIntegerBooleanStringEnums') ->
    [
        'integer_header',
        'boolean_header',
        'string_header',
        'enum_nonref_string_header',
        'enum_ref_string_header'
    ];


request_params('TestsPathString{pathString}Integer{pathInteger}{enumNonrefStringPath}{enumRefStringPath}') ->
    [
        'path_string',
        'path_integer',
        'enum_nonref_string_path',
        'enum_ref_string_path'
    ];


request_params('TestEnumRefString') ->
    [
        'enum_nonref_string_query',
        'enum_ref_string_query'
    ];

request_params('TestQueryDatetimeDateString') ->
    [
        'datetime_query',
        'date_query',
        'string_query'
    ];

request_params('TestQueryIntegerBooleanString') ->
    [
        'integer_query',
        'boolean_query',
        'string_query'
    ];

request_params('TestQueryStyleDeepObjectExplodeTrueObject') ->
    [
        'query_object'
    ];

request_params('TestQueryStyleDeepObjectExplodeTrueObjectAllOf') ->
    [
        'query_object'
    ];

request_params('TestQueryStyleFormExplodeFalseArrayInteger') ->
    [
        'query_object'
    ];

request_params('TestQueryStyleFormExplodeFalseArrayString') ->
    [
        'query_object'
    ];

request_params('TestQueryStyleFormExplodeTrueArrayString') ->
    [
        'query_object'
    ];

request_params('TestQueryStyleFormExplodeTrueObject') ->
    [
        'query_object'
    ];

request_params('TestQueryStyleFormExplodeTrueObjectAllOf') ->
    [
        'query_object'
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




request_param_info('TestBodyApplicationOctetstreamBinary', 'file') ->
    #{
        source =>   body,
        rules => [
            {type, 'binary'},
            schema,
            not_required
        ]
    };

request_param_info('TestBodyMultipartFormdataArrayOfBinary', 'files') ->
    #{
        source =>   body,
        rules => [
            required
        ]
    };

request_param_info('TestBodyMultipartFormdataSingleBinary', 'my-file') ->
    #{
        source =>   body,
        rules => [
            {type, 'binary'},
            not_required
        ]
    };

request_param_info('TestEchoBodyAllOfPet', 'Pet') ->
    #{
        source =>   body,
        rules => [
            schema,
            not_required
        ]
    };

request_param_info('TestEchoBodyFreeFormObjectResponseString', 'object') ->
    #{
        source =>   body,
        rules => [
            schema,
            not_required
        ]
    };

request_param_info('TestEchoBodyPet', 'Pet') ->
    #{
        source =>   body,
        rules => [
            schema,
            not_required
        ]
    };

request_param_info('TestEchoBodyPetResponseString', 'Pet') ->
    #{
        source =>   body,
        rules => [
            schema,
            not_required
        ]
    };

request_param_info('TestEchoBodyStringEnum', 'binary') ->
    #{
        source =>   body,
        rules => [
            schema,
            not_required
        ]
    };

request_param_info('TestEchoBodyTagResponseString', 'Tag') ->
    #{
        source =>   body,
        rules => [
            schema,
            not_required
        ]
    };


request_param_info('TestFormIntegerBooleanString', 'integer_form') ->
    #{
        source =>   body,
        rules => [
            {type, 'integer'},
            not_required
        ]
    };

request_param_info('TestFormIntegerBooleanString', 'boolean_form') ->
    #{
        source =>   body,
        rules => [
            {type, 'boolean'},
            not_required
        ]
    };

request_param_info('TestFormIntegerBooleanString', 'string_form') ->
    #{
        source =>   body,
        rules => [
            {type, 'binary'},
            not_required
        ]
    };

request_param_info('TestFormObjectMultipart', 'marker') ->
    #{
        source =>   body,
        rules => [
            required
        ]
    };

request_param_info('TestFormOneof', 'form1') ->
    #{
        source =>   body,
        rules => [
            {type, 'binary'},
            not_required
        ]
    };

request_param_info('TestFormOneof', 'form2') ->
    #{
        source =>   body,
        rules => [
            {type, 'integer'},
            not_required
        ]
    };

request_param_info('TestFormOneof', 'form3') ->
    #{
        source =>   body,
        rules => [
            {type, 'binary'},
            not_required
        ]
    };

request_param_info('TestFormOneof', 'form4') ->
    #{
        source =>   body,
        rules => [
            {type, 'boolean'},
            not_required
        ]
    };

request_param_info('TestFormOneof', 'id') ->
    #{
        source =>   body,
        rules => [
            {type, 'integer'},
            not_required
        ]
    };

request_param_info('TestFormOneof', 'name') ->
    #{
        source =>   body,
        rules => [
            {type, 'binary'},
            not_required
        ]
    };


request_param_info('TestHeaderIntegerBooleanStringEnums', 'integer_header') ->
    #{
        source =>   header,
        rules => [
            {type, 'integer'},
            not_required
        ]
    };

request_param_info('TestHeaderIntegerBooleanStringEnums', 'boolean_header') ->
    #{
        source =>   header,
        rules => [
            {type, 'boolean'},
            not_required
        ]
    };

request_param_info('TestHeaderIntegerBooleanStringEnums', 'string_header') ->
    #{
        source =>   header,
        rules => [
            {type, 'binary'},
            not_required
        ]
    };

request_param_info('TestHeaderIntegerBooleanStringEnums', 'enum_nonref_string_header') ->
    #{
        source =>   header,
        rules => [
            {type, 'binary'},
            {enum, ['success', 'failure', 'unclassified'] },
            not_required
        ]
    };

request_param_info('TestHeaderIntegerBooleanStringEnums', 'enum_ref_string_header') ->
    #{
        source =>   header,
        rules => [
            not_required
        ]
    };


request_param_info('TestsPathString{pathString}Integer{pathInteger}{enumNonrefStringPath}{enumRefStringPath}', 'path_string') ->
    #{
        source =>  binding ,
        rules => [
            {type, 'binary'},
            required
        ]
    };

request_param_info('TestsPathString{pathString}Integer{pathInteger}{enumNonrefStringPath}{enumRefStringPath}', 'path_integer') ->
    #{
        source =>  binding ,
        rules => [
            {type, 'integer'},
            required
        ]
    };

request_param_info('TestsPathString{pathString}Integer{pathInteger}{enumNonrefStringPath}{enumRefStringPath}', 'enum_nonref_string_path') ->
    #{
        source =>  binding ,
        rules => [
            {type, 'binary'},
            {enum, ['success', 'failure', 'unclassified'] },
            required
        ]
    };

request_param_info('TestsPathString{pathString}Integer{pathInteger}{enumNonrefStringPath}{enumRefStringPath}', 'enum_ref_string_path') ->
    #{
        source =>  binding ,
        rules => [
            required
        ]
    };


request_param_info('TestEnumRefString', 'enum_nonref_string_query') ->
    #{
        source => qs_val  ,
        rules => [
            {type, 'binary'},
            {enum, ['success', 'failure', 'unclassified'] },
            not_required
        ]
    };

request_param_info('TestEnumRefString', 'enum_ref_string_query') ->
    #{
        source => qs_val  ,
        rules => [
            not_required
        ]
    };

request_param_info('TestQueryDatetimeDateString', 'datetime_query') ->
    #{
        source => qs_val  ,
        rules => [
            {type, 'datetime'},
            not_required
        ]
    };

request_param_info('TestQueryDatetimeDateString', 'date_query') ->
    #{
        source => qs_val  ,
        rules => [
            {type, 'date'},
            not_required
        ]
    };

request_param_info('TestQueryDatetimeDateString', 'string_query') ->
    #{
        source => qs_val  ,
        rules => [
            {type, 'binary'},
            not_required
        ]
    };

request_param_info('TestQueryIntegerBooleanString', 'integer_query') ->
    #{
        source => qs_val  ,
        rules => [
            {type, 'integer'},
            not_required
        ]
    };

request_param_info('TestQueryIntegerBooleanString', 'boolean_query') ->
    #{
        source => qs_val  ,
        rules => [
            {type, 'boolean'},
            not_required
        ]
    };

request_param_info('TestQueryIntegerBooleanString', 'string_query') ->
    #{
        source => qs_val  ,
        rules => [
            {type, 'binary'},
            not_required
        ]
    };

request_param_info('TestQueryStyleDeepObjectExplodeTrueObject', 'query_object') ->
    #{
        source => qs_val  ,
        rules => [
            not_required
        ]
    };

request_param_info('TestQueryStyleDeepObjectExplodeTrueObjectAllOf', 'query_object') ->
    #{
        source => qs_val  ,
        rules => [
            not_required
        ]
    };

request_param_info('TestQueryStyleFormExplodeFalseArrayInteger', 'query_object') ->
    #{
        source => qs_val  ,
        rules => [
            not_required
        ]
    };

request_param_info('TestQueryStyleFormExplodeFalseArrayString', 'query_object') ->
    #{
        source => qs_val  ,
        rules => [
            not_required
        ]
    };

request_param_info('TestQueryStyleFormExplodeTrueArrayString', 'query_object') ->
    #{
        source => qs_val  ,
        rules => [
            not_required
        ]
    };

request_param_info('TestQueryStyleFormExplodeTrueObject', 'query_object') ->
    #{
        source => qs_val  ,
        rules => [
            not_required
        ]
    };

request_param_info('TestQueryStyleFormExplodeTrueObjectAllOf', 'query_object') ->
    #{
        source => qs_val  ,
        rules => [
            not_required
        ]
    };

request_param_info(OperationID, Name) ->
    error({unknown_param, OperationID, Name}).

-spec populate_request(
        OperationID :: operation_id(),
        Req :: cowboy_req:req(),
        ValidatorState :: jesse_state:state()) ->
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
    case get_value(Source, Name, Req0) of
        {error, Reason, Req} ->
            {error, Reason, Req};
        {Value, Req} ->
            case prepare_param(Rules, Name, Value, ValidatorState) of
                {ok, Result} -> {ok, Name, Result, Req};
                {error, Reason} ->
                    {error, Reason, Req}
            end
    end.

-spec validate_response(
    OperationID :: operation_id(),
    Code :: 200..599,
    Body :: jesse:json_term(),
    ValidatorState :: jesse_state:state()) -> ok | no_return().


validate_response('TestAuthHttpBasic', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);

validate_response('TestAuthHttpBearer', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);


validate_response('TestBinaryGif', 200, Body, ValidatorState) ->
    validate_response_body('file', 'file', Body, ValidatorState);

validate_response('TestBodyApplicationOctetstreamBinary', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);

validate_response('TestBodyMultipartFormdataArrayOfBinary', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);

validate_response('TestBodyMultipartFormdataSingleBinary', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);

validate_response('TestEchoBodyAllOfPet', 200, Body, ValidatorState) ->
    validate_response_body('Pet', 'Pet', Body, ValidatorState);

validate_response('TestEchoBodyFreeFormObjectResponseString', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);

validate_response('TestEchoBodyPet', 200, Body, ValidatorState) ->
    validate_response_body('Pet', 'Pet', Body, ValidatorState);

validate_response('TestEchoBodyPetResponseString', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);

validate_response('TestEchoBodyStringEnum', 200, Body, ValidatorState) ->
    validate_response_body('StringEnumRef', 'StringEnumRef', Body, ValidatorState);

validate_response('TestEchoBodyTagResponseString', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);


validate_response('TestFormIntegerBooleanString', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);

validate_response('TestFormObjectMultipart', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);

validate_response('TestFormOneof', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);


validate_response('TestHeaderIntegerBooleanStringEnums', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);


validate_response('TestsPathString{pathString}Integer{pathInteger}{enumNonrefStringPath}{enumRefStringPath}', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);


validate_response('TestEnumRefString', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);

validate_response('TestQueryDatetimeDateString', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);

validate_response('TestQueryIntegerBooleanString', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);

validate_response('TestQueryStyleDeepObjectExplodeTrueObject', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);

validate_response('TestQueryStyleDeepObjectExplodeTrueObjectAllOf', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);

validate_response('TestQueryStyleFormExplodeFalseArrayInteger', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);

validate_response('TestQueryStyleFormExplodeFalseArrayString', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);

validate_response('TestQueryStyleFormExplodeTrueArrayString', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);

validate_response('TestQueryStyleFormExplodeTrueObject', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);

validate_response('TestQueryStyleFormExplodeTrueObjectAllOf', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);


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
        {ok, openapi_utils:to_int(Value)}
    catch
        error:badarg ->
            validation_error(Rule, Name)
    end;
validate(Rule = {type, 'float'}, Name, Value, _ValidatorState) ->
    try
        {ok, openapi_utils:to_float(Value)}
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
    case Value =< Max of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;
validate(Rule = {exclusive_max, ExclusiveMax}, Name, Value, _ValidatorState) ->
    case Value > ExclusiveMax of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;
validate(Rule = {min, Min}, Name, Value, _ValidatorState) ->
    case Value >= Min of
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
    Definition =  list_to_binary("#/components/schemas/" ++ openapi_utils:to_list(Name)),
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
    ?LOG_INFO(#{what => "Cannot validate rule", name => Name, rule => Rule}),
    error({unknown_validation_rule, Rule}).

-spec validation_error(Rule :: any(), Name :: any()) -> no_return().

validation_error(ViolatedRule, Name) ->
    validation_error(ViolatedRule, Name, #{}).

-spec validation_error(Rule :: any(), Name :: any(), Info :: #{}) -> no_return().

validation_error(ViolatedRule, Name, Info) ->
    throw({wrong_param, Name, ViolatedRule, Info}).

-spec get_value(body | qs_val | header | binding, Name :: any(), Req0 :: cowboy_req:req()) ->
    {Value :: any(), Req :: cowboy_req:req()} |
    {error, Reason :: any(), Req :: cowboy_req:req()}.
get_value(body, _Name, Req0) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    case prepare_body(Body) of
        {error, Reason} ->
            {error, Reason, Req};
        Value ->
            {Value, Req}
    end;
get_value(qs_val, Name, Req) ->
    QS = cowboy_req:parse_qs(Req),
    Value = openapi_utils:get_opt(openapi_utils:to_qs(Name), QS),
    {Value, Req};
get_value(header, Name, Req) ->
    Headers = cowboy_req:headers(Req),
    Value =  maps:get(openapi_utils:to_header(Name), Headers, undefined),
    {Value, Req};
get_value(binding, Name, Req) ->
    Value = cowboy_req:binding(openapi_utils:to_binding(Name), Req),
    {Value, Req}.

prepare_body(Body) ->
    case Body of
        <<>> -> <<>>;
        _ ->
            try
                json:decode(Body, [return_maps])
            catch
              error:_ ->
                {error, {invalid_body, not_json, Body}}
            end
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
    list_to_binary(string:to_lower(openapi_utils:to_list(V))).
