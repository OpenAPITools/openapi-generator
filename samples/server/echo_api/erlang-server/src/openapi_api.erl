-module(openapi_api).
-moduledoc """
This module offers an API for JSON schema validation, using `jesse` under the hood.

If validation is desired, a jesse state can be loaded using `prepare_validator/1`,
and request and response can be validated using `populate_request/3`
and `validate_response/4` respectively.

For example, the user-defined `Module:accept_callback/4` can be implemented as follows:
```
-spec accept_callback(
        Class :: openapi_api:class(),
        OperationID :: openapi_api:operation_id(),
        Req :: cowboy_req:req(),
        Context :: openapi_logic_handler:context()) ->
    {openapi_logic_handler:accept_callback_return(),
     cowboy_req:req(),
     openapi_logic_handler:context()}.
accept_callback(Class, OperationID, Req0, Context0) ->
    ValidatorState = openapi_api:prepare_validator(),
    case openapi_api:populate_request(OperationID, Req0, ValidatorState) of
        {ok, Model, Req1} ->
            Context1 = maps:merge(Context0, Model),
            case do_accept_callback(Class, OperationID, Req1, Context1) of
                {false, Req2, Context2} ->
                    {false, Req2, Context2};
                {{true, Code, Body}, Req2, Context2} ->
                    case validate_response(OperationID, Code, Body, ValidatorState) of
                        ok ->
                            process_response({ok, Code, Body}, Req2, Context2);
                        {error, Reason} ->
                            process_response({error, Reason}, Req2, Context2)
                    end
            end;
        {error, Reason, Req1} ->
            process_response({error, Reason}, Req1, Context0)
    end.
```
""".

-export([prepare_validator/0, prepare_validator/1, prepare_validator/2]).
-export([populate_request/3, validate_response/4]).

-ignore_xref([populate_request/3, validate_response/4]).
-ignore_xref([prepare_validator/0, prepare_validator/1, prepare_validator/2]).

-type class() ::
    'auth'
    | 'body'
    | 'form'
    | 'header'
    | 'path'
    | 'query'.


-type operation_id() ::
    'test/auth/http/basic' | %% To test HTTP basic authentication
    'test/auth/http/bearer' | %% To test HTTP bearer authentication
    'test/binary/gif' | %% Test binary (gif) response body
    'test/body/application/octetstream/binary' | %% Test body parameter(s)
    'test/body/multipart/formdata/array_of_binary' | %% Test array of binary in multipart mime
    'test/body/multipart/formdata/single_binary' | %% Test single binary in multipart mime
    'test/echo/body/allOf/Pet' | %% Test body parameter(s)
    'test/echo/body/FreeFormObject/response_string' | %% Test free form object
    'test/echo/body/Pet' | %% Test body parameter(s)
    'test/echo/body/Pet/response_string' | %% Test empty response body
    'test/echo/body/string_enum' | %% Test string enum response body
    'test/echo/body/Tag/response_string' | %% Test empty json (request body)
    'test/form/integer/boolean/string' | %% Test form parameter(s)
    'test/form/object/multipart' | %% Test form parameter(s) for multipart schema
    'test/form/oneof' | %% Test form parameter(s) for oneOf schema
    'test/header/integer/boolean/string/enums' | %% Test header parameter(s)
    'tests/path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path}' | %% Test path parameter(s)
    'test/enum_ref_string' | %% Test query parameter(s)
    'test/query/datetime/date/string' | %% Test query parameter(s)
    'test/query/integer/boolean/string' | %% Test query parameter(s)
    'test/query/style_deepObject/explode_true/object' | %% Test query parameter(s)
    'test/query/style_deepObject/explode_true/object/allOf' | %% Test query parameter(s)
    'test/query/style_form/explode_false/array_integer' | %% Test query parameter(s)
    'test/query/style_form/explode_false/array_string' | %% Test query parameter(s)
    'test/query/style_form/explode_true/array_string' | %% Test query parameter(s)
    'test/query/style_form/explode_true/object' | %% Test query parameter(s)
    'test/query/style_form/explode_true/object/allOf' | %% Test query parameter(s)
    {error, unknown_operation}.

-type request_param() :: atom().

-export_type([class/0, operation_id/0]).

-dialyzer({nowarn_function, [validate_response_body/4]}).

-type rule() ::
    {type, binary} |
    {type, byte} |
    {type, integer} |
    {type, float} |
    {type, boolean} |
    {type, date} |
    {type, datetime} |
    {enum, [atom()]} |
    {max, Max :: number()} |
    {exclusive_max, Max :: number()} |
    {min, Min :: number()} |
    {exclusive_min, Min :: number()} |
    {max_length, MaxLength :: integer()} |
    {min_length, MaxLength :: integer()} |
    {pattern, Pattern :: string()} |
    {schema, object | list, binary()} |
    schema |
    required |
    not_required.

-doc #{equiv => prepare_validator/2}.
-spec prepare_validator() -> jesse_state:state().
prepare_validator() ->
    prepare_validator(<<"http://json-schema.org/draft-06/schema#">>).

-doc #{equiv => prepare_validator/2}.
-spec prepare_validator(binary()) -> jesse_state:state().
prepare_validator(SchemaVer) ->
    prepare_validator(get_openapi_path(), SchemaVer).

-doc """
Loads the JSON schema and the desired validation draft into a `t:jesse_state:state/0`.
""".
-spec prepare_validator(file:name_all(), binary()) -> jesse_state:state().
prepare_validator(OpenApiPath, SchemaVer) ->
    {ok, FileContents} = file:read_file(OpenApiPath),
    R = json:decode(FileContents),
    jesse_state:new(R, [{default_schema_ver, SchemaVer}]).

-doc """
Automatically loads the entire body from the cowboy req
and validates the JSON body against the schema.
""".
-spec populate_request(
        OperationID :: operation_id(),
        Req :: cowboy_req:req(),
        ValidatorState :: jesse_state:state()) ->
    {ok, Model :: #{}, Req :: cowboy_req:req()} |
    {error, Reason :: any(), Req :: cowboy_req:req()}.
populate_request(OperationID, Req, ValidatorState) ->
    Params = request_params(OperationID),
    populate_request_params(OperationID, Params, Req, ValidatorState, #{}).

-doc """
Validates that the provided `Code` and `Body` comply with the `ValidatorState` schema
for the `OperationID` operation.
""".
-spec validate_response(
        OperationID :: operation_id(),
        Code :: 200..599,
        Body :: jesse:json_term(),
        ValidatorState :: jesse_state:state()) ->
    ok | {ok, term()} | [ok | {ok, term()}] | no_return().
validate_response('test/auth/http/basic', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/auth/http/bearer', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/binary/gif', 200, Body, ValidatorState) ->
    validate_response_body('file', 'file', Body, ValidatorState);
validate_response('test/body/application/octetstream/binary', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/body/multipart/formdata/array_of_binary', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/body/multipart/formdata/single_binary', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/echo/body/allOf/Pet', 200, Body, ValidatorState) ->
    validate_response_body('Pet', 'Pet', Body, ValidatorState);
validate_response('test/echo/body/FreeFormObject/response_string', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/echo/body/Pet', 200, Body, ValidatorState) ->
    validate_response_body('Pet', 'Pet', Body, ValidatorState);
validate_response('test/echo/body/Pet/response_string', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/echo/body/string_enum', 200, Body, ValidatorState) ->
    validate_response_body('StringEnumRef', 'StringEnumRef', Body, ValidatorState);
validate_response('test/echo/body/Tag/response_string', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/form/integer/boolean/string', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/form/object/multipart', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/form/oneof', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/header/integer/boolean/string/enums', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('tests/path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path}', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/enum_ref_string', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/query/datetime/date/string', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/query/integer/boolean/string', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/query/style_deepObject/explode_true/object', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/query/style_deepObject/explode_true/object/allOf', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/query/style_form/explode_false/array_integer', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/query/style_form/explode_false/array_string', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/query/style_form/explode_true/array_string', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/query/style_form/explode_true/object', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('test/query/style_form/explode_true/object/allOf', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response(_OperationID, _Code, _Body, _ValidatorState) ->
    ok.

%%%
-spec request_params(OperationID :: operation_id()) -> [Param :: request_param()].
request_params('test/auth/http/basic') ->
    [
    ];
request_params('test/auth/http/bearer') ->
    [
    ];
request_params('test/binary/gif') ->
    [
    ];
request_params('test/body/application/octetstream/binary') ->
    [
        'file'
    ];
request_params('test/body/multipart/formdata/array_of_binary') ->
    [
        'files'
    ];
request_params('test/body/multipart/formdata/single_binary') ->
    [
        'my-file'
    ];
request_params('test/echo/body/allOf/Pet') ->
    [
        'Pet'
    ];
request_params('test/echo/body/FreeFormObject/response_string') ->
    [
        'object'
    ];
request_params('test/echo/body/Pet') ->
    [
        'Pet'
    ];
request_params('test/echo/body/Pet/response_string') ->
    [
        'Pet'
    ];
request_params('test/echo/body/string_enum') ->
    [
        'StringEnumRef'
    ];
request_params('test/echo/body/Tag/response_string') ->
    [
        'Tag'
    ];
request_params('test/form/integer/boolean/string') ->
    [
        'integer_form',
        'boolean_form',
        'string_form'
    ];
request_params('test/form/object/multipart') ->
    [
        'marker'
    ];
request_params('test/form/oneof') ->
    [
        'form1',
        'form2',
        'form3',
        'form4',
        'id',
        'name'
    ];
request_params('test/header/integer/boolean/string/enums') ->
    [
        'integer_header',
        'boolean_header',
        'string_header',
        'enum_nonref_string_header',
        'enum_ref_string_header'
    ];
request_params('tests/path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path}') ->
    [
        'path_string',
        'path_integer',
        'enum_nonref_string_path',
        'enum_ref_string_path'
    ];
request_params('test/enum_ref_string') ->
    [
        'enum_nonref_string_query',
        'enum_ref_string_query'
    ];
request_params('test/query/datetime/date/string') ->
    [
        'datetime_query',
        'date_query',
        'string_query'
    ];
request_params('test/query/integer/boolean/string') ->
    [
        'integer_query',
        'boolean_query',
        'string_query'
    ];
request_params('test/query/style_deepObject/explode_true/object') ->
    [
        'query_object'
    ];
request_params('test/query/style_deepObject/explode_true/object/allOf') ->
    [
        'query_object'
    ];
request_params('test/query/style_form/explode_false/array_integer') ->
    [
        'query_object'
    ];
request_params('test/query/style_form/explode_false/array_string') ->
    [
        'query_object'
    ];
request_params('test/query/style_form/explode_true/array_string') ->
    [
        'query_object'
    ];
request_params('test/query/style_form/explode_true/object') ->
    [
        'query_object'
    ];
request_params('test/query/style_form/explode_true/object/allOf') ->
    [
        'query_object'
    ];
request_params(_) ->
    error(unknown_operation).

-spec request_param_info(OperationID :: operation_id(), Name :: request_param()) ->
    #{source => qs_val | binding | header | body, rules => [rule()]}.
request_param_info('test/body/application/octetstream/binary', 'file') ->
    #{
        source => body,
        rules => [
            {type, binary},
            not_required
        ]
    };
request_param_info('test/body/multipart/formdata/array_of_binary', 'files') ->
    #{
        source => body,
        rules => [
            required
        ]
    };
request_param_info('test/body/multipart/formdata/single_binary', 'my-file') ->
    #{
        source => body,
        rules => [
            {type, binary},
            not_required
        ]
    };
request_param_info('test/echo/body/allOf/Pet', 'Pet') ->
    #{
        source => body,
        rules => [
            {schema, object, <<"#/components/schemas/Pet">>},
            not_required
        ]
    };
request_param_info('test/echo/body/FreeFormObject/response_string', 'object') ->
    #{
        source => body,
        rules => [
            not_required
        ]
    };
request_param_info('test/echo/body/Pet', 'Pet') ->
    #{
        source => body,
        rules => [
            {schema, object, <<"#/components/schemas/Pet">>},
            not_required
        ]
    };
request_param_info('test/echo/body/Pet/response_string', 'Pet') ->
    #{
        source => body,
        rules => [
            {schema, object, <<"#/components/schemas/Pet">>},
            not_required
        ]
    };
request_param_info('test/echo/body/string_enum', 'StringEnumRef') ->
    #{
        source => body,
        rules => [
            {schema, object, <<"#/components/schemas/StringEnumRef">>},
            not_required
        ]
    };
request_param_info('test/echo/body/Tag/response_string', 'Tag') ->
    #{
        source => body,
        rules => [
            {schema, object, <<"#/components/schemas/Tag">>},
            not_required
        ]
    };
request_param_info('test/form/integer/boolean/string', 'integer_form') ->
    #{
        source => body,
        rules => [
            {type, integer},
            not_required
        ]
    };
request_param_info('test/form/integer/boolean/string', 'boolean_form') ->
    #{
        source => body,
        rules => [
            {type, boolean},
            not_required
        ]
    };
request_param_info('test/form/integer/boolean/string', 'string_form') ->
    #{
        source => body,
        rules => [
            {type, binary},
            not_required
        ]
    };
request_param_info('test/form/object/multipart', 'marker') ->
    #{
        source => body,
        rules => [
            required
        ]
    };
request_param_info('test/form/oneof', 'form1') ->
    #{
        source => body,
        rules => [
            {type, binary},
            not_required
        ]
    };
request_param_info('test/form/oneof', 'form2') ->
    #{
        source => body,
        rules => [
            {type, integer},
            not_required
        ]
    };
request_param_info('test/form/oneof', 'form3') ->
    #{
        source => body,
        rules => [
            {type, binary},
            not_required
        ]
    };
request_param_info('test/form/oneof', 'form4') ->
    #{
        source => body,
        rules => [
            {type, boolean},
            not_required
        ]
    };
request_param_info('test/form/oneof', 'id') ->
    #{
        source => body,
        rules => [
            {type, integer},
            not_required
        ]
    };
request_param_info('test/form/oneof', 'name') ->
    #{
        source => body,
        rules => [
            {type, binary},
            not_required
        ]
    };
request_param_info('test/header/integer/boolean/string/enums', 'integer_header') ->
    #{
        source => header,
        rules => [
            {type, integer},
            not_required
        ]
    };
request_param_info('test/header/integer/boolean/string/enums', 'boolean_header') ->
    #{
        source => header,
        rules => [
            {type, boolean},
            not_required
        ]
    };
request_param_info('test/header/integer/boolean/string/enums', 'string_header') ->
    #{
        source => header,
        rules => [
            {type, binary},
            not_required
        ]
    };
request_param_info('test/header/integer/boolean/string/enums', 'enum_nonref_string_header') ->
    #{
        source => header,
        rules => [
            {type, binary},
            {enum, ['success', 'failure', 'unclassified'] },
            not_required
        ]
    };
request_param_info('test/header/integer/boolean/string/enums', 'enum_ref_string_header') ->
    #{
        source => header,
        rules => [
            not_required
        ]
    };
request_param_info('tests/path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path}', 'path_string') ->
    #{
        source => binding,
        rules => [
            {type, binary},
            required
        ]
    };
request_param_info('tests/path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path}', 'path_integer') ->
    #{
        source => binding,
        rules => [
            {type, integer},
            required
        ]
    };
request_param_info('tests/path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path}', 'enum_nonref_string_path') ->
    #{
        source => binding,
        rules => [
            {type, binary},
            {enum, ['success', 'failure', 'unclassified'] },
            required
        ]
    };
request_param_info('tests/path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path}', 'enum_ref_string_path') ->
    #{
        source => binding,
        rules => [
            required
        ]
    };
request_param_info('test/enum_ref_string', 'enum_nonref_string_query') ->
    #{
        source => qs_val,
        rules => [
            {type, binary},
            {enum, ['success', 'failure', 'unclassified'] },
            not_required
        ]
    };
request_param_info('test/enum_ref_string', 'enum_ref_string_query') ->
    #{
        source => qs_val,
        rules => [
            not_required
        ]
    };
request_param_info('test/query/datetime/date/string', 'datetime_query') ->
    #{
        source => qs_val,
        rules => [
            {type, datetime},
            not_required
        ]
    };
request_param_info('test/query/datetime/date/string', 'date_query') ->
    #{
        source => qs_val,
        rules => [
            {type, date},
            not_required
        ]
    };
request_param_info('test/query/datetime/date/string', 'string_query') ->
    #{
        source => qs_val,
        rules => [
            {type, binary},
            not_required
        ]
    };
request_param_info('test/query/integer/boolean/string', 'integer_query') ->
    #{
        source => qs_val,
        rules => [
            {type, integer},
            not_required
        ]
    };
request_param_info('test/query/integer/boolean/string', 'boolean_query') ->
    #{
        source => qs_val,
        rules => [
            {type, boolean},
            not_required
        ]
    };
request_param_info('test/query/integer/boolean/string', 'string_query') ->
    #{
        source => qs_val,
        rules => [
            {type, binary},
            not_required
        ]
    };
request_param_info('test/query/style_deepObject/explode_true/object', 'query_object') ->
    #{
        source => qs_val,
        rules => [
            not_required
        ]
    };
request_param_info('test/query/style_deepObject/explode_true/object/allOf', 'query_object') ->
    #{
        source => qs_val,
        rules => [
            not_required
        ]
    };
request_param_info('test/query/style_form/explode_false/array_integer', 'query_object') ->
    #{
        source => qs_val,
        rules => [
            not_required
        ]
    };
request_param_info('test/query/style_form/explode_false/array_string', 'query_object') ->
    #{
        source => qs_val,
        rules => [
            not_required
        ]
    };
request_param_info('test/query/style_form/explode_true/array_string', 'query_object') ->
    #{
        source => qs_val,
        rules => [
            not_required
        ]
    };
request_param_info('test/query/style_form/explode_true/object', 'query_object') ->
    #{
        source => qs_val,
        rules => [
            not_required
        ]
    };
request_param_info('test/query/style_form/explode_true/object/allOf', 'query_object') ->
    #{
        source => qs_val,
        rules => [
            not_required
        ]
    };
request_param_info(OperationID, Name) ->
    error({unknown_param, OperationID, Name}).

-spec populate_request_params(
        operation_id(), [request_param()], cowboy_req:req(), jesse_state:state(), map()) ->
    {ok, map(), cowboy_req:req()} | {error, _, cowboy_req:req()}.
populate_request_params(_, [], Req, _, Model) ->
    {ok, Model, Req};
populate_request_params(OperationID, [ReqParamName | T], Req0, ValidatorState, Model0) ->
    case populate_request_param(OperationID, ReqParamName, Req0, ValidatorState) of
        {ok, V, Req} ->
            Model = Model0#{ReqParamName => V},
            populate_request_params(OperationID, T, Req, ValidatorState, Model);
        Error ->
            Error
    end.

-spec populate_request_param(
        operation_id(), request_param(), cowboy_req:req(), jesse_state:state()) ->
    {ok, term(), cowboy_req:req()} | {error, term(), cowboy_req:req()}.
populate_request_param(OperationID, ReqParamName, Req0, ValidatorState) ->
    #{rules := Rules, source := Source} = request_param_info(OperationID, ReqParamName),
    case get_value(Source, ReqParamName, Req0) of
        {error, Reason, Req} ->
            {error, Reason, Req};
        {Value, Req} ->
            case prepare_param(Rules, ReqParamName, Value, ValidatorState) of
                {ok, Result} -> {ok, Result, Req};
                {error, Reason} ->
                    {error, Reason, Req}
            end
    end.

validate_response_body(list, ReturnBaseType, Body, ValidatorState) ->
    [
        validate(schema, Item, ReturnBaseType, ValidatorState)
    || Item <- Body];

validate_response_body(_, ReturnBaseType, Body, ValidatorState) ->
    validate(schema, Body, ReturnBaseType, ValidatorState).

-spec validate(rule(), term(), request_param(), jesse_state:state()) ->
    ok | {ok, term()}.
validate(required, undefined, ReqParamName, _) ->
    validation_error(required, ReqParamName, undefined);
validate(required, _Value, _, _) ->
    ok;
validate(not_required, _Value, _, _) ->
    ok;
validate(_, undefined, _, _) ->
    ok;
validate({type, boolean}, Value, _, _) when is_boolean(Value) ->
    ok;
validate({type, integer}, Value, _, _) when is_integer(Value) ->
    ok;
validate({type, float}, Value, _, _) when is_float(Value) ->
    ok;
validate({type, binary}, Value, _, _) when is_binary(Value) ->
    ok;
validate({max, Max}, Value, _, _) when Value =< Max ->
    ok;
validate({min, Min}, Value, _, _) when Min =< Value ->
    ok;
validate({exclusive_max, Max}, Value, _, _) when Value < Max ->
    ok;
validate({exclusive_min, Min}, Value, _, _) when Min < Value ->
    ok;
validate({max_length, MaxLength}, Value, _, _) when is_binary(Value), byte_size(Value) =< MaxLength ->
    ok;
validate({min_length, MinLength}, Value, _, _) when is_binary(Value), MinLength =< byte_size(Value) ->
    ok;
validate(Rule = {type, byte}, Value, ReqParamName, _) when is_binary(Value) ->
    try base64:decode(Value) of
        Decoded -> {ok, Decoded}
    catch error:_Error -> validation_error(Rule, ReqParamName, Value)
    end;
validate(Rule = {type, boolean}, Value, ReqParamName, _) when is_binary(Value) ->
    case to_binary(string:lowercase(Value)) of
        <<"true">> -> {ok, true};
        <<"false">> -> {ok, false};
        _ -> validation_error(Rule, ReqParamName, Value)
    end;
validate(Rule = {type, integer}, Value, ReqParamName, _) when is_binary(Value) ->
    try
        {ok, binary_to_integer(Value)}
    catch
        error:badarg ->
            validation_error(Rule, ReqParamName, Value)
    end;
validate(Rule = {type, float}, Value, ReqParamName, _) when is_binary(Value) ->
    try
        {ok, binary_to_float(Value)}
    catch
        error:badarg ->
            validation_error(Rule, ReqParamName, Value)
    end;
validate(Rule = {type, date}, Value, ReqParamName, _) ->
    case is_binary(Value) of
        true -> ok;
        false -> validation_error(Rule, ReqParamName, Value)
    end;
validate(Rule = {type, datetime}, Value, ReqParamName, _) ->
    try calendar:rfc3339_to_system_time(binary_to_list(Value)) of
        _ -> ok
    catch error:_Error -> validation_error(Rule, ReqParamName, Value)
    end;
validate(Rule = {enum, Values}, Value, ReqParamName, _) ->
    try
        FormattedValue = erlang:binary_to_existing_atom(Value, utf8),
        case lists:member(FormattedValue, Values) of
            true -> {ok, FormattedValue};
            false -> validation_error(Rule, ReqParamName, Value)
        end
    catch
        error:badarg ->
            validation_error(Rule, ReqParamName, Value)
    end;
validate(Rule = {pattern, Pattern}, Value, ReqParamName, _) ->
    {ok, MP} = re:compile(Pattern),
    case re:run(Value, MP) of
        {match, _} -> ok;
        _ -> validation_error(Rule, ReqParamName, Value)
    end;
validate(schema, Value, ReqParamName, ValidatorState) ->
    Definition = iolist_to_binary(["#/components/schemas/", atom_to_binary(ReqParamName, utf8)]),
    validate({schema, object, Definition}, Value, ReqParamName, ValidatorState);
validate({schema, list, Definition}, Value, ReqParamName, ValidatorState) ->
    lists:foreach(
      fun(Item) ->
              validate({schema, object, Definition}, Item, ReqParamName, ValidatorState)
      end, Value);
validate(Rule = {schema, object, Definition}, Value, ReqParamName, ValidatorState) ->
    try
        _ = validate_with_schema(Value, Definition, ValidatorState),
        ok
    catch
        throw:[{schema_invalid, _, Error} | _] ->
            Info = #{
                type => schema_invalid,
                error => Error
            },
            validation_error(Rule, ReqParamName, Value, Info);
        throw:[{data_invalid, Schema, Error, _, Path} | _] ->
            Info = #{
                type => data_invalid,
                error => Error,
                schema => Schema,
                path => Path
            },
            validation_error(Rule, ReqParamName, Value, Info)
    end;
validate(Rule, Value, ReqParamName, _) ->
    validation_error(Rule, ReqParamName, Value).

-spec validation_error(rule(), request_param(), term()) -> no_return().
validation_error(ViolatedRule, Name, Value) ->
    validation_error(ViolatedRule, Name, Value, #{}).

-spec validation_error(rule(), request_param(), term(), Info :: #{_ := _}) -> no_return().
validation_error(ViolatedRule, Name, Value, Info) ->
    throw({wrong_param, Name, Value, ViolatedRule, Info}).

-spec get_value(body | qs_val | header | binding, request_param(), cowboy_req:req()) ->
    {any(), cowboy_req:req()} |
    {error, any(), cowboy_req:req()}.
get_value(body, _Name, Req0) ->
    {ok, Body, Req} = read_entire_body(Req0),
    case prepare_body(Body) of
        {error, Reason} ->
            {error, Reason, Req};
        Value ->
            {Value, Req}
    end;
get_value(qs_val, Name, Req) ->
    QS = cowboy_req:parse_qs(Req),
    Value = get_opt(to_qs(Name), QS),
    {Value, Req};
get_value(header, Name, Req) ->
    Headers = cowboy_req:headers(Req),
    Value = maps:get(to_header(Name), Headers, undefined),
    {Value, Req};
get_value(binding, Name, Req) ->
    Value = cowboy_req:binding(Name, Req),
    {Value, Req}.

-spec read_entire_body(cowboy_req:req()) -> {ok, binary(), cowboy_req:req()}.
read_entire_body(Req) ->
    read_entire_body(Req, []).

-spec read_entire_body(cowboy_req:req(), iodata()) -> {ok, binary(), cowboy_req:req()}.
read_entire_body(Request, Acc) -> % {
    case cowboy_req:read_body(Request) of
        {ok, Data, NewRequest} ->
            {ok, iolist_to_binary(lists:reverse([Data | Acc])), NewRequest};
        {more, Data, NewRequest} ->
            read_entire_body(NewRequest, [Data | Acc])
    end.

prepare_body(<<>>) ->
    <<>>;
prepare_body(Body) ->
    try
        json:decode(Body)
    catch
        error:Error ->
            {error, {invalid_json, Body, Error}}
    end.

validate_with_schema(Body, Definition, ValidatorState) ->
    jesse_schema_validator:validate_with_state(
        [{<<"$ref">>, Definition}],
        Body,
        ValidatorState
    ).

-spec prepare_param([rule()], request_param(), term(), jesse_state:state()) ->
    {ok, term()} | {error, Reason :: any()}.
prepare_param(Rules, ReqParamName, Value, ValidatorState) ->
    Fun = fun(Rule, Acc) ->
        case validate(Rule, Acc, ReqParamName, ValidatorState) of
            ok -> Acc;
            {ok, Prepared} -> Prepared
        end
    end,
    try
        Result = lists:foldl(Fun, Value, Rules),
        {ok, Result}
    catch
        throw:Reason ->
            {error, Reason}
    end.

-spec to_binary(iodata()) -> binary().
to_binary(V) when is_binary(V)  -> V;
to_binary(V) when is_list(V)    -> iolist_to_binary(V).

-spec to_header(request_param()) -> binary().
to_header(Name) ->
    to_binary(string:lowercase(atom_to_binary(Name, utf8))).

-spec to_qs(request_param()) -> binary().
to_qs(Name) ->
    atom_to_binary(Name, utf8).

-spec get_opt(any(), []) -> any().
get_opt(Key, Opts) ->
    get_opt(Key, Opts, undefined).

-spec get_opt(any(), [], any()) -> any().
get_opt(Key, Opts, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, Value} -> Value;
        false -> Default
    end.

get_openapi_path() ->
    {ok, AppName} = application:get_application(?MODULE),
    filename:join(priv_dir(AppName), "openapi.json").

-include_lib("kernel/include/file.hrl").

-spec priv_dir(Application :: atom()) -> file:name_all().
priv_dir(AppName) ->
    case code:priv_dir(AppName) of
        Value when is_list(Value) ->
            Value ++ "/";
        _Error ->
            select_priv_dir([filename:join(["apps", atom_to_list(AppName), "priv"]), "priv"])
     end.

select_priv_dir(Paths) ->
    case lists:dropwhile(fun test_priv_dir/1, Paths) of
        [Path | _] -> Path;
        _          -> exit(no_priv_dir)
    end.

test_priv_dir(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = directory}} ->
            false;
        _ ->
            true
    end.
