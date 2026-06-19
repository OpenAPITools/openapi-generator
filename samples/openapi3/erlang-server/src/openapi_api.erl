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
    'pet'
    | 'store'
    | 'user'.


-type operation_id() ::
    'addPet' | %% Add a new pet to the store
    'deletePet' | %% Deletes a pet
    'findPetsByStatus' | %% Finds Pets by status
    'findPetsByTags' | %% Finds Pets by tags
    'getPetById' | %% Find pet by ID
    'updatePet' | %% Update an existing pet
    'updatePetWithForm' | %% Updates a pet in the store with form data
    'uploadFile' | %% uploads an image
    'deleteOrder' | %% Delete purchase order by ID
    'getInventory' | %% Returns pet inventories by status
    'getOrderById' | %% Find purchase order by ID
    'placeOrder' | %% Place an order for a pet
    'createUser' | %% Create user
    'createUsersWithArrayInput' | %% Creates list of users with given input array
    'createUsersWithListInput' | %% Creates list of users with given input array
    'deleteUser' | %% Delete user
    'getUserByName' | %% Get user by user name
    'loginUser' | %% Logs user into the system
    'logoutUser' | %% Logs out current logged in user session
    'updateUser' | %% Updated user
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
validate_response('addPet', 200, Body, ValidatorState) ->
    validate_response_body('Pet', 'Pet', Body, ValidatorState);
validate_response('addPet', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('deletePet', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('findPetsByStatus', 200, Body, ValidatorState) ->
    validate_response_body('list', 'Pet', Body, ValidatorState);
validate_response('findPetsByStatus', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('findPetsByTags', 200, Body, ValidatorState) ->
    validate_response_body('list', 'Pet', Body, ValidatorState);
validate_response('findPetsByTags', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('getPetById', 200, Body, ValidatorState) ->
    validate_response_body('Pet', 'Pet', Body, ValidatorState);
validate_response('getPetById', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('getPetById', 404, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('updatePet', 200, Body, ValidatorState) ->
    validate_response_body('Pet', 'Pet', Body, ValidatorState);
validate_response('updatePet', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('updatePet', 404, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('updatePet', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('updatePetWithForm', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('uploadFile', 200, Body, ValidatorState) ->
    validate_response_body('ApiResponse', 'ApiResponse', Body, ValidatorState);
validate_response('deleteOrder', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('deleteOrder', 404, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('getInventory', 200, Body, ValidatorState) ->
    validate_response_body('map', 'integer', Body, ValidatorState);
validate_response('getOrderById', 200, Body, ValidatorState) ->
    validate_response_body('Order', 'Order', Body, ValidatorState);
validate_response('getOrderById', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('getOrderById', 404, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('placeOrder', 200, Body, ValidatorState) ->
    validate_response_body('Order', 'Order', Body, ValidatorState);
validate_response('placeOrder', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('createUser', 0, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('createUsersWithArrayInput', 0, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('createUsersWithListInput', 0, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('deleteUser', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('deleteUser', 404, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('getUserByName', 200, Body, ValidatorState) ->
    validate_response_body('User', 'User', Body, ValidatorState);
validate_response('getUserByName', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('getUserByName', 404, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('loginUser', 200, Body, ValidatorState) ->
    validate_response_body('binary', 'string', Body, ValidatorState);
validate_response('loginUser', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('logoutUser', 0, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('updateUser', 400, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('updateUser', 404, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response(_OperationID, _Code, _Body, _ValidatorState) ->
    ok.

%%%
-spec request_params(OperationID :: operation_id()) -> [Param :: request_param()].
request_params('addPet') ->
    [
        'Pet'
    ];
request_params('deletePet') ->
    [
        'petId',
        'api_key'
    ];
request_params('findPetsByStatus') ->
    [
        'status'
    ];
request_params('findPetsByTags') ->
    [
        'tags'
    ];
request_params('getPetById') ->
    [
        'petId'
    ];
request_params('updatePet') ->
    [
        'Pet'
    ];
request_params('updatePetWithForm') ->
    [
        'petId',
        'name',
        'status'
    ];
request_params('uploadFile') ->
    [
        'petId',
        'additionalMetadata',
        'file'
    ];
request_params('deleteOrder') ->
    [
        'orderId'
    ];
request_params('getInventory') ->
    [
    ];
request_params('getOrderById') ->
    [
        'orderId'
    ];
request_params('placeOrder') ->
    [
        'Order'
    ];
request_params('createUser') ->
    [
        'User'
    ];
request_params('createUsersWithArrayInput') ->
    [
        'array'
    ];
request_params('createUsersWithListInput') ->
    [
        'array'
    ];
request_params('deleteUser') ->
    [
        'username'
    ];
request_params('getUserByName') ->
    [
        'username'
    ];
request_params('loginUser') ->
    [
        'username',
        'password'
    ];
request_params('logoutUser') ->
    [
    ];
request_params('updateUser') ->
    [
        'username',
        'User'
    ];
request_params(_) ->
    error(unknown_operation).

-spec request_param_info(OperationID :: operation_id(), Name :: request_param()) ->
    #{source => qs_val | binding | header | body, rules => [rule()]}.
request_param_info('addPet', 'Pet') ->
    #{
        source => body,
        rules => [
            {schema, object, <<"#/components/schemas/Pet">>},
            required
        ]
    };
request_param_info('deletePet', 'petId') ->
    #{
        source => binding,
        rules => [
            {type, integer},
            required
        ]
    };
request_param_info('deletePet', 'api_key') ->
    #{
        source => header,
        rules => [
            {type, binary},
            not_required
        ]
    };
request_param_info('findPetsByStatus', 'status') ->
    #{
        source => qs_val,
        rules => [
            {enum, ['available', 'pending', 'sold'] },
            required
        ]
    };
request_param_info('findPetsByTags', 'tags') ->
    #{
        source => qs_val,
        rules => [
            required
        ]
    };
request_param_info('getPetById', 'petId') ->
    #{
        source => binding,
        rules => [
            {type, integer},
            required
        ]
    };
request_param_info('updatePet', 'Pet') ->
    #{
        source => body,
        rules => [
            {schema, object, <<"#/components/schemas/Pet">>},
            required
        ]
    };
request_param_info('updatePetWithForm', 'petId') ->
    #{
        source => binding,
        rules => [
            {type, integer},
            required
        ]
    };
request_param_info('updatePetWithForm', 'name') ->
    #{
        source => body,
        rules => [
            {type, binary},
            not_required
        ]
    };
request_param_info('updatePetWithForm', 'status') ->
    #{
        source => body,
        rules => [
            {type, binary},
            not_required
        ]
    };
request_param_info('uploadFile', 'petId') ->
    #{
        source => binding,
        rules => [
            {type, integer},
            required
        ]
    };
request_param_info('uploadFile', 'additionalMetadata') ->
    #{
        source => body,
        rules => [
            {type, binary},
            not_required
        ]
    };
request_param_info('uploadFile', 'file') ->
    #{
        source => body,
        rules => [
            {type, binary},
            not_required
        ]
    };
request_param_info('deleteOrder', 'orderId') ->
    #{
        source => binding,
        rules => [
            {type, binary},
            required
        ]
    };
request_param_info('getOrderById', 'orderId') ->
    #{
        source => binding,
        rules => [
            {type, integer},
            {max, 5},
            {min, 1},
            required
        ]
    };
request_param_info('placeOrder', 'Order') ->
    #{
        source => body,
        rules => [
            {schema, object, <<"#/components/schemas/Order">>},
            required
        ]
    };
request_param_info('createUser', 'User') ->
    #{
        source => body,
        rules => [
            {schema, object, <<"#/components/schemas/User">>},
            required
        ]
    };
request_param_info('createUsersWithArrayInput', 'array') ->
    #{
        source => body,
        rules => [
            {schema, list, <<"#/components/schemas/User">>},
            required
        ]
    };
request_param_info('createUsersWithListInput', 'array') ->
    #{
        source => body,
        rules => [
            {schema, list, <<"#/components/schemas/User">>},
            required
        ]
    };
request_param_info('deleteUser', 'username') ->
    #{
        source => binding,
        rules => [
            {type, binary},
            required
        ]
    };
request_param_info('getUserByName', 'username') ->
    #{
        source => binding,
        rules => [
            {type, binary},
            required
        ]
    };
request_param_info('loginUser', 'username') ->
    #{
        source => qs_val,
        rules => [
            {type, binary},
            {pattern, "^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$"},
            required
        ]
    };
request_param_info('loginUser', 'password') ->
    #{
        source => qs_val,
        rules => [
            {type, binary},
            required
        ]
    };
request_param_info('updateUser', 'username') ->
    #{
        source => binding,
        rules => [
            {type, binary},
            required
        ]
    };
request_param_info('updateUser', 'User') ->
    #{
        source => body,
        rules => [
            {schema, object, <<"#/components/schemas/User">>},
            required
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
