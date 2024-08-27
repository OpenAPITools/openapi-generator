-module(openapi_router).

-export([get_paths/1, get_validator_state/0]).

-type operations() :: #{
    Method :: binary() => openapi_api:operation_id()
}.

-type init_opts()  :: {
    Operations :: operations(),
    LogicHandler :: atom(),
    ValidatorMod :: module()
}.

-export_type([init_opts/0]).

-spec get_paths(LogicHandler :: atom()) -> cowboy_router:routes().
get_paths(LogicHandler) ->
    ValidatorState = prepare_validator(),
    PreparedPaths = maps:fold(
        fun(Path, #{operations := Operations, handler := Handler}, Acc) ->
            [{Path, Handler, Operations} | Acc]
        end,
        [],
        group_paths()
    ),
    [
        {'_',
            [{P, H, {O, LogicHandler, ValidatorState}} || {P, H, O} <- PreparedPaths]
        }
    ].

group_paths() ->
    maps:fold(
        fun(OperationID, #{path := Path, method := Method, handler := Handler}, Acc) ->
            case maps:find(Path, Acc) of
                {ok, PathInfo0 = #{operations := Operations0}} ->
                    Operations = Operations0#{Method => OperationID},
                    PathInfo = PathInfo0#{operations => Operations},
                    Acc#{Path => PathInfo};
                error ->
                    Operations = #{Method => OperationID},
                    PathInfo = #{handler => Handler, operations => Operations},
                    Acc#{Path => PathInfo}
            end
        end,
        #{},
        get_operations()
    ).

get_operations() ->
    #{ 
        'TestAuthHttpBasic' => #{
            path => "/auth/http/basic",
            method => <<"POST">>,
            handler => 'openapi_auth_handler'
        },
        'TestAuthHttpBearer' => #{
            path => "/auth/http/bearer",
            method => <<"POST">>,
            handler => 'openapi_auth_handler'
        },
        'TestBinaryGif' => #{
            path => "/binary/gif",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
        'TestBodyApplicationOctetstreamBinary' => #{
            path => "/body/application/octetstream/binary",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
        'TestBodyMultipartFormdataArrayOfBinary' => #{
            path => "/body/application/octetstream/array_of_binary",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
        'TestBodyMultipartFormdataSingleBinary' => #{
            path => "/body/application/octetstream/single_binary",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
        'TestEchoBodyAllOfPet' => #{
            path => "/echo/body/allOf/Pet",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
        'TestEchoBodyFreeFormObjectResponseString' => #{
            path => "/echo/body/FreeFormObject/response_string",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
        'TestEchoBodyPet' => #{
            path => "/echo/body/Pet",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
        'TestEchoBodyPetResponseString' => #{
            path => "/echo/body/Pet/response_string",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
        'TestEchoBodyStringEnum' => #{
            path => "/echo/body/string_enum",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
        'TestEchoBodyTagResponseString' => #{
            path => "/echo/body/Tag/response_string",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
        'TestFormIntegerBooleanString' => #{
            path => "/form/integer/boolean/string",
            method => <<"POST">>,
            handler => 'openapi_form_handler'
        },
        'TestFormObjectMultipart' => #{
            path => "/form/object/multipart",
            method => <<"POST">>,
            handler => 'openapi_form_handler'
        },
        'TestFormOneof' => #{
            path => "/form/oneof",
            method => <<"POST">>,
            handler => 'openapi_form_handler'
        },
        'TestHeaderIntegerBooleanStringEnums' => #{
            path => "/header/integer/boolean/string/enums",
            method => <<"GET">>,
            handler => 'openapi_header_handler'
        },
        'TestsPathString{pathString}Integer{pathInteger}{enumNonrefStringPath}{enumRefStringPath}' => #{
            path => "/path/string/:path_string/integer/:path_integer/:enum_nonref_string_path/:enum_ref_string_path",
            method => <<"GET">>,
            handler => 'openapi_path_handler'
        },
        'TestEnumRefString' => #{
            path => "/query/enum_ref_string",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        },
        'TestQueryDatetimeDateString' => #{
            path => "/query/datetime/date/string",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        },
        'TestQueryIntegerBooleanString' => #{
            path => "/query/integer/boolean/string",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        },
        'TestQueryStyleDeepObjectExplodeTrueObject' => #{
            path => "/query/style_deepObject/explode_true/object",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        },
        'TestQueryStyleDeepObjectExplodeTrueObjectAllOf' => #{
            path => "/query/style_deepObject/explode_true/object/allOf",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        },
        'TestQueryStyleFormExplodeFalseArrayInteger' => #{
            path => "/query/style_form/explode_false/array_integer",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        },
        'TestQueryStyleFormExplodeFalseArrayString' => #{
            path => "/query/style_form/explode_false/array_string",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        },
        'TestQueryStyleFormExplodeTrueArrayString' => #{
            path => "/query/style_form/explode_true/array_string",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        },
        'TestQueryStyleFormExplodeTrueObject' => #{
            path => "/query/style_form/explode_true/object",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        },
        'TestQueryStyleFormExplodeTrueObjectAllOf' => #{
            path => "/query/style_form/explode_true/object/allOf",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        }
    }.

get_validator_state() ->
    persistent_term:get({?MODULE, validator_state}).

prepare_validator() ->
    {ok, FileContents} = file:read_file(get_openapi_path()),
    R = json:decode(FileContents),
    JesseState = jesse_state:new(R, [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>}]),
    persistent_term:put({?MODULE, validator_state}, JesseState),
    ?MODULE.

get_openapi_path() ->
    {ok, AppName} = application:get_application(?MODULE),
    filename:join(openapi_utils:priv_dir(AppName), "openapi.json").
