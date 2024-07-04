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

-spec get_paths(LogicHandler :: atom()) ->  [{'_',[{
    Path :: string(),
    Handler :: atom(),
    InitOpts :: init_opts()
}]}].

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
        'AddPet' => #{
            path => "/v2/pet",
            method => <<"POST">>,
            handler => 'openapi_pet_handler'
        },
        'DeletePet' => #{
            path => "/v2/pet/:petId",
            method => <<"DELETE">>,
            handler => 'openapi_pet_handler'
        },
        'FindPetsByStatus' => #{
            path => "/v2/pet/findByStatus",
            method => <<"GET">>,
            handler => 'openapi_pet_handler'
        },
        'FindPetsByTags' => #{
            path => "/v2/pet/findByTags",
            method => <<"GET">>,
            handler => 'openapi_pet_handler'
        },
        'GetPetById' => #{
            path => "/v2/pet/:petId",
            method => <<"GET">>,
            handler => 'openapi_pet_handler'
        },
        'UpdatePet' => #{
            path => "/v2/pet",
            method => <<"PUT">>,
            handler => 'openapi_pet_handler'
        },
        'UpdatePetWithForm' => #{
            path => "/v2/pet/:petId",
            method => <<"POST">>,
            handler => 'openapi_pet_handler'
        },
        'UploadFile' => #{
            path => "/v2/pet/:petId/uploadImage",
            method => <<"POST">>,
            handler => 'openapi_pet_handler'
        },
        'DeleteOrder' => #{
            path => "/v2/store/order/:orderId",
            method => <<"DELETE">>,
            handler => 'openapi_store_handler'
        },
        'GetInventory' => #{
            path => "/v2/store/inventory",
            method => <<"GET">>,
            handler => 'openapi_store_handler'
        },
        'GetOrderById' => #{
            path => "/v2/store/order/:orderId",
            method => <<"GET">>,
            handler => 'openapi_store_handler'
        },
        'PlaceOrder' => #{
            path => "/v2/store/order",
            method => <<"POST">>,
            handler => 'openapi_store_handler'
        },
        'CreateUser' => #{
            path => "/v2/user",
            method => <<"POST">>,
            handler => 'openapi_user_handler'
        },
        'CreateUsersWithArrayInput' => #{
            path => "/v2/user/createWithArray",
            method => <<"POST">>,
            handler => 'openapi_user_handler'
        },
        'CreateUsersWithListInput' => #{
            path => "/v2/user/createWithList",
            method => <<"POST">>,
            handler => 'openapi_user_handler'
        },
        'DeleteUser' => #{
            path => "/v2/user/:username",
            method => <<"DELETE">>,
            handler => 'openapi_user_handler'
        },
        'GetUserByName' => #{
            path => "/v2/user/:username",
            method => <<"GET">>,
            handler => 'openapi_user_handler'
        },
        'LoginUser' => #{
            path => "/v2/user/login",
            method => <<"GET">>,
            handler => 'openapi_user_handler'
        },
        'LogoutUser' => #{
            path => "/v2/user/logout",
            method => <<"GET">>,
            handler => 'openapi_user_handler'
        },
        'UpdateUser' => #{
            path => "/v2/user/:username",
            method => <<"PUT">>,
            handler => 'openapi_user_handler'
        }
    }.

get_validator_state() ->
    persistent_term:get({?MODULE, validator_state}).


prepare_validator() ->
    R = jsx:decode(element(2, file:read_file(get_openapi_path()))),
    JesseState = jesse_state:new(R, [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>}]),
    persistent_term:put({?MODULE, validator_state}, JesseState),
    ?MODULE.


get_openapi_path() ->
    {ok, AppName} = application:get_application(?MODULE),
    filename:join(openapi_utils:priv_dir(AppName), "openapi.json").
