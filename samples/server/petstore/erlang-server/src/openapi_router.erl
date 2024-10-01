-module(openapi_router).

-export([get_paths/1]).

-type method() :: binary().
-type operations() :: #{method() => openapi_api:operation_id()}.
-type init_opts()  :: {operations(), module()}.

-export_type([init_opts/0]).

-spec get_paths(LogicHandler :: module()) -> cowboy_router:routes().
get_paths(LogicHandler) ->
    PreparedPaths = maps:fold(
                      fun(Path, #{operations := Operations, handler := Handler}, Acc) ->
                              [{Path, Handler, Operations} | Acc]
                      end, [], group_paths()
                     ),
    [{'_', [{P, H, {O, LogicHandler}} || {P, H, O} <- PreparedPaths]}].

group_paths() ->
    maps:fold(
      fun(OperationID, #{servers := Servers, base_path := BasePath, path := Path,
                         method := Method, handler := Handler}, Acc) ->
              FullPaths = build_full_paths(Servers, BasePath, Path),
              merge_paths(FullPaths, OperationID, Method, Handler, Acc)
      end, #{}, get_operations()).

build_full_paths([], BasePath, Path) ->
    [lists:append([BasePath, Path])];
build_full_paths(Servers, _BasePath, Path) ->
    [lists:append([Server, Path]) || Server <- Servers ].

merge_paths(FullPaths, OperationID, Method, Handler, Acc) ->
    lists:foldl(
      fun(Path, Acc0) ->
              case maps:find(Path, Acc0) of
                  {ok, PathInfo0 = #{operations := Operations0}} ->
                      Operations = Operations0#{Method => OperationID},
                      PathInfo = PathInfo0#{operations => Operations},
                      Acc0#{Path => PathInfo};
                  error ->
                      Operations = #{Method => OperationID},
                      PathInfo = #{handler => Handler, operations => Operations},
                      Acc0#{Path => PathInfo}
              end
      end, Acc, FullPaths).

get_operations() ->
    #{ 
       'AddPet' => #{
            servers => [],
            base_path => "/v2",
            path => "/pet",
            method => <<"POST">>,
            handler => 'openapi_pet_handler'
        },
       'DeletePet' => #{
            servers => [],
            base_path => "/v2",
            path => "/pet/:petId",
            method => <<"DELETE">>,
            handler => 'openapi_pet_handler'
        },
       'FindPetsByStatus' => #{
            servers => [],
            base_path => "/v2",
            path => "/pet/findByStatus",
            method => <<"GET">>,
            handler => 'openapi_pet_handler'
        },
       'FindPetsByTags' => #{
            servers => [],
            base_path => "/v2",
            path => "/pet/findByTags",
            method => <<"GET">>,
            handler => 'openapi_pet_handler'
        },
       'GetPetById' => #{
            servers => [],
            base_path => "/v2",
            path => "/pet/:petId",
            method => <<"GET">>,
            handler => 'openapi_pet_handler'
        },
       'UpdatePet' => #{
            servers => [],
            base_path => "/v2",
            path => "/pet",
            method => <<"PUT">>,
            handler => 'openapi_pet_handler'
        },
       'UpdatePetWithForm' => #{
            servers => [],
            base_path => "/v2",
            path => "/pet/:petId",
            method => <<"POST">>,
            handler => 'openapi_pet_handler'
        },
       'UploadFile' => #{
            servers => [],
            base_path => "/v2",
            path => "/pet/:petId/uploadImage",
            method => <<"POST">>,
            handler => 'openapi_pet_handler'
        },
       'DeleteOrder' => #{
            servers => [],
            base_path => "/v2",
            path => "/store/order/:orderId",
            method => <<"DELETE">>,
            handler => 'openapi_store_handler'
        },
       'GetInventory' => #{
            servers => [],
            base_path => "/v2",
            path => "/store/inventory",
            method => <<"GET">>,
            handler => 'openapi_store_handler'
        },
       'GetOrderById' => #{
            servers => [],
            base_path => "/v2",
            path => "/store/order/:orderId",
            method => <<"GET">>,
            handler => 'openapi_store_handler'
        },
       'PlaceOrder' => #{
            servers => [],
            base_path => "/v2",
            path => "/store/order",
            method => <<"POST">>,
            handler => 'openapi_store_handler'
        },
       'CreateUser' => #{
            servers => [],
            base_path => "/v2",
            path => "/user",
            method => <<"POST">>,
            handler => 'openapi_user_handler'
        },
       'CreateUsersWithArrayInput' => #{
            servers => [],
            base_path => "/v2",
            path => "/user/createWithArray",
            method => <<"POST">>,
            handler => 'openapi_user_handler'
        },
       'CreateUsersWithListInput' => #{
            servers => [],
            base_path => "/v2",
            path => "/user/createWithList",
            method => <<"POST">>,
            handler => 'openapi_user_handler'
        },
       'DeleteUser' => #{
            servers => [],
            base_path => "/v2",
            path => "/user/:username",
            method => <<"DELETE">>,
            handler => 'openapi_user_handler'
        },
       'GetUserByName' => #{
            servers => [],
            base_path => "/v2",
            path => "/user/:username",
            method => <<"GET">>,
            handler => 'openapi_user_handler'
        },
       'LoginUser' => #{
            servers => [],
            base_path => "/v2",
            path => "/user/login",
            method => <<"GET">>,
            handler => 'openapi_user_handler'
        },
       'LogoutUser' => #{
            servers => [],
            base_path => "/v2",
            path => "/user/logout",
            method => <<"GET">>,
            handler => 'openapi_user_handler'
        },
       'UpdateUser' => #{
            servers => [],
            base_path => "/v2",
            path => "/user/:username",
            method => <<"PUT">>,
            handler => 'openapi_user_handler'
        }
    }.
