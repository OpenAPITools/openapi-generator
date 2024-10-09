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
       'test/auth/http/basic' => #{
            servers => [],
            base_path => "",
            path => "/auth/http/basic",
            method => <<"POST">>,
            handler => 'openapi_auth_handler'
        },
       'test/auth/http/bearer' => #{
            servers => [],
            base_path => "",
            path => "/auth/http/bearer",
            method => <<"POST">>,
            handler => 'openapi_auth_handler'
        },
       'test/binary/gif' => #{
            servers => [],
            base_path => "",
            path => "/binary/gif",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
       'test/body/application/octetstream/binary' => #{
            servers => [],
            base_path => "",
            path => "/body/application/octetstream/binary",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
       'test/body/multipart/formdata/array_of_binary' => #{
            servers => [],
            base_path => "",
            path => "/body/application/octetstream/array_of_binary",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
       'test/body/multipart/formdata/single_binary' => #{
            servers => [],
            base_path => "",
            path => "/body/application/octetstream/single_binary",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
       'test/echo/body/allOf/Pet' => #{
            servers => [],
            base_path => "",
            path => "/echo/body/allOf/Pet",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
       'test/echo/body/FreeFormObject/response_string' => #{
            servers => [],
            base_path => "",
            path => "/echo/body/FreeFormObject/response_string",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
       'test/echo/body/Pet' => #{
            servers => [],
            base_path => "",
            path => "/echo/body/Pet",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
       'test/echo/body/Pet/response_string' => #{
            servers => [],
            base_path => "",
            path => "/echo/body/Pet/response_string",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
       'test/echo/body/string_enum' => #{
            servers => [],
            base_path => "",
            path => "/echo/body/string_enum",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
       'test/echo/body/Tag/response_string' => #{
            servers => [],
            base_path => "",
            path => "/echo/body/Tag/response_string",
            method => <<"POST">>,
            handler => 'openapi_body_handler'
        },
       'test/form/integer/boolean/string' => #{
            servers => [],
            base_path => "",
            path => "/form/integer/boolean/string",
            method => <<"POST">>,
            handler => 'openapi_form_handler'
        },
       'test/form/object/multipart' => #{
            servers => [],
            base_path => "",
            path => "/form/object/multipart",
            method => <<"POST">>,
            handler => 'openapi_form_handler'
        },
       'test/form/oneof' => #{
            servers => [],
            base_path => "",
            path => "/form/oneof",
            method => <<"POST">>,
            handler => 'openapi_form_handler'
        },
       'test/header/integer/boolean/string/enums' => #{
            servers => [],
            base_path => "",
            path => "/header/integer/boolean/string/enums",
            method => <<"GET">>,
            handler => 'openapi_header_handler'
        },
       'tests/path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path}' => #{
            servers => [],
            base_path => "",
            path => "/path/string/:path_string/integer/:path_integer/:enum_nonref_string_path/:enum_ref_string_path",
            method => <<"GET">>,
            handler => 'openapi_path_handler'
        },
       'test/enum_ref_string' => #{
            servers => [],
            base_path => "",
            path => "/query/enum_ref_string",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        },
       'test/query/datetime/date/string' => #{
            servers => [],
            base_path => "",
            path => "/query/datetime/date/string",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        },
       'test/query/integer/boolean/string' => #{
            servers => [],
            base_path => "",
            path => "/query/integer/boolean/string",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        },
       'test/query/style_deepObject/explode_true/object' => #{
            servers => [],
            base_path => "",
            path => "/query/style_deepObject/explode_true/object",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        },
       'test/query/style_deepObject/explode_true/object/allOf' => #{
            servers => [],
            base_path => "",
            path => "/query/style_deepObject/explode_true/object/allOf",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        },
       'test/query/style_form/explode_false/array_integer' => #{
            servers => [],
            base_path => "",
            path => "/query/style_form/explode_false/array_integer",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        },
       'test/query/style_form/explode_false/array_string' => #{
            servers => [],
            base_path => "",
            path => "/query/style_form/explode_false/array_string",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        },
       'test/query/style_form/explode_true/array_string' => #{
            servers => [],
            base_path => "",
            path => "/query/style_form/explode_true/array_string",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        },
       'test/query/style_form/explode_true/object' => #{
            servers => [],
            base_path => "",
            path => "/query/style_form/explode_true/object",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        },
       'test/query/style_form/explode_true/object/allOf' => #{
            servers => [],
            base_path => "",
            path => "/query/style_form/explode_true/object/allOf",
            method => <<"GET">>,
            handler => 'openapi_query_handler'
        }
    }.
