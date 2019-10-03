-module(petstore_utils).

-export([ request/2
        , request/4
        ]).

-type response() :: #{ status  := integer()
                     , headers := map()
                     , body    := iolist()
                     }.

-export_type([response/0]).

-spec request(atom(), string()) -> response().
request(Method, Url) ->
  request(Method, Url, undefined, undefined).

-spec request(atom(), iolist(), iolist(), string()) -> response().
request(Method, Url0, Body, ContentType) ->
  Url         = binary_to_list(iolist_to_binary(Url0)),
  Headers     = headers(),
  Request     = case Body of
                  undefined -> {Url, Headers};
                  _         -> {Url, Headers, ContentType, Body}
                end,
  HTTPOptions = [{autoredirect, true}],
  Options     = [],
  %% Disable pipelining to avoid the socket getting closed during long runs
  ok = httpc:set_options([ {max_keep_alive_length, 0}
                         , {max_pipeline_length, 0}
                         , {max_sessions, 0}
                         ]),
  Result = httpc:request(Method, Request, HTTPOptions, Options),
  {ok, {{_Ver, Status, _Phrase}, RespHeaders, RespBody}} = Result,

  Response = #{ status  => Status
              , headers => maps:from_list(RespHeaders)
              , body    => RespBody
              },
  decode_body(Response).

-spec headers() -> [{string(), string()}].
headers() ->
  [ {"Accept",        "application/json"}
  | basic_auth()
  ].

-spec basic_auth() -> [{string(), string()}].
basic_auth() ->
  case application:get_env(petstore, basic_auth, undefined) of
    undefined -> [];
    {Username, Password} ->
      Credentials = base64:encode_to_string(Username ++ ":" ++ Password),
      [{"Authorization", "Basic " ++ Credentials}]
  end.

-spec decode_body(response()) -> response().
decode_body(#{ headers := #{"content-type" := "application/json"}
             , body    := Body
             } = Response) ->
  Json = jsx:decode( unicode:characters_to_binary(Body)
                   , [return_maps, {labels, atom}]
                   ),
  Response#{body_json => Json};
decode_body(Response) ->
  Response.
