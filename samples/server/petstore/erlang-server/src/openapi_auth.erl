-module(openapi_auth).

-export([authorize_api_key/5]).

-spec authorize_api_key(
    LogicHandler :: atom(),
    OperationID :: openapi_api:operation_id(),
    From :: header | qs_val,
    KeyParam :: iodata() | atom(),
    Req ::cowboy_req:req()
)-> {true, Context :: #{binary() => any()}, Req ::cowboy_req:req()} |
    {false, AuthHeader :: binary(), Req ::cowboy_req:req()}.

authorize_api_key(LogicHandler, OperationID, From, KeyParam, Req0) ->
    {ApiKey, Req} = get_api_key(From, KeyParam, Req0),
    case ApiKey of
        undefined ->
            AuthHeader = <<"">>,
            {false, AuthHeader, Req};
        _ ->
            Result = openapi_logic_handler:authorize_api_key(
                LogicHandler,
                OperationID,
                ApiKey
            ),
            case Result of
                {true, Context}  ->
                    {true, Context, Req};
                false ->
                    AuthHeader = <<"">>,
                    {false, AuthHeader, Req}
            end
    end.

get_api_key(header, KeyParam, Req) ->
    Headers = cowboy_req:headers(Req),
    {
        maps:get(
            openapi_utils:to_header(KeyParam),
            Headers,
            undefined
        ),
        Req
    };

get_api_key(qs_val, KeyParam, Req) ->
    QS = cowboy_req:parse_qs(Req),
    { openapi_utils:get_opt(KeyParam, QS), Req}.
