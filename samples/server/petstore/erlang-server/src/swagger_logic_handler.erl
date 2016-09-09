-module(swagger_logic_handler).

-export([handle_request/4]).
-export([authorize_api_key/3]).
-type context() :: #{binary() => any()}.
-type handler_response() ::{
    Status :: cowboy:http_status(),
    Headers :: cowboy:http_headers(),
    Body :: #{}
}.

-export_type([handler_response/0]).

-callback authorize_api_key(
    OperationID :: swagger_api:operation_id(),
    ApiKey :: binary()
) ->
    Result :: boolean() | {boolean(), context()}.


-callback handle_request(OperationID :: swagger_api:operation_id(), Request :: any(), Context :: context()) ->
    handler_response().

-spec handle_request(
    Handler :: atom(),
    OperationID :: swagger_api:operation_id(),
    Request :: any(),
    Context :: context()
) ->
    handler_response().

handle_request(Handler, OperationID, Req, Context) ->
    Handler:handle_request(OperationID, Req, Context).

-spec authorize_api_key(Handler :: atom(), OperationID :: swagger_api:operation_id(), ApiKey :: binary()) ->
    Result :: false | {true, context()}.
authorize_api_key(Handler, OperationID, ApiKey) ->
    Handler:authorize_api_key(OperationID, ApiKey).
