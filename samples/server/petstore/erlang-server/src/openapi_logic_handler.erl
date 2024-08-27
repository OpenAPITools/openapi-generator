-module(openapi_logic_handler).

-export([handle_request/4]).
-type context() :: #{binary() => any()}.
-type handler_response() ::{cowboy:http_status(), cowboy:http_headers(), json:encode_value()}.

-export_type([handler_response/0]).

-callback authorize_api_key(openapi_api:operation_id(), binary()) ->
    boolean() | {boolean(), context()}.

-callback handle_request(openapi_api:operation_id(), cowboy_req:req(), context()) ->
    handler_response().

-spec handle_request(module(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
    handler_response().
handle_request(Handler, OperationID, Req, Context) ->
    Handler:handle_request(OperationID, Req, Context).

-spec authorize_api_key(module(), openapi_api:operation_id(), binary()) ->
    Result :: false | {true, context()}.
authorize_api_key(Handler, OperationID, ApiKey) ->
    Handler:authorize_api_key(OperationID, ApiKey).
