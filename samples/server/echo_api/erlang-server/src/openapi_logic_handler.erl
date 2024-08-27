-module(openapi_logic_handler).

-export([handle_request/4]).
-type context() :: #{binary() => any()}.
-type handler_response() ::{cowboy:http_status(), cowboy:http_headers(), json:encode_value()}.

-export_type([handler_response/0]).


-callback handle_request(openapi_api:operation_id(), cowboy_req:req(), context()) ->
    handler_response().

-spec handle_request(module(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
    handler_response().
handle_request(Handler, OperationID, Req, Context) ->
    Handler:handle_request(OperationID, Req, Context).

