-module(openapi_default_logic_handler).

-behaviour(openapi_logic_handler).

-include_lib("kernel/include/logger.hrl").

-export([handle_request/3]).


-spec handle_request(
        OperationID :: openapi_api:operation_id(),
        Req :: cowboy_req:req(),
        Context :: #{_ => _}) ->
    {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: json:encode_value()}.

handle_request(OperationID, Req, Context) ->
    ?LOG_ERROR(#{what => "Got not implemented request to process",
                 operation_id => OperationID, request => Req, context => Context}),
    {501, #{}, #{}}.
