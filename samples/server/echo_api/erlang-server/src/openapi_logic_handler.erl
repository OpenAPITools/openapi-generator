-module(openapi_logic_handler).

-include_lib("kernel/include/logger.hrl").

-type api_key_callback() ::
    fun((openapi_api:operation_id(), binary()) -> {true, context()} | {false, iodata()}).
-type accept_callback() ::
    fun((atom(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
            boolean() | {created, iodata()} | {see_other, iodata()}).
-type provide_callback() ::
    fun((atom(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
            {cowboy_req:resp_body(), cowboy_req:req(), context()}).
-type context() :: #{binary() => any()}.

-export_type([context/0, api_key_callback/0, accept_callback/0, provide_callback/0]).

-optional_callbacks([api_key_callback/2]).

-callback api_key_callback(openapi_api:operation_id(), binary()) ->
    {true, context()} | {false, iodata()}.

-callback accept_callback(atom(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
    boolean() | {created, iodata()} | {see_other, iodata()}.

-callback provide_callback(atom(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
    {cowboy_req:resp_body(), cowboy_req:req(), context()}.

-export([api_key_callback/2, accept_callback/4, provide_callback/4]).
-ignore_xref([api_key_callback/2, accept_callback/4, provide_callback/4]).

-spec api_key_callback(openapi_api:operation_id(), binary()) -> {true, #{}}.
api_key_callback(OperationID, ApiKey) ->
    ?LOG_ERROR(#{what => "Got not implemented api_key_callback request",
                 operation_id => OperationID,
                 api_key => ApiKey}),
    {true, #{}}.

-spec accept_callback(atom(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
    {cowboy:http_status(), cowboy:http_headers(), json:encode_value()}.
accept_callback(Class, OperationID, Req, Context) ->
    ?LOG_ERROR(#{what => "Got not implemented request to process",
                 class => Class,
                 operation_id => OperationID,
                 request => Req,
                 context => Context}),
    {501, #{}, #{}}.

-spec provide_callback(atom(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
    {cowboy_req:resp_body(), cowboy_req:req(), context()}.
provide_callback(Class, OperationID, Req, Context) ->
    ?LOG_ERROR(#{what => "Got not implemented request to process",
                 class => Class,
                 operation_id => OperationID,
                 request => Req,
                 context => Context}),
    {<<>>, Req, Context}.
