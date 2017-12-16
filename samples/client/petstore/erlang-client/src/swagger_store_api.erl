-module(swagger_store_api).

-export([delete_order/1, delete_order/2,
         get_inventory/0, get_inventory/1,
         get_order_by_id/1, get_order_by_id/2,
         place_order/1, place_order/2]).

-define(BASE_URL, <<"http://petstore.swagger.io/v2">>).

%% @doc Delete purchase order by ID
%% For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
-spec delete_order(binary()) -> ok | {error, integer()}.
delete_order(OrderId) ->
    delete_order(OrderId, , #{}).

-spec delete_order(binary(), maps:map()) -> ok | {error, integer()}.
delete_order(OrderId, _Optional) ->
    Method = delete,
    Path = ["/store/order/", OrderId, ""],
    QS = [],
    Headers = [],
    Body1 = [],
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, _RespHeaders, _ClientRef} ->
             ok;
        {ok, Status, _RespHeaders, _ClientRef} ->
             {error, Status}
    end.

%% @doc Returns pet inventories by status
%% Returns a map of status codes to quantities
-spec get_inventory() -> {ok, list(), maps:map()} | {error, string()}.
get_inventory() ->
    get_inventory(, #{}).

-spec get_inventory(maps:map()) -> {ok, list(), maps:map()} | {error, string()}.
get_inventory(_Optional) ->
    Method = get,
    Path = ["/store/inventory"],
    QS = [],
    Headers = [],
    Body1 = [],
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, RespHeaders, ClientRef} ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            {ok, RespHeaders, jsx:decode(ResponseBody, [return_maps])}
    end.

%% @doc Find purchase order by ID
%% For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
-spec get_order_by_id(integer()) -> {ok, list(), swagger_order:swagger_order()} | {error, string()}.
get_order_by_id(OrderId) ->
    get_order_by_id(OrderId, , #{}).

-spec get_order_by_id(integer(), maps:map()) -> {ok, list(), swagger_order:swagger_order()} | {error, string()}.
get_order_by_id(OrderId, _Optional) ->
    Method = get,
    Path = ["/store/order/", OrderId, ""],
    QS = [],
    Headers = [],
    Body1 = [],
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, RespHeaders, ClientRef} ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            {ok, RespHeaders, jsx:decode(ResponseBody, [return_maps])}; 
        {ok, 400, _RespHeaders, _ClientRef} ->
            {error, "Invalid ID supplied"}; 
        {ok, 404, _RespHeaders, _ClientRef} ->
            {error, "Order not found"}
    end.

%% @doc Place an order for a pet
-spec place_order(swagger_order:swagger_order(), term()) -> {ok, list(), swagger_order:swagger_order()} | {error, string()}.
place_order(Body) ->
    place_order(Body, Body, #{}).

-spec place_order(swagger_order:swagger_order(), term(), maps:map()) -> {ok, list(), swagger_order:swagger_order()} | {error, string()}.
place_order(Body, Body, _Optional) ->
    Method = post,
    Path = ["/store/order"],
    QS = [],
    Headers = [],
    Body1 = Body,
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, RespHeaders, ClientRef} ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            {ok, RespHeaders, jsx:decode(ResponseBody, [return_maps])}; 
        {ok, 400, _RespHeaders, _ClientRef} ->
            {error, "Invalid Order"}
    end.


