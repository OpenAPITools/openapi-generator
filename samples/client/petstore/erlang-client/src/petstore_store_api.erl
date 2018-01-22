-module(petstore_store_api).

-export([delete_order/2, delete_order/3,
         get_inventory/1, get_inventory/2,
         get_order_by_id/2, get_order_by_id/3,
         place_order/2, place_order/3]).

-define(BASE_URL, "/v2").

%% @doc Delete purchase order by ID
%% For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
-spec delete_order(ctx:ctx(), binary()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
delete_order(Ctx, OrderId) ->
    delete_order(Ctx, OrderId, #{}).

-spec delete_order(ctx:ctx(), binary(), maps:map()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
delete_order(Ctx, OrderId, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = delete,
    Path = ["/store/order/", OrderId, ""],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Returns pet inventories by status
%% Returns a map of status codes to quantities
-spec get_inventory(ctx:ctx()) -> {ok, maps:map(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
get_inventory(Ctx) ->
    get_inventory(Ctx, #{}).

-spec get_inventory(ctx:ctx(), maps:map()) -> {ok, maps:map(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
get_inventory(Ctx, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = get,
    Path = ["/store/inventory"],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Find purchase order by ID
%% For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
-spec get_order_by_id(ctx:ctx(), integer()) -> {ok, petstore_order:petstore_order(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
get_order_by_id(Ctx, OrderId) ->
    get_order_by_id(Ctx, OrderId, #{}).

-spec get_order_by_id(ctx:ctx(), integer(), maps:map()) -> {ok, petstore_order:petstore_order(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
get_order_by_id(Ctx, OrderId, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = get,
    Path = ["/store/order/", OrderId, ""],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Place an order for a pet
-spec place_order(ctx:ctx(), petstore_order:petstore_order()) -> {ok, petstore_order:petstore_order(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
place_order(Ctx, Body) ->
    place_order(Ctx, Body, #{}).

-spec place_order(ctx:ctx(), petstore_order:petstore_order(), maps:map()) -> {ok, petstore_order:petstore_order(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
place_order(Ctx, Body, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = post,
    Path = ["/store/order"],
    QS = [],
    Headers = [],
    Body1 = Body,
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).


