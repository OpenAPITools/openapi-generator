-module(petstore_pet_api).

-export([add_pet/2, add_pet/3,
         delete_pet/2, delete_pet/3,
         find_pets_by_status/2, find_pets_by_status/3,
         find_pets_by_tags/2, find_pets_by_tags/3,
         get_pet_by_id/2, get_pet_by_id/3,
         update_pet/2, update_pet/3,
         update_pet_with_form/2, update_pet_with_form/3,
         upload_file/2, upload_file/3]).

-define(BASE_URL, <<"/v2">>).

%% @doc Add a new pet to the store
%% 
-spec add_pet(ctx:ctx(), petstore_pet:petstore_pet()) -> {ok, petstore_pet:petstore_pet(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
add_pet(Ctx, PetstorePet) ->
    add_pet(Ctx, PetstorePet, #{}).

-spec add_pet(ctx:ctx(), petstore_pet:petstore_pet(), maps:map()) -> {ok, petstore_pet:petstore_pet(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
add_pet(Ctx, PetstorePet, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = post,
    Path = [<<"/pet">>],
    QS = [],
    Headers = [],
    Body1 = PetstorePet,
    ContentTypeHeader = petstore_utils:select_header_content_type([<<"application/json">>, <<"application/xml">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Deletes a pet
%% 
-spec delete_pet(ctx:ctx(), integer()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
delete_pet(Ctx, PetId) ->
    delete_pet(Ctx, PetId, #{}).

-spec delete_pet(ctx:ctx(), integer(), maps:map()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
delete_pet(Ctx, PetId, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = delete,
    Path = [<<"/pet/", PetId, "">>],
    QS = [],
    Headers = []++petstore_utils:optional_params(['api_key'], _OptionalParams),
    Body1 = [],
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Finds Pets by status
%% Multiple status values can be provided with comma separated strings
-spec find_pets_by_status(ctx:ctx(), list()) -> {ok, [petstore_pet:petstore_pet()], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
find_pets_by_status(Ctx, Status) ->
    find_pets_by_status(Ctx, Status, #{}).

-spec find_pets_by_status(ctx:ctx(), list(), maps:map()) -> {ok, [petstore_pet:petstore_pet()], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
find_pets_by_status(Ctx, Status, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = get,
    Path = [<<"/pet/findByStatus">>],
    QS = lists:flatten([[{<<"status">>, X} || X <- Status]])++petstore_utils:optional_params([], _OptionalParams),
    Headers = [],
    Body1 = [],
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Finds Pets by tags
%% Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
-spec find_pets_by_tags(ctx:ctx(), list()) -> {ok, [petstore_pet:petstore_pet()], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
find_pets_by_tags(Ctx, Tags) ->
    find_pets_by_tags(Ctx, Tags, #{}).

-spec find_pets_by_tags(ctx:ctx(), list(), maps:map()) -> {ok, [petstore_pet:petstore_pet()], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
find_pets_by_tags(Ctx, Tags, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = get,
    Path = [<<"/pet/findByTags">>],
    QS = lists:flatten([[{<<"tags">>, X} || X <- Tags]])++petstore_utils:optional_params([], _OptionalParams),
    Headers = [],
    Body1 = [],
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Find pet by ID
%% Returns a single pet
-spec get_pet_by_id(ctx:ctx(), integer()) -> {ok, petstore_pet:petstore_pet(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
get_pet_by_id(Ctx, PetId) ->
    get_pet_by_id(Ctx, PetId, #{}).

-spec get_pet_by_id(ctx:ctx(), integer(), maps:map()) -> {ok, petstore_pet:petstore_pet(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
get_pet_by_id(Ctx, PetId, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = get,
    Path = [<<"/pet/", PetId, "">>],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Update an existing pet
%% 
-spec update_pet(ctx:ctx(), petstore_pet:petstore_pet()) -> {ok, petstore_pet:petstore_pet(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
update_pet(Ctx, PetstorePet) ->
    update_pet(Ctx, PetstorePet, #{}).

-spec update_pet(ctx:ctx(), petstore_pet:petstore_pet(), maps:map()) -> {ok, petstore_pet:petstore_pet(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
update_pet(Ctx, PetstorePet, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = put,
    Path = [<<"/pet">>],
    QS = [],
    Headers = [],
    Body1 = PetstorePet,
    ContentTypeHeader = petstore_utils:select_header_content_type([<<"application/json">>, <<"application/xml">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Updates a pet in the store with form data
%% 
-spec update_pet_with_form(ctx:ctx(), integer()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
update_pet_with_form(Ctx, PetId) ->
    update_pet_with_form(Ctx, PetId, #{}).

-spec update_pet_with_form(ctx:ctx(), integer(), maps:map()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
update_pet_with_form(Ctx, PetId, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = post,
    Path = [<<"/pet/", PetId, "">>],
    QS = [],
    Headers = [],
    Body1 = {form, []++petstore_utils:optional_params(['name', 'status'], _OptionalParams)},
    ContentTypeHeader = petstore_utils:select_header_content_type([<<"application/x-www-form-urlencoded">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc uploads an image
%% 
-spec upload_file(ctx:ctx(), integer()) -> {ok, petstore_api_response:petstore_api_response(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
upload_file(Ctx, PetId) ->
    upload_file(Ctx, PetId, #{}).

-spec upload_file(ctx:ctx(), integer(), maps:map()) -> {ok, petstore_api_response:petstore_api_response(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
upload_file(Ctx, PetId, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = post,
    Path = [<<"/pet/", PetId, "/uploadImage">>],
    QS = [],
    Headers = [],
    Body1 = {form, []++petstore_utils:optional_params(['additionalMetadata', 'file'], _OptionalParams)},
    ContentTypeHeader = petstore_utils:select_header_content_type([<<"multipart/form-data">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).


