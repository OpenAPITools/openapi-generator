-module(swagger_pet_api).

-export([add_pet/1, add_pet/2,
         delete_pet/1, delete_pet/2,
         find_pets_by_status/1, find_pets_by_status/2,
         find_pets_by_tags/1, find_pets_by_tags/2,
         get_pet_by_id/1, get_pet_by_id/2,
         update_pet/1, update_pet/2,
         update_pet_with_form/1, update_pet_with_form/2,
         upload_file/1, upload_file/2]).

-define(BASE_URL, <<"http://petstore.swagger.io/v2">>).

%% @doc Add a new pet to the store
-spec add_pet(swagger_pet:swagger_pet(), term()) -> ok | {error, integer()}.
add_pet(Body) ->
    add_pet(Body, Body, #{}).

-spec add_pet(swagger_pet:swagger_pet(), term(), maps:map()) -> ok | {error, integer()}.
add_pet(Body, Body, _Optional) ->
    Method = post,
    Path = ["/pet"],
    QS = [],
    Headers = [],
    Body1 = Body,
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, _RespHeaders, _ClientRef} ->
             ok;
        {ok, Status, _RespHeaders, _ClientRef} ->
             {error, Status}
    end.

%% @doc Deletes a pet
-spec delete_pet(integer()) -> ok | {error, integer()}.
delete_pet(PetId) ->
    delete_pet(PetId, , #{}).

-spec delete_pet(integer(), maps:map()) -> ok | {error, integer()}.
delete_pet(PetId, _Optional) ->
    Method = delete,
    Path = ["/pet/", PetId, ""],
    QS = [],
    Headers = []++[{X, maps:get(X, _Optional)} || X <- ['api_key'], maps:is_key(X, _Optional)],
    Body1 = [],
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, _RespHeaders, _ClientRef} ->
             ok;
        {ok, Status, _RespHeaders, _ClientRef} ->
             {error, Status}
    end.

%% @doc Finds Pets by status
%% Multiple status values can be provided with comma separated strings
-spec find_pets_by_status(list()) -> {ok, list(), [swagger_pet:swagger_pet()]} | {error, string()}.
find_pets_by_status(Status) ->
    find_pets_by_status(Status, , #{}).

-spec find_pets_by_status(list(), maps:map()) -> {ok, list(), [swagger_pet:swagger_pet()]} | {error, string()}.
find_pets_by_status(Status, _Optional) ->
    Method = get,
    Path = ["/pet/findByStatus"],
    QS = lists:flatten([[{<<"status">>, X} || X <- Status]])++[{X, maps:get(X, _Optional)} || X <- [], maps:is_key(X, _Optional)],
    Headers = [],
    Body1 = [],
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, RespHeaders, ClientRef} ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            {ok, RespHeaders, jsx:decode(ResponseBody, [return_maps])}; 
        {ok, 400, _RespHeaders, _ClientRef} ->
            {error, "Invalid status value"}
    end.

%% @doc Finds Pets by tags
%% Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
-spec find_pets_by_tags(list()) -> {ok, list(), [swagger_pet:swagger_pet()]} | {error, string()}.
find_pets_by_tags(Tags) ->
    find_pets_by_tags(Tags, , #{}).

-spec find_pets_by_tags(list(), maps:map()) -> {ok, list(), [swagger_pet:swagger_pet()]} | {error, string()}.
find_pets_by_tags(Tags, _Optional) ->
    Method = get,
    Path = ["/pet/findByTags"],
    QS = lists:flatten([[{<<"tags">>, X} || X <- Tags]])++[{X, maps:get(X, _Optional)} || X <- [], maps:is_key(X, _Optional)],
    Headers = [],
    Body1 = [],
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, RespHeaders, ClientRef} ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            {ok, RespHeaders, jsx:decode(ResponseBody, [return_maps])}; 
        {ok, 400, _RespHeaders, _ClientRef} ->
            {error, "Invalid tag value"}
    end.

%% @doc Find pet by ID
%% Returns a single pet
-spec get_pet_by_id(integer()) -> {ok, list(), swagger_pet:swagger_pet()} | {error, string()}.
get_pet_by_id(PetId) ->
    get_pet_by_id(PetId, , #{}).

-spec get_pet_by_id(integer(), maps:map()) -> {ok, list(), swagger_pet:swagger_pet()} | {error, string()}.
get_pet_by_id(PetId, _Optional) ->
    Method = get,
    Path = ["/pet/", PetId, ""],
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
            {error, "Pet not found"}
    end.

%% @doc Update an existing pet
-spec update_pet(swagger_pet:swagger_pet(), term()) -> ok | {error, integer()}.
update_pet(Body) ->
    update_pet(Body, Body, #{}).

-spec update_pet(swagger_pet:swagger_pet(), term(), maps:map()) -> ok | {error, integer()}.
update_pet(Body, Body, _Optional) ->
    Method = put,
    Path = ["/pet"],
    QS = [],
    Headers = [],
    Body1 = Body,
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, _RespHeaders, _ClientRef} ->
             ok;
        {ok, Status, _RespHeaders, _ClientRef} ->
             {error, Status}
    end.

%% @doc Updates a pet in the store with form data
-spec update_pet_with_form(integer()) -> ok | {error, integer()}.
update_pet_with_form(PetId) ->
    update_pet_with_form(PetId, , #{}).

-spec update_pet_with_form(integer(), maps:map()) -> ok | {error, integer()}.
update_pet_with_form(PetId, _Optional) ->
    Method = post,
    Path = ["/pet/", PetId, ""],
    QS = [],
    Headers = [],
    Body1 = {form, []++[{X, maps:get(X, _Optional)} || X <- ['name', 'status'], maps:is_key(X, _Optional)]},
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, _RespHeaders, _ClientRef} ->
             ok;
        {ok, Status, _RespHeaders, _ClientRef} ->
             {error, Status}
    end.

%% @doc uploads an image
-spec upload_file(integer()) -> {ok, list(), swagger_api_response:swagger_api_response()} | {error, string()}.
upload_file(PetId) ->
    upload_file(PetId, , #{}).

-spec upload_file(integer(), maps:map()) -> {ok, list(), swagger_api_response:swagger_api_response()} | {error, string()}.
upload_file(PetId, _Optional) ->
    Method = post,
    Path = ["/pet/", PetId, "/uploadImage"],
    QS = [],
    Headers = [],
    Body1 = {form, []++[{X, maps:get(X, _Optional)} || X <- ['additionalMetadata', 'file'], maps:is_key(X, _Optional)]},
    Opts = [],
    Url = hackney_url:make_url(?BASE_URL, Path, QS),

    case hackney:request(Method, Url, Headers, Body1, Opts) of
        {ok, 200, RespHeaders, ClientRef} ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            {ok, RespHeaders, jsx:decode(ResponseBody, [return_maps])}
    end.


