-module(openapi_pet).

-export([encode/1]).

-export_type([openapi_pet/0]).

-type openapi_pet() ::
    #{ 'id' => integer(),
       'category' => openapi_category:openapi_category(),
       'name' := binary(),
       'photoUrls' := list(),
       'tags' => list(),
       'status' => binary()
     }.

encode(#{ 'id' := Id,
          'category' := Category,
          'name' := Name,
          'photoUrls' := PhotoUrls,
          'tags' := Tags,
          'status' := Status
        }) ->
    #{ 'id' => Id,
       'category' => Category,
       'name' => Name,
       'photoUrls' => PhotoUrls,
       'tags' => Tags,
       'status' => Status
     }.
