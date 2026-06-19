-module(petstore_pet).

-export([encode/1]).

-export_type([petstore_pet/0]).

-type petstore_pet() ::
    #{ 'id' => integer(),
       'category' => petstore_category:petstore_category(),
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
