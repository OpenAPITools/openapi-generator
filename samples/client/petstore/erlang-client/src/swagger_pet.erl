-module(swagger_pet).

-export_type([swagger_pet/0,
              encode/1,
              decode/1]).

-type swagger_pet() ::
    #{ 'id' => integer(),
       'category' => swagger_category:swagger_category(),
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
     }
