-module(swagger_category).

-export_type([swagger_category/0,
              encode/1,
              decode/1]).

-type swagger_category() ::
    #{ 'id' => integer(),
       'name' => binary()
     }.

encode(#{ 'id' := Id,
          'name' := Name
        }) ->
    #{ 'id' => Id,
       'name' => Name
     }
