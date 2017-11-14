-module(swagger_tag).

-export_type([swagger_tag/0,
              encode/1,
              decode/1]).

-type swagger_tag() ::
    #{ 'id' => integer(),
       'name' => binary()
     }.

encode(#{ 'id' := Id,
          'name' := Name
        }) ->
    #{ 'id' => Id,
       'name' => Name
     }
