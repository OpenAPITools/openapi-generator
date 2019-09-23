-module(openapi_category).

-export([encode/1]).

-export_type([openapi_category/0]).

-type openapi_category() ::
    #{ 'id' => integer(),
       'name' => binary()
     }.

encode(#{ 'id' := Id,
          'name' := Name
        }) ->
    #{ 'id' => Id,
       'name' => Name
     }.
