-module(openapi_tag).

-export([encode/1]).

-export_type([openapi_tag/0]).

-type openapi_tag() ::
    #{ 'id' => integer(),
       'name' => binary()
     }.

encode(#{ 'id' := Id,
          'name' := Name
        }) ->
    #{ 'id' => Id,
       'name' => Name
     }.
