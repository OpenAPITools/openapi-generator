-module(swagger_category).

-export([encode/1]).

-export_type([swagger_category/0]).

-type swagger_category() ::
    #{ 'id' => integer(),
       'name' => binary()
     }.

encode(#{ 'id' := Id,
          'name' := Name
        }) ->
    #{ 'id' => Id,
       'name' => Name
     }.
