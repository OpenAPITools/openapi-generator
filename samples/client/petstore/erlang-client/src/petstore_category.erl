-module(petstore_category).

-export([encode/1]).

-export_type([petstore_category/0]).

-type petstore_category() ::
    #{ 'id' => integer(),
       'name' => binary()
     }.

encode(#{ 'id' := Id,
          'name' := Name
        }) ->
    #{ 'id' => Id,
       'name' => Name
     }.
