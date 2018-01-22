-module(petstore_tag).

-export([encode/1]).

-export_type([petstore_tag/0]).

-type petstore_tag() ::
    #{ 'id' => integer(),
       'name' => binary()
     }.

encode(#{ 'id' := Id,
          'name' := Name
        }) ->
    #{ 'id' => Id,
       'name' => Name
     }.
