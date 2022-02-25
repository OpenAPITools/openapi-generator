-module(petstore_inline_object).

-export([encode/1]).

-export_type([petstore_inline_object/0]).

-type petstore_inline_object() ::
    #{ 'name' => binary(),
       'status' => binary()
     }.

encode(#{ 'name' := Name,
          'status' := Status
        }) ->
    #{ 'name' => Name,
       'status' => Status
     }.
