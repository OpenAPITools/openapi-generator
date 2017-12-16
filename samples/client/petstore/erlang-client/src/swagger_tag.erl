-module(swagger_tag).

-export([encode/1]).

-export_type([swagger_tag/0]).

-type swagger_tag() ::
    #{ 'id' => integer(),
       'name' => binary()
     }.

encode(#{ 'id' := Id,
          'name' := Name
        }) ->
    #{ 'id' => Id,
       'name' => Name
     }.
