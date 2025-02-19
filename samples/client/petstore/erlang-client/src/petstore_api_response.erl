-module(petstore_api_response).

-export([encode/1]).

-export_type([petstore_api_response/0]).

-type petstore_api_response() ::
    #{ 'code' => integer(),
       'type' => binary(),
       'message' => binary()
     }.

encode(#{ 'code' := Code,
          'type' := Type,
          'message' := Message
        }) ->
    #{ 'code' => Code,
       'type' => Type,
       'message' => Message
     }.
