-module(swagger_api_response).

-export_type([swagger_api_response/0,
              encode/1,
              decode/1]).

-type swagger_api_response() ::
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
     }
