-module(petstore_api_response).

-include("petstore.hrl").

-export([petstore_api_response/0]).

-export_type([petstore_api_response/0]).

-type petstore_api_response() ::
  [ {'code', integer() }
  | {'type', binary() }
  | {'message', binary() }
  ].

petstore_api_response() ->
  [ {'code', integer() }
  , {'type', binary() }
  , {'message', binary() }
  ].
