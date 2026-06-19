-module(petstore_api_response).

-include("petstore.hrl").

-export([petstore_api_response/0]).

-export([petstore_api_response/1]).

-export_type([petstore_api_response/0]).

-type petstore_api_response() ::
  [ {'code', integer() }
  | {'type', binary() }
  | {'message', binary() }
  ].


petstore_api_response() ->
    petstore_api_response([]).

petstore_api_response(Fields) ->
  Default = [ {'code', integer() }
            , {'type', binary() }
            , {'message', binary() }
            ],
  lists:ukeymerge(1, lists:sort(Fields), lists:sort(Default)).

