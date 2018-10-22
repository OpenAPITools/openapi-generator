-module(petstore_order).

-include("petstore.hrl").

-export([petstore_order/0]).

-export([petstore_order/1]).

-export_type([petstore_order/0]).

-type petstore_order() ::
  [ {'id', integer() }
  | {'petId', integer() }
  | {'quantity', integer() }
  | {'shipDate', datetime() }
  | {'status', binary() }
  | {'complete', boolean() }
  ].


petstore_order() ->
    petstore_order([]).

petstore_order(Fields) ->
  Default = [ {'id', integer() }
            , {'petId', integer() }
            , {'quantity', integer() }
            , {'shipDate', datetime() }
            , {'status', elements([<<"placed">>, <<"approved">>, <<"delivered">>]) }
            , {'complete', boolean() }
            ],
  lists:ukeymerge(1, lists:sort(Fields), lists:sort(Default)).

