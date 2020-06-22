-module(petstore_inline_object).

-include("petstore.hrl").

-export([petstore_inline_object/0]).

-export([petstore_inline_object/1]).

-export_type([petstore_inline_object/0]).

-type petstore_inline_object() ::
  [ {'name', binary() }
  | {'status', binary() }
  ].


petstore_inline_object() ->
    petstore_inline_object([]).

petstore_inline_object(Fields) ->
  Default = [ {'name', binary() }
            , {'status', binary() }
            ],
  lists:ukeymerge(1, lists:sort(Fields), lists:sort(Default)).

