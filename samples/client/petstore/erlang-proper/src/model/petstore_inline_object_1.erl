-module(petstore_inline_object_1).

-include("petstore.hrl").

-export([petstore_inline_object_1/0]).

-export([petstore_inline_object_1/1]).

-export_type([petstore_inline_object_1/0]).

-type petstore_inline_object_1() ::
  [ {'additionalMetadata', binary() }
  | {'file', binary() }
  ].


petstore_inline_object_1() ->
    petstore_inline_object_1([]).

petstore_inline_object_1(Fields) ->
  Default = [ {'additionalMetadata', binary() }
            , {'file', binary() }
            ],
  lists:ukeymerge(1, lists:sort(Fields), lists:sort(Default)).

