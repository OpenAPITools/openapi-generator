-module(petstore_category).

-include("petstore.hrl").

-export([petstore_category/0]).

-export([petstore_category/1]).

-export_type([petstore_category/0]).

-type petstore_category() ::
  [ {'id', integer() }
  | {'name', binary() }
  ].


petstore_category() ->
    petstore_category([]).

petstore_category(Fields) ->
  Default = [ {'id', integer() }
            , {'name', binary() }
            ],
  lists:ukeymerge(1, lists:sort(Fields), lists:sort(Default)).

