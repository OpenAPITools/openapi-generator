-module(petstore_tag).

-include("petstore.hrl").

-export([petstore_tag/0]).

-export([petstore_tag/1]).

-export_type([petstore_tag/0]).

-type petstore_tag() ::
  [ {'id', integer() }
  | {'name', binary() }
  ].


petstore_tag() ->
    petstore_tag([]).

petstore_tag(Fields) ->
  Default = [ {'id', integer() }
            , {'name', binary() }
            ],
  lists:ukeymerge(1, lists:sort(Fields), lists:sort(Default)).

