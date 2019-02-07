-module(petstore_pet).

-include("petstore.hrl").

-export([petstore_pet/0]).

-export([petstore_pet/1]).

-export_type([petstore_pet/0]).

-type petstore_pet() ::
  [ {'id', integer() }
  | {'category', petstore_category:petstore_category() }
  | {'name', binary() }
  | {'photoUrls', list(binary()) }
  | {'tags', list(petstore_tag:petstore_tag()) }
  | {'status', binary() }
  ].


petstore_pet() ->
    petstore_pet([]).

petstore_pet(Fields) ->
  Default = [ {'id', integer() }
            , {'category', petstore_category:petstore_category() }
            , {'name', binary() }
            , {'photoUrls', list(binary()) }
            , {'tags', list(petstore_tag:petstore_tag()) }
            , {'status', elements([<<"available">>, <<"pending">>, <<"sold">>]) }
            ],
  lists:ukeymerge(1, lists:sort(Fields), lists:sort(Default)).

