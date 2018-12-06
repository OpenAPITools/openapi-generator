-module(petstore_category).

-include("petstore.hrl").

-export([petstore_category/0]).

-export_type([petstore_category/0]).

-type petstore_category() ::
  [ {'id', integer() }
  | {'name', binary() }
  ].

petstore_category() ->
  [ {'id', integer() }
  , {'name', binary() }
  ].
