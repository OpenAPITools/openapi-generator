-module(petstore_tag).

-include("petstore.hrl").

-export([petstore_tag/0]).

-export_type([petstore_tag/0]).

-type petstore_tag() ::
  [ {'id', integer() }
  | {'name', binary() }
  ].

petstore_tag() ->
  [ {'id', integer() }
  , {'name', binary() }
  ].
