-module(petstore_user).

-include("petstore.hrl").

-export([petstore_user/0]).

-export_type([petstore_user/0]).

-type petstore_user() ::
  [ {'id', integer() }
  | {'username', binary() }
  | {'firstName', binary() }
  | {'lastName', binary() }
  | {'email', binary() }
  | {'password', binary() }
  | {'phone', binary() }
  | {'userStatus', integer() }
  ].

petstore_user() ->
  [ {'id', integer() }
  , {'username', binary() }
  , {'firstName', binary() }
  , {'lastName', binary() }
  , {'email', binary() }
  , {'password', binary() }
  , {'phone', binary() }
  , {'userStatus', integer() }
  ].
