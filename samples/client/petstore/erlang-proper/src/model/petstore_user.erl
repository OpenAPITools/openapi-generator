-module(petstore_user).

-include("petstore.hrl").

-export([petstore_user/0]).

-export([petstore_user/1]).

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
    petstore_user([]).

petstore_user(Fields) ->
  Default = [ {'id', integer() }
            , {'username', binary() }
            , {'firstName', binary() }
            , {'lastName', binary() }
            , {'email', binary() }
            , {'password', binary() }
            , {'phone', binary() }
            , {'userStatus', integer() }
            ],
  lists:ukeymerge(1, lists:sort(Fields), lists:sort(Default)).

