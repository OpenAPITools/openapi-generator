-module(prop_petstore).

-export([prop_test/0]).

prop_test() ->
  {ok, _} = application:ensure_all_started(petstore),
  petstore_statem:prop_main().
