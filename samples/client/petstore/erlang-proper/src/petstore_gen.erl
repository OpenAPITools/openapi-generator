-module(petstore_gen).

-compile({no_auto_import,[date/0]}).

-include_lib("proper/include/proper_common.hrl").

%%==============================================================================
%% Exports
%%==============================================================================

-export([ binary/0
        , binary/1
        , binary/2
        , integer/0
        , integer/1
        , integer/2
        , boolean/0
        , list/0
        , list/1
        , list/2
        , list/3
        , map/0
        , date/0
        , datetime/0
        , any/0
        , elements/1
        ]).

-define(CHARS, [$a, $b, $c]).

%%==============================================================================
%% Generators
%%==============================================================================

binary() -> binary(10).

binary(Min, Max) ->
  ?LET( {X, N}
      , { proper_types:elements(?CHARS)
        , proper_types:choose(Min, Max)
        }
      , iolist_to_binary(lists:duplicate(N, X))
      ).

binary(N) ->
  ?LET( X
      , proper_types:elements(?CHARS)
      , iolist_to_binary(lists:duplicate(N, X))
      ).

integer() -> proper_types:int().

integer(0) -> proper_types:nat();
integer(Min) ->
  ?LET( N
      , proper_types:nat()
      , proper_types:choose(Min, Min + N)
      ).

integer(Min, Max) -> proper_types:choose(Min, Max).

boolean() -> proper_types:bool().

list() -> list(any()).

list(Type) -> proper_types:list(Type).

list(Type, Min) ->
  ?LET( N
      , integer(0)
      , ?LET(X, list(Type, Min, Min + N), X)
      ).

list(Type, Min, Max) when Min =< Max ->
  ?LET( {X, Y}
      , { proper_types:vector(Min, Type)
        , proper_types:resize(Max - Min, proper_types:list(Type))
        }
      , X ++ Y
      ).

map() -> proper_types:map(any(), any()).

date() ->
  ?LET( X
      , ?SUCHTHAT( X
                 , { year()
                   , proper_types:choose(1, 12)
                   , proper_types:choose(1, 31)
                   }
                 , calendar:valid_date(X)
                 )
      , begin
          {Year, Month, Day} = X,
          YearBin  = num_binary_format(Year, "4"),
          MonthBin = num_binary_format(Month, "2"),
          DayBin   = num_binary_format(Day, "2"),
          <<YearBin/binary, "-", MonthBin/binary, "-", DayBin/binary>>
        end
      ).

datetime() ->
  Date = date(),
  Hour = hour(),
  ?LET( X
      , {Date, Hour}
      , begin
          {D, H} = X,
          <<D/binary, "T", H/binary, "+0000">>
        end
      ).

any() ->
  Any = [ binary()
        , integer()
        , boolean()
        %% We don't include lists and maps to avoid huge values
        %% , list()
        %% , map()
        , date()
        , datetime()
        ],
  proper_types:oneof(Any).

elements(Items) ->
  proper_types:elements(Items).

%%==============================================================================
%% Internal
%%==============================================================================

year() ->
  ?LET( X
      , proper_types:nat()
      , 1970 + X
      ).

hour() ->
  ?LET( X
      , { proper_types:choose(0, 23)
        , proper_types:choose(0, 59)
        , proper_types:choose(0, 59)
        , proper_types:choose(0, 999)
        }
      , begin
          {Hours, Mins, Secs, Millis} = X,
          HoursBin  = num_binary_format(Hours, "2"),
          MinsBin   = num_binary_format(Mins, "2"),
          SecsBin   = num_binary_format(Secs, "2"),
          MillisBin = num_binary_format(Millis, "3"),
          <<HoursBin/binary, ":", MinsBin/binary, ":",
            SecsBin/binary, ".", MillisBin/binary>>
        end
      ).

num_binary_format(X, N) ->
  list_to_binary(io_lib:format("~" ++ N ++ "..0B", [X])).
