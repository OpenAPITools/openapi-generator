-compile({no_auto_import,[date/0]}).

-import( petstore_gen
       , [ binary/0
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
         ]
       ).

-type date()     :: calendar:date().
-type datetime() :: calendar:datetime().
