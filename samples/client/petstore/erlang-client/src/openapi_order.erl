-module(openapi_order).

-export([encode/1]).

-export_type([openapi_order/0]).

-type openapi_order() ::
    #{ 'id' => integer(),
       'petId' => integer(),
       'quantity' => integer(),
       'shipDate' => openapi_date_time:openapi_date_time(),
       'status' => binary(),
       'complete' => boolean()
     }.

encode(#{ 'id' := Id,
          'petId' := PetId,
          'quantity' := Quantity,
          'shipDate' := ShipDate,
          'status' := Status,
          'complete' := Complete
        }) ->
    #{ 'id' => Id,
       'petId' => PetId,
       'quantity' => Quantity,
       'shipDate' => ShipDate,
       'status' => Status,
       'complete' => Complete
     }.
