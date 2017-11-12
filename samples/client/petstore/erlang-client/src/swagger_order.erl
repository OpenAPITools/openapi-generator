-module(swagger_order).

-export_type([swagger_order/0,
              encode/1,
              decode/1]).

-type swagger_order() ::
    #{ 'id' => integer(),
       'petId' => integer(),
       'quantity' => integer(),
       'shipDate' => swagger_date_time:swagger_date_time(),
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
     }
