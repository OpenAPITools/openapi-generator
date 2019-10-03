-module(petstore_order).

-export([encode/1]).

-export_type([petstore_order/0]).

-type petstore_order() ::
    #{ 'id' => integer(),
       'petId' => integer(),
       'quantity' => integer(),
       'shipDate' => petstore_date_time:petstore_date_time(),
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
