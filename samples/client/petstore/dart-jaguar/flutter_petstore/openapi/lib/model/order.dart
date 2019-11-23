import 'package:jaguar_serializer/jaguar_serializer.dart';


part 'order.jser.dart';

class Order {
  
  @Alias('id', isNullable: false,  )
  final int id;
  
  @Alias('petId', isNullable: false,  )
  final int petId;
  
  @Alias('quantity', isNullable: false,  )
  final int quantity;
  
  @Alias('shipDate', isNullable: false,  )
  final DateTime shipDate;
   /* Order Status */
  @Alias('status', isNullable: false,
          
  )
  final String status;
  //enum statusEnum {  placed,  approved,  delivered,  };
  @Alias('complete', isNullable: false,  )
  final bool complete;
  

  Order(
      

{
     this.id = null,  
     this.petId = null,  
     this.quantity = null,  
     this.shipDate = null,  
     this.status = null,  
     this.complete = false 
    
    }
  );

  @override
  String toString() {
    return 'Order[id=$id, petId=$petId, quantity=$quantity, shipDate=$shipDate, status=$status, complete=$complete, ]';
  }
}

@GenSerializer(nullableFields: true)
class OrderSerializer extends Serializer<Order> with _$OrderSerializer {

}

