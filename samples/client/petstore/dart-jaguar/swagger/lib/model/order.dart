import 'package:jaguar_serializer/jaguar_serializer.dart';

part 'order.jser.dart';

class Order {
  
  final int id;
  
  final int petId;
  
  final int quantity;
  
  final DateTime shipDate;
   /* Order Status */
  final String status;
  //enum statusEnum {  placed,  approved,  delivered,  };
  final bool complete;
  

  Order(
    

{
     this.id = null,  
     this.petId = null,  
     this.quantity = null,  
     this.shipDate = null,  
     this.status = null,  
     this.complete = null 
    
    }
  );

  @override
  String toString() {
    return 'Order[id=$id, petId=$petId, quantity=$quantity, shipDate=$shipDate, status=$status, complete=$complete, ]';
  }
}

@GenSerializer()
class OrderSerializer extends Serializer<Order> with _$OrderSerializer {

}
