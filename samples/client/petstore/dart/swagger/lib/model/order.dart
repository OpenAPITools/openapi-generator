part of swagger.api;

@Entity()
class Order {
  
  @Property(name: 'id')
  int id = null;
  

  @Property(name: 'petId')
  int petId = null;
  

  @Property(name: 'quantity')
  int quantity = null;
  

  @Property(name: 'shipDate')
  DateTime shipDate = null;
  
/* Order Status */
  @Property(name: 'status')
  String status = null;
  //enum statusEnum {  placed,  approved,  delivered,  };

  @Property(name: 'complete')
  bool complete = null;
  
  Order();

  @override
  String toString()  {
    return 'Order[id=$id, petId=$petId, quantity=$quantity, shipDate=$shipDate, status=$status, complete=$complete, ]';
  }
}

