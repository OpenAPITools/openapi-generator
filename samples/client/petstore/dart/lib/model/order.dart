part of api;


@Entity()
class Order {
  
  int id = null;
  
  
  int petId = null;
  
  
  int quantity = null;
  
  
  DateTime shipDate = null;
  
  /* Order Status */
  String status = null;
  //enum statusEnum {  placed,  approved,  delivered,  };
  
  bool complete = null;
  
  
  Order();

  @override
  String toString()  {
    return 'Order[id=$id, petId=$petId, quantity=$quantity, shipDate=$shipDate, status=$status, complete=$complete, ]';
  }

}

