part of swagger.api;

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
  String toString() {
    return 'Order[id=$id, petId=$petId, quantity=$quantity, shipDate=$shipDate, status=$status, complete=$complete, ]';
  }

  Order.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    id =
    json['id'];
    petId =
    json['petId'];
    quantity =
    json['quantity'];
    shipDate = json['shipDate'] == null ? null : DateTime.parse(json['shipDate']);
    status =
    json['status'];
    complete =
    json['complete'];
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'petId': petId,
      'quantity': quantity,
      'shipDate': shipDate == null ? '' : shipDate.toUtc().toIso8601String(),
      'status': status,
      'complete': complete
     };
  }

  static List<Order> listFromJson(List<Map<String, dynamic>> json) {
    var list = new List<Order>();
    if (json != null && json.length > 0) {
      json.forEach((Map<String, dynamic> value) => list.add(new Order.fromJson(value)));
    }
    return list;
  }

  static Map<String, Order> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, Order>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new Order.fromJson(value));
    }
    return map;
  }
}

