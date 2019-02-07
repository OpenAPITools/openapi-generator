part of openapi.api;

class Order {
  
  int id = null;
  
  int petId = null;
  
  int quantity = null;
  
  DateTime shipDate = null;
  /* Order Status */
  String status = null;
  //enum statusEnum {  placed,  approved,  delivered,  };{
  
  bool complete = false;
  Order();

  @override
  String toString() {
    return 'Order[id=$id, petId=$petId, quantity=$quantity, shipDate=$shipDate, status=$status, complete=$complete, ]';
  }

  Order.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    if (json['id'] == null) {
      id = null;
    } else {
          id = json['id'];
    }
    if (json['petId'] == null) {
      petId = null;
    } else {
          petId = json['petId'];
    }
    if (json['quantity'] == null) {
      quantity = null;
    } else {
          quantity = json['quantity'];
    }
    if (json['shipDate'] == null) {
      shipDate = null;
    } else {
      shipDate = DateTime.parse(json['shipDate']);
    }
    if (json['status'] == null) {
      status = null;
    } else {
          status = json['status'];
    }
    if (json['complete'] == null) {
      complete = null;
    } else {
          complete = json['complete'];
    }
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

  static List<Order> listFromJson(List<dynamic> json) {
    return json == null ? new List<Order>() : json.map((value) => new Order.fromJson(value)).toList();
  }

  static Map<String, Order> mapFromJson(Map<String, dynamic> json) {
    var map = new Map<String, Order>();
    if (json != null && json.length > 0) {
      json.forEach((String key, dynamic value) => map[key] = new Order.fromJson(value));
    }
    return map;
  }
}

