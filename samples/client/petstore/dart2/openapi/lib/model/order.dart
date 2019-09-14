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
    id = json['id'];
    petId = json['petId'];
    quantity = json['quantity'];
    shipDate = (json['shipDate'] == null) ?
      null :
      DateTime.parse(json['shipDate']);
    status = json['status'];
    complete = json['complete'];
  }

  Map<String, dynamic> toJson() {
    Map <String, dynamic> json = {};
    if (id != null)
      json['id'] = id;
    if (petId != null)
      json['petId'] = petId;
    if (quantity != null)
      json['quantity'] = quantity;
    if (shipDate != null)
      json['shipDate'] = shipDate == null ? null : shipDate.toUtc().toIso8601String();
    if (status != null)
      json['status'] = status;
    if (complete != null)
      json['complete'] = complete;
    return json;
  }

  static List<Order> listFromJson(List<dynamic> json) {
    return json == null ? List<Order>() : json.map((value) => Order.fromJson(value)).toList();
  }

  static Map<String, Order> mapFromJson(Map<String, dynamic> json) {
    var map = Map<String, Order>();
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic value) => map[key] = Order.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Order-objects as value to a dart map
  static Map<String, List<Order>> mapListFromJson(Map<String, dynamic> json) {
    var map = Map<String, List<Order>>();
     if (json != null && json.isNotEmpty) {
       json.forEach((String key, dynamic value) {
         map[key] = Order.listFromJson(value);
       });
     }
     return map;
  }
}

