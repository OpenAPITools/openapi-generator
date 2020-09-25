part of openapi.api;

class Order {
  
  int id;
  
  int petId;
  
  int quantity;
  
  DateTime shipDate;
  /// Order Status
  OrderStatusEnum status;
  
  bool complete = false;

  Order({
    this.id,
    this.petId,
    this.quantity,
    this.shipDate,
    this.status,
    this.complete = false,
  });

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
    status = OrderStatusEnum.fromJson(json['status']);
    complete = json['complete'];
  }

  Map<String, dynamic> toJson() {
    Map<String, dynamic> json = {};
    if (id != null)
      json['id'] = id;
    if (petId != null)
      json['petId'] = petId;
    if (quantity != null)
      json['quantity'] = quantity;
    if (shipDate != null)
      json['shipDate'] = shipDate == null ? null : shipDate.toUtc().toIso8601String();
    if (status != null)
      json['status'] = status.value;
    if (complete != null)
      json['complete'] = complete;
    return json;
  }

  static List<Order> listFromJson(List<dynamic> json) {
    return json == null ? List<Order>() : json.map((value) => Order.fromJson(value)).toList();
  }

  static Map<String, Order> mapFromJson(Map<String, dynamic> json) {
    final map = Map<String, Order>();
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic value) => map[key] = Order.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Order-objects as value to a dart map
  static Map<String, List<Order>> mapListFromJson(Map<String, dynamic> json) {
    final map = Map<String, List<Order>>();
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic value) {
        map[key] = Order.listFromJson(value);
      });
    }
    return map;
  }
}
class OrderStatusEnum {
  /// The underlying value of this enum member.
  final String value;

  const OrderStatusEnum._internal(this.value);

  /// Order Status
  static const OrderStatusEnum placed_ = OrderStatusEnum._internal("placed");
  /// Order Status
  static const OrderStatusEnum approved_ = OrderStatusEnum._internal("approved");
  /// Order Status
  static const OrderStatusEnum delivered_ = OrderStatusEnum._internal("delivered");

  static List<OrderStatusEnum> get values => const [
        placed_,
        approved_,
        delivered_,
      ];

  String toJson () {
    return value;
  }

  @override
  String toString () {
    return value;
  }

  static OrderStatusEnum fromJson(String value) {
    return OrderStatusEnumTypeTransformer().decode(value);
  }

  static List<OrderStatusEnum> listFromJson(List<dynamic> json) {
    return json == null
      ? List<OrderStatusEnum>()
      : json.map((value) => OrderStatusEnum.fromJson(value)).toList();
  }
}

class OrderStatusEnumTypeTransformer {

  dynamic encode(OrderStatusEnum data) {
    return data.value;
  }

  OrderStatusEnum decode(dynamic data) {
    switch (data) {
      case "placed": return OrderStatusEnum.placed_;
      case "approved": return OrderStatusEnum.approved_;
      case "delivered": return OrderStatusEnum.delivered_;
      default: return null;
    }
  }
}


