//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element
// ignore_for_file: always_put_required_named_parameters_first

part of openapi.api;

class Order {
  Order({
    this.id,
    this.petId,
    this.quantity,
    this.shipDate,
    this.status,
    this.complete,
  });

  
  int id;

  
  int petId;

  
  int quantity;

  
  DateTime shipDate;

  /// Order Status
  OrderStatusEnum status;

  
  bool complete = false;

  @override
  String toString() => 'Order[id=$id, petId=$petId, quantity=$quantity, shipDate=$shipDate, status=$status, complete=$complete, ]';

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
    final json = <String, dynamic>{};
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

  static List<Order> listFromJson(List<dynamic> json, {bool growable}) =>
    json == null
      ? <Order>[]
      : json.map((v) => Order.fromJson(v)).toList(growable: true == growable);

  static Map<String, Order> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Order>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = Order.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of Order-objects as value to a dart map
  static Map<String, List<Order>> mapListFromJson(Map<String, dynamic> json, {bool growable}) {
    final map = <String, List<Order>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = Order.listFromJson(v, growable: growable);
      });
    }
    return map;
  }
}

/// Possible values for the [OrderStatusEnum] enum.
abstract class OrderStatusEnumValue {
  /// Disable instantiation.
  const OrderStatusEnumValue._();


  /// Order Status
  static const placed_ = "placed";

  /// Order Status
  static const approved_ = "approved";

  /// Order Status
  static const delivered_ = "delivered";
}

class OrderStatusEnum {
  /// Instantiate a new enum with the provided [value].
  const OrderStatusEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value.toString();

  String toJson() => value;

  /// Order Status
  static const OrderStatusEnum placed_ = OrderStatusEnum._(OrderStatusEnumValue.placed_);

  /// Order Status
  static const OrderStatusEnum approved_ = OrderStatusEnum._(OrderStatusEnumValue.approved_);

  /// Order Status
  static const OrderStatusEnum delivered_ = OrderStatusEnum._(OrderStatusEnumValue.delivered_);

  /// List of all possible values in this [enum][OrderStatusEnum].
  static const values = <OrderStatusEnum>[
    placed_,
    approved_,
    delivered_,
  ];

  static OrderStatusEnum fromJson(String value) =>
    OrderStatusEnumTypeTransformer().decode(value);

  static List<OrderStatusEnum> listFromJson(List<dynamic> json, {bool growable}) =>
    json == null
      ? <OrderStatusEnum>[]
      : json
          .map((value) => OrderStatusEnum.fromJson(value))
          .toList(growable: true == growable);
}

class OrderStatusEnumTypeTransformer {
  const OrderStatusEnumTypeTransformer._();

  factory OrderStatusEnumTypeTransformer() => _instance ??= OrderStatusEnumTypeTransformer._();

  String encode(OrderStatusEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a OrderStatusEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  OrderStatusEnum decode(dynamic data, {bool allowNull}) {
    switch (data) {
      case OrderStatusEnumValue.placed_: return OrderStatusEnum.placed_;
      case OrderStatusEnumValue.approved_: return OrderStatusEnum.approved_;
      case OrderStatusEnumValue.delivered_: return OrderStatusEnum.delivered_;
      default:
        if (false == allowNull) {
          throw UnimplementedError('Unknown enum value to decode: $data');
        }
    }
    return null;
  }

  /// Singleton [OrderStatusEnumTypeTransformer] instance.
  static OrderStatusEnumTypeTransformer _instance;
}


