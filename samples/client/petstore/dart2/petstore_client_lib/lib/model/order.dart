//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Order {
  /// Returns a new [Order] instance.
  Order({
    this.id,
    this.petId,
    this.quantity,
    this.shipDate,
    this.status,
    this.complete = false,
  });

  /// Returns a new [Order] instance and optionally import its values from
  /// [json] if it's non-null.
  Order.fromJson(Map<String, dynamic> json) {
    if (json != null) {
      id = json['id'];
      petId = json['petId'];
      quantity = json['quantity'];
      shipDate = json['shipDate'] == null
        ? null
        : DateTime.parse(json['shipDate']);
      status = OrderStatusEnum.fromJson(json['status']);
      complete = json['complete'];
    }
  }

  
  int id;

  
  int petId;

  
  int quantity;

  
  DateTime shipDate;

  /// Order Status
  OrderStatusEnum status;

  
  bool complete;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Order &&
     other.id == id &&
     other.petId == petId &&
     other.quantity == quantity &&
     other.shipDate == shipDate &&
     other.status == status &&
     other.complete == complete;

  @override
  int get hashCode =>
    id.hashCode +
    petId.hashCode +
    quantity.hashCode +
    shipDate.hashCode +
    status.hashCode +
    complete.hashCode;

  @override
  String toString() => 'Order[id=$id, petId=$petId, quantity=$quantity, shipDate=$shipDate, status=$status, complete=$complete]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (id != null) {
      json['id'] = id;
    }
    if (petId != null) {
      json['petId'] = petId;
    }
    if (quantity != null) {
      json['quantity'] = quantity;
    }
    if (shipDate != null) {
      json['shipDate'] = shipDate.toUtc().toIso8601String();
    }
    if (status != null) {
      json['status'] = status;
    }
    if (complete != null) {
      json['complete'] = complete;
    }
    return json;
  }

  static List<Order> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <Order>[]
      : json.map((v) => Order.fromJson(v)).toList(growable: true == growable);

  static Map<String, Order> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Order>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = Order.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of Order-objects as value to a dart map
  static Map<String, List<Order>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Order>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = Order.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

/// Order Status
class OrderStatusEnum {
  /// Instantiate a new enum with the provided [value].
  const OrderStatusEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  bool operator ==(Object other) => identical(this, other) ||
      other is OrderStatusEnum && other.value == value ||
      other is String && other == value;

  @override
  int get hashCode => toString().hashCode;

  @override
  String toString() => value;

  String toJson() => value;

  static const placed_ = OrderStatusEnum._('placed');
  static const approved_ = OrderStatusEnum._('approved');
  static const delivered_ = OrderStatusEnum._('delivered');

  /// List of all possible values in this [enum][OrderStatusEnum].
  static const values = <OrderStatusEnum>[
    placed_,
    approved_,
    delivered_,
  ];

  static OrderStatusEnum fromJson(String value) =>
    OrderStatusEnumTypeTransformer().decode(value);

  static List<OrderStatusEnum> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <OrderStatusEnum>[]
      : json
          .map((value) => OrderStatusEnum.fromJson(value))
          .toList(growable: true == growable);
}

/// Transformation class that can [encode] an instance of [OrderStatusEnum] to String,
/// and [decode] dynamic data back to [OrderStatusEnum].
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
      case 'placed': return OrderStatusEnum.placed_;
      case 'approved': return OrderStatusEnum.approved_;
      case 'delivered': return OrderStatusEnum.delivered_;
      default:
        if (allowNull == false) {
          throw ArgumentError('Unknown enum value to decode: $data');
        }
    }
    return null;
  }

  /// Singleton [OrderStatusEnumTypeTransformer] instance.
  static OrderStatusEnumTypeTransformer _instance;
}

