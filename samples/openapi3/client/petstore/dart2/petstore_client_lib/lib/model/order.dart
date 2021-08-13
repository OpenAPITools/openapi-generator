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
    (id == null ? 0 : id.hashCode) +
    (petId == null ? 0 : petId.hashCode) +
    (quantity == null ? 0 : quantity.hashCode) +
    (shipDate == null ? 0 : shipDate.hashCode) +
    (status == null ? 0 : status.hashCode) +
    (complete == null ? 0 : complete.hashCode);

  @override
  String toString() => 'Order[id=$id, petId=$petId, quantity=$quantity, shipDate=$shipDate, status=$status, complete=$complete]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (id != null) {
      json[r'id'] = id;
    }
    if (petId != null) {
      json[r'petId'] = petId;
    }
    if (quantity != null) {
      json[r'quantity'] = quantity;
    }
    if (shipDate != null) {
      json[r'shipDate'] = shipDate.toUtc().toIso8601String();
    }
    if (status != null) {
      json[r'status'] = status;
    }
    if (complete != null) {
      json[r'complete'] = complete;
    }
    return json;
  }

  /// Returns a new [Order] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static Order fromJson(Map<String, dynamic> json) => json == null
    ? null
    : Order(
        id: json[r'id'],
        petId: json[r'petId'],
        quantity: json[r'quantity'],
        shipDate: json[r'shipDate'] == null
          ? null
          : DateTime.parse(json[r'shipDate']),
        status: OrderStatusEnum.fromJson(json[r'status']),
        complete: json[r'complete'],
    );

  static List<Order> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <Order>[]
      : json.map((dynamic value) => Order.fromJson(value)).toList(growable: true == growable);

  static Map<String, Order> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Order>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = Order.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Order-objects as value to a dart map
  static Map<String, List<Order>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Order>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = Order.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
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
  String toString() => value;

  String toJson() => value;

  static const placed = OrderStatusEnum._(r'placed');
  static const approved = OrderStatusEnum._(r'approved');
  static const delivered = OrderStatusEnum._(r'delivered');

  /// List of all possible values in this [enum][OrderStatusEnum].
  static const values = <OrderStatusEnum>[
    placed,
    approved,
    delivered,
  ];

  static OrderStatusEnum fromJson(dynamic value) =>
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
      case r'placed': return OrderStatusEnum.placed;
      case r'approved': return OrderStatusEnum.approved;
      case r'delivered': return OrderStatusEnum.delivered;
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

