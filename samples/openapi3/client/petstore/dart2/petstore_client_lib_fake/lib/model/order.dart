//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
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
  // ignore: unnecessary_parenthesis
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
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Order fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return Order(
        id: mapValueOfType<int>(json, r'id'),
        petId: mapValueOfType<int>(json, r'petId'),
        quantity: mapValueOfType<int>(json, r'quantity'),
        shipDate: mapDateTime(json, r'shipDate', ''),
        status: OrderStatusEnum.fromJson(json[r'status']),
        complete: mapValueOfType<bool>(json, r'complete'),
      );
    }
    return null;
  }

  static List<Order> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(Order.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <Order>[];

  static Map<String, Order> mapFromJson(dynamic json) {
    final map = <String, Order>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = Order.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Order-objects as value to a dart map
  static Map<String, List<Order>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Order>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = Order.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
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
  String toString() => value ?? '';

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

  static List<OrderStatusEnum> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(OrderStatusEnum.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <OrderStatusEnum>[];
}

/// Transformation class that can [encode] an instance of [OrderStatusEnum] to String,
/// and [decode] dynamic data back to [OrderStatusEnum].
class OrderStatusEnumTypeTransformer {
  factory OrderStatusEnumTypeTransformer() => _instance ??= const OrderStatusEnumTypeTransformer._();

  const OrderStatusEnumTypeTransformer._();

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
    if (data != null) {
      switch (data.toString()) {
        case r'placed': return OrderStatusEnum.placed;
        case r'approved': return OrderStatusEnum.approved;
        case r'delivered': return OrderStatusEnum.delivered;
        default:
          if (allowNull == false) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [OrderStatusEnumTypeTransformer] instance.
  static OrderStatusEnumTypeTransformer _instance;
}


