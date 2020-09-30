//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: avoid_init_to_null, lines_longer_than_80_chars
// ignore_for_file: prefer_single_quotes

part of openapi.api;

/// [String] values for all properties defined in [Order].
abstract class OrderStrings {
  const OrderStrings._();

  static const id_ = "id";
  static const petId_ = "petId";
  static const quantity_ = "quantity";
  static const shipDate_ = "shipDate";
  static const status_ = "status";
  static const complete_ = "complete";
}

class Order {
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
    id.hashCode +
    petId.hashCode +
    quantity.hashCode +
    shipDate.hashCode +
    status.hashCode +
    complete.hashCode;

  @override
  String toString() => _toString("");

  Order.fromJson(Map<String, dynamic> json) {
    if (json == null) {
      return;
    }
    id = json[OrderStrings.id_];
    petId = json[OrderStrings.petId_];
    quantity = json[OrderStrings.quantity_];
    shipDate = json[OrderStrings.shipDate_] == null ?
      null :
      DateTime.parse(json[OrderStrings.shipDate_]);
    status = OrderStatusEnum.fromJson(json[OrderStrings.status_]);
    complete = json[OrderStrings.complete_];
  }

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (id != null) {
      json[OrderStrings.id_] = id;
    }
    if (petId != null) {
      json[OrderStrings.petId_] = petId;
    }
    if (quantity != null) {
      json[OrderStrings.quantity_] = quantity;
    }
    if (shipDate != null) {
      json[OrderStrings.shipDate_] = shipDate.toUtc().toIso8601String();
    }
    if (status != null) {
      json[OrderStrings.status_] = status;
    }
    if (complete != null) {
      json[OrderStrings.complete_] = complete;
    }
    return json;
  }

  String _toString(String prefix) {
    final sb = StringBuffer();

    sb.write("Order=[");

    sb.write("\n$prefix  ");
    sb.write(OrderStrings.id_);
    sb.write(": ");
    sb.write(id);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(OrderStrings.petId_);
    sb.write(": ");
    sb.write(petId);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(OrderStrings.quantity_);
    sb.write(": ");
    sb.write(quantity);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(OrderStrings.shipDate_);
    sb.write(": ");
    sb.write(shipDate == null ? "null" : shipDate._toString("$prefix  "));
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(OrderStrings.status_);
    sb.write(": ");
    sb.write(status);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(OrderStrings.complete_);
    sb.write(": ");
    sb.write(complete);
  

    sb.write("\n$prefix]");

    return sb.toString();
  }

  static List<Order> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable}) =>
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
  static Map<String, List<Order>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable}) {
    final map = <String, List<Order>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = Order.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

/// [String] values for all enums defined in [Order].
abstract class OrderStrings {
  const OrderStrings._();

  static const placed_ = "placed";
  static const approved_ = "approved";
  static const delivered_ = "delivered";
}

/// Order Status
class OrderStatusEnum {
  /// Instantiate a new enum with the provided [value].
  const OrderStatusEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is OrderStatusEnum && other.value == value ||
      other is String && other == value;

  @override
  int get hashCode => toString().hashCode;

  @override
  String toString() => value;

  String toJson() => toString();

  String _toString(String _) => toString();

  static const placed_ = OrderStatusEnum._(OrderStatusEnumStrings.placed_);
  static const approved_ = OrderStatusEnum._(OrderStatusEnumStrings.approved_);
  static const delivered_ = OrderStatusEnum._(OrderStatusEnumStrings.delivered_);

  /// List of all possible values in this [enum][OrderStatusEnum].
  static const values = <OrderStatusEnum>[
    placed_,
    approved_,
    delivered_,
  ];

  static OrderStatusEnum fromJson(String value) =>
    OrderStatusEnumTypeTransformer().decode(value);

  static List<OrderStatusEnum> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable}) =>
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
      case OrderStatusEnumStrings.placed_:
        return OrderStatusEnum.placed_;
      case OrderStatusEnumStrings.approved_:
        return OrderStatusEnum.approved_;
      case OrderStatusEnumStrings.delivered_:
        return OrderStatusEnum.delivered_;
      default:
        if (false == allowNull) {
          throw ArgumentError("Unknown enum value to decode: $data");
        }
    }
    return null;
  }

  /// Singleton [OrderStatusEnumTypeTransformer] instance.
  static OrderStatusEnumTypeTransformer _instance;
}

