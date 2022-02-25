//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

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

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  int? id;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  int? petId;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  int? quantity;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  DateTime? shipDate;

  /// Order Status
  OrderStatusEnum? status;

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
    (id == null ? 0 : id!.hashCode) +
    (petId == null ? 0 : petId!.hashCode) +
    (quantity == null ? 0 : quantity!.hashCode) +
    (shipDate == null ? 0 : shipDate!.hashCode) +
    (status == null ? 0 : status!.hashCode) +
    (complete.hashCode);

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
      json[r'shipDate'] = shipDate!.toUtc().toIso8601String();
    }
    if (status != null) {
      json[r'status'] = status;
    }
      json[r'complete'] = complete;
    return json;
  }

  /// Returns a new [Order] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Order? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "Order[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "Order[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return Order(
        id: mapValueOfType<int>(json, r'id'),
        petId: mapValueOfType<int>(json, r'petId'),
        quantity: mapValueOfType<int>(json, r'quantity'),
        shipDate: mapDateTime(json, r'shipDate', ''),
        status: OrderStatusEnum.fromJson(json[r'status']),
        complete: mapValueOfType<bool>(json, r'complete') ?? false,
      );
    }
    return null;
  }

  static List<Order>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <Order>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = Order.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, Order> mapFromJson(dynamic json) {
    final map = <String, Order>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = Order.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of Order-objects as value to a dart map
  static Map<String, List<Order>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<Order>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = Order.listFromJson(entry.value, growable: growable,);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
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

  static OrderStatusEnum? fromJson(dynamic value) => OrderStatusEnumTypeTransformer().decode(value);

  static List<OrderStatusEnum>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <OrderStatusEnum>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = OrderStatusEnum.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
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
  OrderStatusEnum? decode(dynamic data, {bool allowNull = true}) {
    if (data != null) {
      switch (data.toString()) {
        case r'placed': return OrderStatusEnum.placed;
        case r'approved': return OrderStatusEnum.approved;
        case r'delivered': return OrderStatusEnum.delivered;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [OrderStatusEnumTypeTransformer] instance.
  static OrderStatusEnumTypeTransformer? _instance;
}


