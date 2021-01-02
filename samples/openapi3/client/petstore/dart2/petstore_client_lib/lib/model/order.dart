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
        status: _$enumDecode(_$OrderStatusEnum, json[r'status']),
        complete: json[r'complete'],
    );

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
enum OrderStatusEnum {
        placed,
        approved,
        delivered,
}

const _$OrderStatusEnum = <OrderStatusEnum, dynamic>{
        OrderStatusEnum.placed: 'placed',
        OrderStatusEnum.approved: 'approved',
        OrderStatusEnum.delivered: 'delivered',
};


