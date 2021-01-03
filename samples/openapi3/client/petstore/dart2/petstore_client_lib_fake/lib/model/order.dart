//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

@JsonSerializable(
  checked: true,
  includeIfNull: false,
  disallowUnrecognizedKeys: true,
)
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

  @JsonKey(
    name: r'id',
    
    
    
  )
  int id;

  @JsonKey(
    name: r'petId',
    
    
    
  )
  int petId;

  @JsonKey(
    name: r'quantity',
    
    
    
  )
  int quantity;

  @JsonKey(
    name: r'shipDate',
    
    
    
  )
  DateTime shipDate;

  /// Order Status
  @JsonKey(
    name: r'status',
    
    
    
  )
  OrderStatusEnum status;

  @JsonKey(
    name: r'complete',
    
    defaultValue: false,
    
  )
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
  String toString() => toJson().toString();

  factory Order.fromJson(Map<String, dynamic> json) => _$OrderFromJson(json);
  Map<String, dynamic> toJson() => _$OrderToJson(this);
}

/// Order Status
enum OrderStatusEnum {

    @JsonValue(r'placed')
    
    placed,
    @JsonValue(r'approved')
    
    approved,
    @JsonValue(r'delivered')
    
    delivered,

}


