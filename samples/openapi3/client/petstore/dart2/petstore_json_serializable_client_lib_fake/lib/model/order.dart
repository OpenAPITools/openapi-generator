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
  createToJson: true,
  disallowUnrecognizedKeys: true,
  explicitToJson: true,
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
    nullable: false,
    name: r'id',
    required: false,
  )
  int id;

  @JsonKey(
    nullable: false,
    name: r'petId',
    required: false,
  )
  int petId;

  @JsonKey(
    nullable: false,
    name: r'quantity',
    required: false,
  )
  int quantity;

  @JsonKey(
    nullable: false,
    name: r'shipDate',
    required: false,
  )
  DateTime shipDate;

      /// Order Status
  @JsonKey(
    nullable: false,
    name: r'status',
    required: false,
  )
  OrderStatusEnum status;

  @JsonKey(
    defaultValue: false,
    name: r'complete',
    required: false,
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

  factory Order.fromJson(Map<String, dynamic> json) => _$OrderFromJson(json);

  Map<String, dynamic> toJson() => _$OrderToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

/// Order Status
enum OrderStatusEnum {
  placed,
  approved,
  delivered,
}

