//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:json_annotation/json_annotation.dart';

part 'order.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
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
    
    name: r'id',
    required: false,
    includeIfNull: false
  )


  final int? id;



  @JsonKey(
    
    name: r'petId',
    required: false,
    includeIfNull: false
  )


  final int? petId;



  @JsonKey(
    
    name: r'quantity',
    required: false,
    includeIfNull: false
  )


  final int? quantity;



  @JsonKey(
    
    name: r'shipDate',
    required: false,
    includeIfNull: false
  )


  final DateTime? shipDate;



      /// Order Status
  @JsonKey(
    
    name: r'status',
    required: false,
    includeIfNull: false
  )


  final OrderStatusEnum? status;



  @JsonKey(
    defaultValue: false,
    name: r'complete',
    required: false,
    includeIfNull: false
  )


  final bool? complete;



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
  @JsonValue('placed')
  placed,
  @JsonValue('approved')
  approved,
  @JsonValue('delivered')
  delivered,
  @JsonValue('unknown_default_open_api')
  unknownDefaultOpenApi,
}


