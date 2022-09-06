//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
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
    id.hashCode +
    petId.hashCode +
    quantity.hashCode +
    shipDate.hashCode +
    status.hashCode +
    complete.hashCode;

  factory Order.fromJson(Map<String, dynamic> json) => _$OrderFromJson(json);

  Map<String, dynamic> toJson() => _$OrderToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

/// Order Status
enum OrderStatusEnum {
  @JsonValue(r'placed')
  placed,
  @JsonValue(r'approved')
  approved,
  @JsonValue(r'delivered')
  delivered,
  @JsonValue(r'unknown_default_open_api')
  unknownDefaultOpenApi,
}


