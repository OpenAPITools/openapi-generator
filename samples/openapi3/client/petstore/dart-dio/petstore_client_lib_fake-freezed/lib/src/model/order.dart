//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'order.freezed.dart';
part 'order.g.dart';


@freezed
abstract class Order with _$Order {
  const factory Order({
    @JsonKey(
      name: r'id',
      required: false,
      includeIfNull: false,
    )
    int? id,
    @JsonKey(
      name: r'petId',
      required: false,
      includeIfNull: false,
    )
    int? petId,
    @JsonKey(
      name: r'quantity',
      required: false,
      includeIfNull: false,
    )
    int? quantity,
    @JsonKey(
      name: r'shipDate',
      required: false,
      includeIfNull: false,
    )
    DateTime? shipDate,
    /// Order Status
    @JsonKey(
      name: r'status',
      required: false,
      includeIfNull: false,
      unknownEnumValue: OrderStatusEnum.unknownDefaultOpenApi,
    )
    OrderStatusEnum? status,
    @JsonKey(
      name: r'complete',
      required: false,
      includeIfNull: false,
    )
  
    @Default(false) bool complete,
  }) = _Order;

  factory Order.fromJson(Map<String, dynamic> json) =>
      _$OrderFromJson(json);
}

/// Order Status
enum OrderStatusEnum {
/**
 * line1
 * line2
 * 
 */
@JsonValue(r'placed')
placed(r'placed'),
/**
 * line3
 * line4
 * 
 */
@JsonValue(r'approved')
approved(r'approved'),
/**
 * line5
 * line6
 * 
 */
@JsonValue(r'delivered')
delivered(r'delivered'),
@JsonValue(r'unknown_default_open_api')
unknownDefaultOpenApi(r'unknown_default_open_api');

const OrderStatusEnum(this.value);

final String value;

@override
String toString() => value;
}


