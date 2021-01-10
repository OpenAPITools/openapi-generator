//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'order.g.dart';

abstract class Order implements Built<Order, OrderBuilder> {

    @nullable
    @BuiltValueField(wireName: r'id')
    int get id;

    @nullable
    @BuiltValueField(wireName: r'petId')
    int get petId;

    @nullable
    @BuiltValueField(wireName: r'quantity')
    int get quantity;

    @nullable
    @BuiltValueField(wireName: r'shipDate')
    DateTime get shipDate;

    /// Order Status
    @nullable
    @BuiltValueField(wireName: r'status')
    OrderStatusEnum get status;
    // enum statusEnum {  placed,  approved,  delivered,  };

    @nullable
    @BuiltValueField(wireName: r'complete')
    bool get complete;

    // Boilerplate code needed to wire-up generated code
    Order._();

    static void _initializeBuilder(OrderBuilder b) => b
        ..complete = false;

    factory Order([void updates(OrderBuilder b)]) = _$Order;
    static Serializer<Order> get serializer => _$orderSerializer;
}

class OrderStatusEnum extends EnumClass {

  /// Order Status
  @BuiltValueEnumConst(wireName: r'placed')
  static const OrderStatusEnum placed = _$orderStatusEnum_placed;
  /// Order Status
  @BuiltValueEnumConst(wireName: r'approved')
  static const OrderStatusEnum approved = _$orderStatusEnum_approved;
  /// Order Status
  @BuiltValueEnumConst(wireName: r'delivered')
  static const OrderStatusEnum delivered = _$orderStatusEnum_delivered;

  static Serializer<OrderStatusEnum> get serializer => _$orderStatusEnumSerializer;

  const OrderStatusEnum._(String name): super(name);

  static BuiltSet<OrderStatusEnum> get values => _$orderStatusEnumValues;
  static OrderStatusEnum valueOf(String name) => _$orderStatusEnumValueOf(name);
}

