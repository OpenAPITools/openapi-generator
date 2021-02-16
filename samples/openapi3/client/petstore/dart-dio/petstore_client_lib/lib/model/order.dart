//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

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

    @BuiltValueField(wireName: r'complete')
    bool get complete;

    Order._();

    static void _initializeBuilder(OrderBuilder b) => b
        ..complete = false;

    factory Order([void updates(OrderBuilder b)]) = _$Order;

    @BuiltValueSerializer(custom: true)
    static Serializer<Order> get serializer => _$OrderSerializer();
}

class _$OrderSerializer implements StructuredSerializer<Order> {

    @override
    final Iterable<Type> types = const [Order, _$Order];
    @override
    final String wireName = r'Order';

    @override
    Iterable<Object> serialize(Serializers serializers, Order object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.id != null) {
            result
                ..add(r'id')
                ..add(serializers.serialize(object.id,
                    specifiedType: const FullType(int)));
        }
        if (object.petId != null) {
            result
                ..add(r'petId')
                ..add(serializers.serialize(object.petId,
                    specifiedType: const FullType(int)));
        }
        if (object.quantity != null) {
            result
                ..add(r'quantity')
                ..add(serializers.serialize(object.quantity,
                    specifiedType: const FullType(int)));
        }
        if (object.shipDate != null) {
            result
                ..add(r'shipDate')
                ..add(serializers.serialize(object.shipDate,
                    specifiedType: const FullType(DateTime)));
        }
        if (object.status != null) {
            result
                ..add(r'status')
                ..add(serializers.serialize(object.status,
                    specifiedType: const FullType(OrderStatusEnum)));
        }
        if (object.complete != null) {
            result
                ..add(r'complete')
                ..add(serializers.serialize(object.complete,
                    specifiedType: const FullType(bool)));
        }
        return result;
    }

    @override
    Order deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = OrderBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'id':
                    result.id = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    break;
                case r'petId':
                    result.petId = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    break;
                case r'quantity':
                    result.quantity = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    break;
                case r'shipDate':
                    result.shipDate = serializers.deserialize(value,
                        specifiedType: const FullType(DateTime)) as DateTime;
                    break;
                case r'status':
                    result.status = serializers.deserialize(value,
                        specifiedType: const FullType(OrderStatusEnum)) as OrderStatusEnum;
                    break;
                case r'complete':
                    result.complete = serializers.deserialize(value,
                        specifiedType: const FullType(bool)) as bool;
                    break;
            }
        }
        return result.build();
    }
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

