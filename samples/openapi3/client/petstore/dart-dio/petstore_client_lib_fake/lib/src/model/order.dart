//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'order.g.dart';

/// Order
///
/// Properties:
/// * [id] 
/// * [petId] 
/// * [quantity] 
/// * [shipDate] 
/// * [status] - Order Status
/// * [complete] 
@BuiltValue()
abstract class Order implements Built<Order, OrderBuilder> {
  @BuiltValueField(wireName: r'id')
  int? get id;

  @BuiltValueField(wireName: r'petId')
  int? get petId;

  @BuiltValueField(wireName: r'quantity')
  int? get quantity;

  @BuiltValueField(wireName: r'shipDate')
  DateTime? get shipDate;

  /// Order Status
  @BuiltValueField(wireName: r'status')
  OrderStatusEnum? get status;
  // enum statusEnum {  placed,  approved,  delivered,  };

  @BuiltValueField(wireName: r'complete')
  bool? get complete;

  Order._();

  factory Order([void updates(OrderBuilder b)]) = _$Order;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(OrderBuilder b) => b
      ..complete = false;

  @BuiltValueSerializer(custom: true)
  static Serializer<Order> get serializer => _$OrderSerializer();
}

class _$OrderSerializer implements PrimitiveSerializer<Order> {
  @override
  final Iterable<Type> types = const [Order, _$Order];

  @override
  final String wireName = r'Order';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Order object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.id != null) {
      yield r'id';
      yield serializers.serialize(
        object.id,
        specifiedType: const FullType(int),
      );
    }
    if (object.petId != null) {
      yield r'petId';
      yield serializers.serialize(
        object.petId,
        specifiedType: const FullType(int),
      );
    }
    if (object.quantity != null) {
      yield r'quantity';
      yield serializers.serialize(
        object.quantity,
        specifiedType: const FullType(int),
      );
    }
    if (object.shipDate != null) {
      yield r'shipDate';
      yield serializers.serialize(
        object.shipDate,
        specifiedType: const FullType(DateTime),
      );
    }
    if (object.status != null) {
      yield r'status';
      yield serializers.serialize(
        object.status,
        specifiedType: const FullType(OrderStatusEnum),
      );
    }
    if (object.complete != null) {
      yield r'complete';
      yield serializers.serialize(
        object.complete,
        specifiedType: const FullType(bool),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    Order object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required OrderBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'id':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.id = valueDes;
          break;
        case r'petId':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.petId = valueDes;
          break;
        case r'quantity':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.quantity = valueDes;
          break;
        case r'shipDate':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(DateTime),
          ) as DateTime;
          result.shipDate = valueDes;
          break;
        case r'status':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(OrderStatusEnum),
          ) as OrderStatusEnum;
          result.status = valueDes;
          break;
        case r'complete':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(bool),
          ) as bool;
          result.complete = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  Order deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = OrderBuilder();
    final serializedList = (serialized as Iterable<Object?>).toList();
    final unhandled = <Object?>[];
    _deserializeProperties(
      serializers,
      serialized,
      specifiedType: specifiedType,
      serializedList: serializedList,
      unhandled: unhandled,
      result: result,
    );
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
  /// Order Status
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const OrderStatusEnum unknownDefaultOpenApi = _$orderStatusEnum_unknownDefaultOpenApi;

  static Serializer<OrderStatusEnum> get serializer => _$orderStatusEnumSerializer;

  const OrderStatusEnum._(String name): super(name);

  static BuiltSet<OrderStatusEnum> get values => _$orderStatusEnumValues;
  static OrderStatusEnum valueOf(String name) => _$orderStatusEnumValueOf(name);
}

