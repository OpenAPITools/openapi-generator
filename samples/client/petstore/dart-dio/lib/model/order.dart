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
    /* Order Status */
        @nullable
    @BuiltValueField(wireName: r'status')
    OrderStatus get status;
        //enum statusEnum {  placed,  approved,  delivered,  };
    
        @nullable
    @BuiltValueField(wireName: r'complete')
    bool get complete;

    // Boilerplate code needed to wire-up generated code
    Order._();

    factory Order([updates(OrderBuilder b)]) = _$Order;
    static Serializer<Order> get serializer => _$orderSerializer;
}

class OrderStatus extends EnumClass {

  /// Order Status
  @BuiltValueEnumConst(wireName: "placed")
  static const OrderStatus placed = _$placed;
  /// Order Status
  @BuiltValueEnumConst(wireName: "approved")
  static const OrderStatus approved = _$approved;
  /// Order Status
  @BuiltValueEnumConst(wireName: "delivered")
  static const OrderStatus delivered = _$delivered;

  static Serializer<OrderStatus> get serializer => _$orderStatusSerializer;

  const OrderStatus._(String name): super(name);

  static BuiltSet<OrderStatus> get values => _$orderStatusValues;
  static OrderStatus valueOf(String name) => _$orderStatusValueOf(name);
}


