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
    String get status;
        //enum statusEnum {  placed,  approved,  delivered,  };
    
        @nullable
    @BuiltValueField(wireName: r'complete')
    bool get complete;

    // Boilerplate code needed to wire-up generated code
    Order._();

    factory Order([updates(OrderBuilder b)]) = _$Order;
    static Serializer<Order> get serializer => _$orderSerializer;

}

