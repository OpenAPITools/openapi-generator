        import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'order.g.dart';

abstract class Order implements Built<Order, OrderBuilder> {

    
        @nullable

    
    @BuiltValueField(wireName: 'id')
    int get id;
    
        @nullable

    
    @BuiltValueField(wireName: 'petId')
    int get petId;
    
        @nullable

    
    @BuiltValueField(wireName: 'quantity')
    int get quantity;
    
        @nullable

    
    @BuiltValueField(wireName: 'shipDate')
    DateTime get shipDate;
    /* Order Status */
        @nullable

    /* Order Status */
    @BuiltValueField(wireName: 'status')
    String get status;
        //enum statusEnum {  placed,  approved,  delivered,  };
    
        @nullable

    
    @BuiltValueField(wireName: 'complete')
    bool get complete;

    // Boilerplate code needed to wire-up generated code
    Order._();

    factory Order([updates(OrderBuilder b)]) = _$Order;
    static Serializer<Order> get serializer => _$orderSerializer;

}

