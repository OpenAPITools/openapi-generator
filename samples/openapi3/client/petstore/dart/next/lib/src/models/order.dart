// Model def

import 'package:petstore_api/_internal.dart';


part 'order.reflection.dart';


/// OrderMixin
///
/// Properties:
/// * [id] 
/// * [petId] 
/// * [quantity] 
/// * [shipDate] 
/// * [status] - Order Status
/// * [complete] 
mixin OrderMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
> get id;
UndefinedWrapper<
            int
> get petId;
UndefinedWrapper<
            int
> get quantity;
UndefinedWrapper<
            DateTime
> get shipDate;
UndefinedWrapper<
            OrderStatusEnum
> get status;
UndefinedWrapper<
            bool
> get complete;
  
}

/// Order
///
/// Properties:
/// * [id] 
/// * [petId] 
/// * [quantity] 
/// * [shipDate] 
/// * [status] - Order Status
/// * [complete] 
class Order with
$OpenApiObjectMixin,

OrderMixin {
  @override
  UndefinedWrapper<
            int
> id;
  @override
  UndefinedWrapper<
            int
> petId;
  @override
  UndefinedWrapper<
            int
> quantity;
  @override
  UndefinedWrapper<
            DateTime
> shipDate;
  @override
  UndefinedWrapper<
            OrderStatusEnum
> status;
  @override
  UndefinedWrapper<
            bool
> complete;

  AdditionalProperties<Object
?> additionalProperties;

  

  Order.$all({
        required this.id,
    required this.petId,
    required this.quantity,
    required this.shipDate,
    required this.status,
    required this.complete,
    required this.additionalProperties,
    
  });

  Order({
      this.id = const UndefinedWrapper
        .undefined()
,
  this.petId = const UndefinedWrapper
        .undefined()
,
  this.quantity = const UndefinedWrapper
        .undefined()
,
  this.shipDate = const UndefinedWrapper
        .undefined()
,
  this.status = const UndefinedWrapper
        .undefined()
,
  this.complete = const UndefinedWrapper
    (
        
        false
    )
    
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = OrderReflection.instance;
  OrderReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory Order.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Order clone() {
    return $reflection.clone(this);
  }
}














extension type const OrderStatusEnum._(String value) implements String {
  /// Order Status
      const OrderStatusEnum.placed() : this._(r'placed');
  /// Order Status
      const OrderStatusEnum.approved() : this._(r'approved');
  /// Order Status
      const OrderStatusEnum.delivered() : this._(r'delivered');

  /// Creates a [OrderStatusEnum] enum from a value and safely checking if it exists.
  factory OrderStatusEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static const $reflection = EnumReflection<OrderStatusEnum, String>(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'placed', oasValue: r'placed', value: OrderStatusEnum.placed()),
      
        EnumMemberReflection(dartName: r'approved', oasValue: r'approved', value: OrderStatusEnum.approved()),
      
        EnumMemberReflection(dartName: r'delivered', oasValue: r'delivered', value: OrderStatusEnum.delivered()),
      
    ],
  );

  factory OrderStatusEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
  }

  /// Creates a [OrderStatusEnum] enum from a value without checking if it exists.
  const OrderStatusEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<OrderStatusEnum> values = [
    OrderStatusEnum.placed(),
    OrderStatusEnum.approved(),
    OrderStatusEnum.delivered(),
    
  ];
}





