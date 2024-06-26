// Model def

import 'package:openapi/_internal.dart';


part 'order.reflection.dart';
part 'order.serialization.dart';


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
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = OrderReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$OrderToMap(this);
  }
  factory Order.fromMap(Map<String, dynamic> src) {
    return _$OrderFromMap(src);
  }
  static Order? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Order.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$OrderCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Order.deserialize(Object? src) {
    return _$OrderDeserialize(src);
  }
  static Order? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Order.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$OrderCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$OrderSerialize(this);
  }
}




extension type const OrderStatusEnum._(String value) {
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

  /// Creates a [OrderStatusEnum] enum from a value without checking if it exists.
  const OrderStatusEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<OrderStatusEnum> values = [
    OrderStatusEnum.placed(),
    OrderStatusEnum.approved(),
    OrderStatusEnum.delivered(),
    
  ];
}

