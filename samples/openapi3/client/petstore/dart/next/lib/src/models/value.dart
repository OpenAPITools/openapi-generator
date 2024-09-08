// Model def

import 'package:petstore_api/_internal.dart';


part 'value.reflection.dart';


/// ValueMixin
mixin ValueMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            Scalar
> get oneOf0;
  UndefinedWrapper<
    List<
        
            Scalar
>
> get oneOf1;
}

/// Value
class Value with
$OpenApiObjectMixin,

ValueMixin {

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            Scalar
> oneOf0;
  
  @override
  UndefinedWrapper<
    List<
        
            Scalar
>
> oneOf1;
  

  Value.$all({
        required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
  });

  Value({
        AdditionalProperties<Object
?>? additionalProperties,
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ValueReflection.instance;
  ValueReflection get $classReflection => $reflection;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,].where((e) => e.isDefined).take(2);
      if (oneOfs.length > 1) {
        // there must be AT MOST one "oneOf" schema.
        return false;
      }
      
    return super.validate();
  }

  factory Value.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Value clone() {
    return $reflection.clone(this);
  }
}


