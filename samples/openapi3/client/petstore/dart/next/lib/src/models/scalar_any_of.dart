// Model def

import 'package:petstore_api/_internal.dart';


part 'scalar_any_of.reflection.dart';


/// Values of scalar type using anyOf
mixin ScalarAnyOfMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            String
> get anyOf0;
  UndefinedWrapper<
            num
> get anyOf1;
  UndefinedWrapper<
            bool
> get anyOf2;
}

/// Values of scalar type using anyOf
class ScalarAnyOf with
$OpenApiObjectMixin,

ScalarAnyOfMixin {

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            String
> anyOf0;
  
  @override
  UndefinedWrapper<
            num
> anyOf1;
  
  @override
  UndefinedWrapper<
            bool
> anyOf2;
  

  ScalarAnyOf.$all({
        required this.additionalProperties,
    
    required this.anyOf0,
    required this.anyOf1,
    required this.anyOf2,
  });

  ScalarAnyOf({
        AdditionalProperties<Object
?>? additionalProperties,
    
    this.anyOf0 = const UndefinedWrapper.undefined(),
    
    this.anyOf1 = const UndefinedWrapper.undefined(),
    
    this.anyOf2 = const UndefinedWrapper.undefined(),
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ScalarAnyOfReflection.instance;
  ScalarAnyOfReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  factory ScalarAnyOf.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  ScalarAnyOf clone() {
    return $reflection.clone(this);
  }
}


