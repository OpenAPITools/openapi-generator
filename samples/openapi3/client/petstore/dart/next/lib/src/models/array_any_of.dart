// Model def

import 'package:petstore_api/_internal.dart';


part 'array_any_of.reflection.dart';


/// ArrayAnyOfMixin
mixin ArrayAnyOfMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            int
> get anyOf0;
  UndefinedWrapper<
    List<
        
            String
>
> get anyOf1;
}

/// ArrayAnyOf
class ArrayAnyOf with
$OpenApiObjectMixin,

ArrayAnyOfMixin {

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            int
> anyOf0;
  
  @override
  UndefinedWrapper<
    List<
        
            String
>
> anyOf1;
  

  ArrayAnyOf.$all({
        required this.additionalProperties,
    
    required this.anyOf0,
    required this.anyOf1,
  });

  ArrayAnyOf({
        AdditionalProperties<Object
?>? additionalProperties,
    
    this.anyOf0 = const UndefinedWrapper.undefined(),
    
    this.anyOf1 = const UndefinedWrapper.undefined(),
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ArrayAnyOfReflection.instance;
  ArrayAnyOfReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  factory ArrayAnyOf.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  ArrayAnyOf clone() {
    return $reflection.clone(this);
  }
}


