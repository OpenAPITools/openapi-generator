// Model def

import 'package:petstore_api/_internal.dart';


part 'apple.reflection.dart';


/// AppleMixin
///
/// Properties:
/// * [cultivar] 
/// * [origin] 
mixin AppleMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get cultivar;
UndefinedWrapper<
            String
> get origin;
  
}

/// Apple
///
/// Properties:
/// * [cultivar] 
/// * [origin] 
class Apple with
$OpenApiObjectMixin,

AppleMixin {
  @override
  UndefinedWrapper<
            String
> cultivar;
  @override
  UndefinedWrapper<
            String
> origin;

  AdditionalProperties<Object
?> additionalProperties;

  

  Apple.$all({
        required this.cultivar,
    required this.origin,
    required this.additionalProperties,
    
  });

  Apple({
      this.cultivar = const UndefinedWrapper
        .undefined()
,
  this.origin = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = AppleReflection.instance;
  AppleReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory Apple.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Apple clone() {
    return $reflection.clone(this);
  }
}








