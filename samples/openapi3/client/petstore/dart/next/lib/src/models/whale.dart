// Model def

import 'package:petstore_api/_internal.dart';


part 'whale.reflection.dart';


/// WhaleMixin
///
/// Properties:
/// * [hasBaleen] 
/// * [hasTeeth] 
/// * [className] 
mixin WhaleMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            bool
> get hasBaleen;
UndefinedWrapper<
            bool
> get hasTeeth;

            String
 get className;
  
}

/// Whale
///
/// Properties:
/// * [hasBaleen] 
/// * [hasTeeth] 
/// * [className] 
class Whale with
$OpenApiObjectMixin,

WhaleMixin {
  @override
  UndefinedWrapper<
            bool
> hasBaleen;
  @override
  UndefinedWrapper<
            bool
> hasTeeth;
  @override
  
            String
 className;

  AdditionalProperties<Object
?> additionalProperties;

  

  Whale.$all({
        required this.hasBaleen,
    required this.hasTeeth,
    required this.className,
    required this.additionalProperties,
    
  });

  Whale({
      this.hasBaleen = const UndefinedWrapper
        .undefined()
,
  this.hasTeeth = const UndefinedWrapper
        .undefined()
,
required  this.className     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = WhaleReflection.instance;
  WhaleReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory Whale.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Whale clone() {
    return $reflection.clone(this);
  }
}











