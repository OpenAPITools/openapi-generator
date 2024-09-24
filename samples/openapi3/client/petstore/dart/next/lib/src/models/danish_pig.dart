// Model def

import 'package:petstore_api/_internal.dart';


part 'danish_pig.reflection.dart';


/// DanishPigMixin
///
/// Properties:
/// * [className] 
mixin DanishPigMixin on
  $OpenApiObjectMixin {
  
            String
 get className;
  
}

/// DanishPig
///
/// Properties:
/// * [className] 
class DanishPig with
$OpenApiObjectMixin,

DanishPigMixin {
  @override
  
            String
 className;

  AdditionalProperties<Object
?> additionalProperties;

  

  DanishPig.$all({
        required this.className,
    required this.additionalProperties,
    
  });

  DanishPig({
    required  this.className     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = DanishPigReflection.instance;
  DanishPigReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory DanishPig.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  DanishPig clone() {
    return $reflection.clone(this);
  }
}





