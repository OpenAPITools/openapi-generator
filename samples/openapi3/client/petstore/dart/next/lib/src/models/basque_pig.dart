// Model def

import 'package:petstore_api/_internal.dart';


part 'basque_pig.reflection.dart';


/// BasquePigMixin
///
/// Properties:
/// * [className] 
mixin BasquePigMixin on
  $OpenApiObjectMixin {
  
            String
 get className;
  
}

/// BasquePig
///
/// Properties:
/// * [className] 
class BasquePig with
$OpenApiObjectMixin,

BasquePigMixin {
  @override
  
            String
 className;

  AdditionalProperties<Object
?> additionalProperties;

  

  BasquePig.$all({
        required this.className,
    required this.additionalProperties,
    
  });

  BasquePig({
    required  this.className     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = BasquePigReflection.instance;
  BasquePigReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory BasquePig.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  BasquePig clone() {
    return $reflection.clone(this);
  }
}





