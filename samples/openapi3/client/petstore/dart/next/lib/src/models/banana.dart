// Model def

import 'package:petstore_api/_internal.dart';


part 'banana.reflection.dart';


/// BananaMixin
///
/// Properties:
/// * [lengthCm] 
mixin BananaMixin on
  $OpenApiObjectMixin {
  
            num
 get lengthCm;
  
}

/// Banana
///
/// Properties:
/// * [lengthCm] 
class Banana with
$OpenApiObjectMixin,

BananaMixin {
  @override
  
            num
 lengthCm;

  AdditionalProperties<Object
?> additionalProperties;

  

  Banana.$all({
        required this.lengthCm,
    required this.additionalProperties,
    
  });

  Banana({
    required  this.lengthCm     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = BananaReflection.instance;
  BananaReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory Banana.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Banana clone() {
    return $reflection.clone(this);
  }
}





