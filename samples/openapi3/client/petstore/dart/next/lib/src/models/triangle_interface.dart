// Model def

import 'package:petstore_api/_internal.dart';


part 'triangle_interface.reflection.dart';


/// TriangleInterfaceMixin
///
/// Properties:
/// * [triangleType] 
mixin TriangleInterfaceMixin on
  $OpenApiObjectMixin {
  
            String
 get triangleType;
  
}

/// TriangleInterface
///
/// Properties:
/// * [triangleType] 
class TriangleInterface with
$OpenApiObjectMixin,

TriangleInterfaceMixin {
  @override
  
            String
 triangleType;

  AdditionalProperties<Object
?> additionalProperties;

  

  TriangleInterface.$all({
        required this.triangleType,
    required this.additionalProperties,
    
  });

  TriangleInterface({
    required  this.triangleType     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = TriangleInterfaceReflection.instance;
  TriangleInterfaceReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory TriangleInterface.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  TriangleInterface clone() {
    return $reflection.clone(this);
  }
}





