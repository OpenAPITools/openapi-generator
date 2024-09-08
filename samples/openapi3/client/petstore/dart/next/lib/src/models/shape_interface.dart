// Model def

import 'package:petstore_api/_internal.dart';


part 'shape_interface.reflection.dart';


/// ShapeInterfaceMixin
///
/// Properties:
/// * [shapeType] 
mixin ShapeInterfaceMixin on
  $OpenApiObjectMixin {
  
            String
 get shapeType;
  
}

/// ShapeInterface
///
/// Properties:
/// * [shapeType] 
class ShapeInterface with
$OpenApiObjectMixin,

ShapeInterfaceMixin {
  @override
  
            String
 shapeType;

  AdditionalProperties<Object
?> additionalProperties;

  

  ShapeInterface.$all({
        required this.shapeType,
    required this.additionalProperties,
    
  });

  ShapeInterface({
    required  this.shapeType     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ShapeInterfaceReflection.instance;
  ShapeInterfaceReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory ShapeInterface.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  ShapeInterface clone() {
    return $reflection.clone(this);
  }
}





