// Model def

import 'package:petstore_api/_internal.dart';


part 'equilateral_triangle.reflection.dart';


/// EquilateralTriangleMixin
///
/// Properties:
mixin EquilateralTriangleMixin on
  ShapeInterfaceMixin, TriangleInterfaceMixin, $OpenApiObjectMixin {
    
}

/// EquilateralTriangle
///
/// Properties:
/// * [shapeType] 
/// * [triangleType] 
class EquilateralTriangle with
$OpenApiObjectMixin,
ShapeInterfaceMixin,TriangleInterfaceMixin,
EquilateralTriangleMixin {
  @override
  
            String
 shapeType;
  @override
  
            String
 triangleType;

  AdditionalProperties<Object
?> additionalProperties;

  

  EquilateralTriangle.$all({
        required this.shapeType,
    required this.triangleType,
    required this.additionalProperties,
    
  });

  EquilateralTriangle({
    required  this.shapeType     ,
required  this.triangleType     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = EquilateralTriangleReflection.instance;
  EquilateralTriangleReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  factory EquilateralTriangle.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  EquilateralTriangle clone() {
    return $reflection.clone(this);
  }
}


