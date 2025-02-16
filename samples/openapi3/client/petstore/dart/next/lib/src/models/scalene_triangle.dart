// Model def

import 'package:petstore_api/_internal.dart';


part 'scalene_triangle.reflection.dart';


/// ScaleneTriangleMixin
///
/// Properties:
mixin ScaleneTriangleMixin on
  ShapeInterfaceMixin, TriangleInterfaceMixin, $OpenApiObjectMixin {
    
}

/// ScaleneTriangle
///
/// Properties:
/// * [shapeType] 
/// * [triangleType] 
class ScaleneTriangle with
$OpenApiObjectMixin,
ShapeInterfaceMixin,TriangleInterfaceMixin,
ScaleneTriangleMixin {
  @override
  
            String
 shapeType;
  @override
  
            String
 triangleType;

  AdditionalProperties<Object
?> additionalProperties;

  

  ScaleneTriangle.$all({
        required this.shapeType,
    required this.triangleType,
    required this.additionalProperties,
    
  });

  ScaleneTriangle({
    required  this.shapeType     ,
required  this.triangleType     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ScaleneTriangleReflection.instance;
  ScaleneTriangleReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  factory ScaleneTriangle.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  ScaleneTriangle clone() {
    return $reflection.clone(this);
  }
}


