// Model def

import 'package:petstore_api/_internal.dart';


part 'isosceles_triangle.reflection.dart';


/// IsoscelesTriangleMixin
///
/// Properties:
mixin IsoscelesTriangleMixin on
  ShapeInterfaceMixin, TriangleInterfaceMixin, $OpenApiObjectMixin {
    
}

/// IsoscelesTriangle
///
/// Properties:
/// * [shapeType] 
/// * [triangleType] 
class IsoscelesTriangle with
$OpenApiObjectMixin,
ShapeInterfaceMixin,TriangleInterfaceMixin,
IsoscelesTriangleMixin {
  @override
  
            String
 shapeType;
  @override
  
            String
 triangleType;



  

  IsoscelesTriangle.$all({
        required this.shapeType,
    required this.triangleType,
    
    
  });

  IsoscelesTriangle({
    required  this.shapeType     ,
required  this.triangleType     ,
    
    
  });

  static const $reflection = IsoscelesTriangleReflection.instance;
  IsoscelesTriangleReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  factory IsoscelesTriangle.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  IsoscelesTriangle clone() {
    return $reflection.clone(this);
  }
}


