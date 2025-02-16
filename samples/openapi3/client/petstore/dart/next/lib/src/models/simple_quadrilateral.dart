// Model def

import 'package:petstore_api/_internal.dart';


part 'simple_quadrilateral.reflection.dart';


/// SimpleQuadrilateralMixin
///
/// Properties:
mixin SimpleQuadrilateralMixin on
  QuadrilateralInterfaceMixin, ShapeInterfaceMixin, $OpenApiObjectMixin {
    
}

/// SimpleQuadrilateral
///
/// Properties:
/// * [quadrilateralType] 
/// * [shapeType] 
class SimpleQuadrilateral with
$OpenApiObjectMixin,
QuadrilateralInterfaceMixin,ShapeInterfaceMixin,
SimpleQuadrilateralMixin {
  @override
  
            String
 quadrilateralType;
  @override
  
            String
 shapeType;

  AdditionalProperties<Object
?> additionalProperties;

  

  SimpleQuadrilateral.$all({
        required this.quadrilateralType,
    required this.shapeType,
    required this.additionalProperties,
    
  });

  SimpleQuadrilateral({
    required  this.quadrilateralType     ,
required  this.shapeType     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = SimpleQuadrilateralReflection.instance;
  SimpleQuadrilateralReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  factory SimpleQuadrilateral.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  SimpleQuadrilateral clone() {
    return $reflection.clone(this);
  }
}


