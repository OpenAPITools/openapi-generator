// Model def

import 'package:petstore_api/_internal.dart';


part 'complex_quadrilateral.reflection.dart';


/// ComplexQuadrilateralMixin
///
/// Properties:
mixin ComplexQuadrilateralMixin on
  QuadrilateralInterfaceMixin, ShapeInterfaceMixin, $OpenApiObjectMixin {
    
}

/// ComplexQuadrilateral
///
/// Properties:
/// * [quadrilateralType] 
/// * [shapeType] 
class ComplexQuadrilateral with
$OpenApiObjectMixin,
QuadrilateralInterfaceMixin,ShapeInterfaceMixin,
ComplexQuadrilateralMixin {
  @override
  
            String
 quadrilateralType;
  @override
  
            String
 shapeType;

  AdditionalProperties<Object
?> additionalProperties;

  

  ComplexQuadrilateral.$all({
        required this.quadrilateralType,
    required this.shapeType,
    required this.additionalProperties,
    
  });

  ComplexQuadrilateral({
    required  this.quadrilateralType     ,
required  this.shapeType     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ComplexQuadrilateralReflection.instance;
  ComplexQuadrilateralReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  factory ComplexQuadrilateral.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  ComplexQuadrilateral clone() {
    return $reflection.clone(this);
  }
}


