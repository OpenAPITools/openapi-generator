// Model def

import 'package:petstore_api/_internal.dart';


part 'quadrilateral.reflection.dart';


/// QuadrilateralMixin
///
/// Properties:
/// * [quadrilateralType] 
mixin QuadrilateralMixin on
  $OpenApiObjectMixin {
  
            String
 get quadrilateralType;
  
  UndefinedWrapper<
            SimpleQuadrilateral
> get oneOf0;
  UndefinedWrapper<
            ComplexQuadrilateral
> get oneOf1;
}

/// Quadrilateral
///
/// Properties:
/// * [quadrilateralType] 
class Quadrilateral with
$OpenApiObjectMixin,

QuadrilateralMixin {
  @override
  
            String
 quadrilateralType;

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            SimpleQuadrilateral
> oneOf0;
  
  @override
  UndefinedWrapper<
            ComplexQuadrilateral
> oneOf1;
  

  Quadrilateral.$all({
        required this.quadrilateralType,
    required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
  });

  Quadrilateral({
    required  this.quadrilateralType     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = QuadrilateralReflection.instance;
  QuadrilateralReflection get $classReflection => $reflection;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,].where((e) => e.isDefined).take(2);
      if (oneOfs.length > 1) {
        // there must be AT MOST one "oneOf" schema.
        return false;
      }
      
    return super.validate();
  }

  factory Quadrilateral.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Quadrilateral clone() {
    return $reflection.clone(this);
  }
}





