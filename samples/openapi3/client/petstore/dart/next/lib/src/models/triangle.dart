// Model def

import 'package:petstore_api/_internal.dart';


part 'triangle.reflection.dart';


/// TriangleMixin
///
/// Properties:
/// * [triangleType] 
mixin TriangleMixin on
  $OpenApiObjectMixin {
  
            String
 get triangleType;
  
  UndefinedWrapper<
            EquilateralTriangle
> get oneOf0;
  UndefinedWrapper<
            IsoscelesTriangle
> get oneOf1;
  UndefinedWrapper<
            ScaleneTriangle
> get oneOf2;
}

/// Triangle
///
/// Properties:
/// * [triangleType] 
class Triangle with
$OpenApiObjectMixin,

TriangleMixin {
  @override
  
            String
 triangleType;

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            EquilateralTriangle
> oneOf0;
  
  @override
  UndefinedWrapper<
            IsoscelesTriangle
> oneOf1;
  
  @override
  UndefinedWrapper<
            ScaleneTriangle
> oneOf2;
  

  Triangle.$all({
        required this.triangleType,
    required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
    required this.oneOf2,
  });

  Triangle({
    required  this.triangleType     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
    this.oneOf2 = const UndefinedWrapper.undefined(),
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = TriangleReflection.instance;
  TriangleReflection get $classReflection => $reflection;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,oneOf2,].where((e) => e.isDefined).take(2);
      if (oneOfs.length > 1) {
        // there must be AT MOST one "oneOf" schema.
        return false;
      }
      
    return super.validate();
  }

  factory Triangle.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Triangle clone() {
    return $reflection.clone(this);
  }
}





