// Model def

import 'package:petstore_api/_internal.dart';


part 'shape.reflection.dart';


/// ShapeMixin
mixin ShapeMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            Triangle
> get oneOf0;
  UndefinedWrapper<
            Quadrilateral
> get oneOf1;
}

/// Shape
class Shape with
$OpenApiObjectMixin,

ShapeMixin {

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            Triangle
> oneOf0;
  
  @override
  UndefinedWrapper<
            Quadrilateral
> oneOf1;
  

  Shape.$all({
        required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
  });

  Shape({
        AdditionalProperties<Object
?>? additionalProperties,
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ShapeReflection.instance;
  ShapeReflection get $classReflection => $reflection;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,].where((e) => e.isDefined).take(2);
      if (oneOfs.length > 1) {
        // there must be AT MOST one "oneOf" schema.
        return false;
      }
      
    return super.validate();
  }

  factory Shape.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Shape clone() {
    return $reflection.clone(this);
  }
}


