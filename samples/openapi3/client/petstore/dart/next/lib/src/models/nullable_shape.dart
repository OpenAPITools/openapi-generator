// Model def

import 'package:petstore_api/_internal.dart';


part 'nullable_shape.reflection.dart';


/// The value may be a shape or the 'null' value. The 'nullable' attribute was introduced in OAS schema >= 3.0 and has been deprecated in OAS schema >= 3.1.
mixin NullableShapeMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            Triangle
> get oneOf0;
  UndefinedWrapper<
            Quadrilateral
> get oneOf1;
}

/// The value may be a shape or the 'null' value. The 'nullable' attribute was introduced in OAS schema >= 3.0 and has been deprecated in OAS schema >= 3.1.
class NullableShape with
$OpenApiObjectMixin,

NullableShapeMixin {

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
  

  NullableShape.$all({
        required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
  });

  NullableShape({
        AdditionalProperties<Object
?>? additionalProperties,
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = NullableShapeReflection.instance;
  NullableShapeReflection get $classReflection => $reflection;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,].where((e) => e.isDefined).take(2);
      if (oneOfs.length > 1) {
        // there must be AT MOST one "oneOf" schema.
        return false;
      }
      
    return super.validate();
  }

  factory NullableShape.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  NullableShape clone() {
    return $reflection.clone(this);
  }
}


