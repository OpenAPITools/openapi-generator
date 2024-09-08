// Model def

import 'package:petstore_api/_internal.dart';


part 'scalar.reflection.dart';


/// Values of scalar type
mixin ScalarMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            String
> get oneOf0;
  UndefinedWrapper<
            num
> get oneOf1;
  UndefinedWrapper<
            bool
> get oneOf2;
}

/// Values of scalar type
class Scalar with
$OpenApiObjectMixin,

ScalarMixin {

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            String
> oneOf0;
  
  @override
  UndefinedWrapper<
            num
> oneOf1;
  
  @override
  UndefinedWrapper<
            bool
> oneOf2;
  

  Scalar.$all({
        required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
    required this.oneOf2,
  });

  Scalar({
        AdditionalProperties<Object
?>? additionalProperties,
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
    this.oneOf2 = const UndefinedWrapper.undefined(),
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ScalarReflection.instance;
  ScalarReflection get $classReflection => $reflection;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,oneOf2,].where((e) => e.isDefined).take(2);
      if (oneOfs.length > 1) {
        // there must be AT MOST one "oneOf" schema.
        return false;
      }
      
    return super.validate();
  }

  factory Scalar.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Scalar clone() {
    return $reflection.clone(this);
  }
}


