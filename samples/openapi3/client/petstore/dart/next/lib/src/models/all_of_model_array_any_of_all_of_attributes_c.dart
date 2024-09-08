// Model def

import 'package:petstore_api/_internal.dart';


part 'all_of_model_array_any_of_all_of_attributes_c.reflection.dart';


/// AllOfModelArrayAnyOfAllOfAttributesCMixin
///
/// Properties:
mixin AllOfModelArrayAnyOfAllOfAttributesCMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            Pet
> get oneOf0;
  UndefinedWrapper<
            Order
> get oneOf1;
}

/// AllOfModelArrayAnyOfAllOfAttributesC
///
/// Properties:
class AllOfModelArrayAnyOfAllOfAttributesC with
$OpenApiObjectMixin,

AllOfModelArrayAnyOfAllOfAttributesCMixin {

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            Pet
> oneOf0;
  
  @override
  UndefinedWrapper<
            Order
> oneOf1;
  

  AllOfModelArrayAnyOfAllOfAttributesC.$all({
        required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
  });

  AllOfModelArrayAnyOfAllOfAttributesC({
        AdditionalProperties<Object
?>? additionalProperties,
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = AllOfModelArrayAnyOfAllOfAttributesCReflection.instance;
  AllOfModelArrayAnyOfAllOfAttributesCReflection get $classReflection => $reflection;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,].where((e) => e.isDefined).take(2);
      if (oneOfs.length > 1) {
        // there must be AT MOST one "oneOf" schema.
        return false;
      }
      
    return super.validate();
  }

  factory AllOfModelArrayAnyOfAllOfAttributesC.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  AllOfModelArrayAnyOfAllOfAttributesC clone() {
    return $reflection.clone(this);
  }
}


