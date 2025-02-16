// Model def

import 'package:petstore_api/_internal.dart';


part 'free_form_object_test_class_properties.reflection.dart';


/// FreeFormObjectTestClassPropertiesMixin
mixin FreeFormObjectTestClassPropertiesMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            String
> get oneOf0;
  UndefinedWrapper<
    Map<String, 
        Object
?>
> get oneOf1;
}

/// FreeFormObjectTestClassProperties
class FreeFormObjectTestClassProperties with
$OpenApiObjectMixin,

FreeFormObjectTestClassPropertiesMixin {

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            String
> oneOf0;
  
  @override
  UndefinedWrapper<
    Map<String, 
        Object
?>
> oneOf1;
  

  FreeFormObjectTestClassProperties.$all({
        required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
  });

  FreeFormObjectTestClassProperties({
        AdditionalProperties<Object
?>? additionalProperties,
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = FreeFormObjectTestClassPropertiesReflection.instance;
  FreeFormObjectTestClassPropertiesReflection get $classReflection => $reflection;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,].where((e) => e.isDefined).take(2);
      if (oneOfs.length > 1) {
        // there must be AT MOST one "oneOf" schema.
        return false;
      }
      
    return super.validate();
  }

  factory FreeFormObjectTestClassProperties.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  FreeFormObjectTestClassProperties clone() {
    return $reflection.clone(this);
  }
}


