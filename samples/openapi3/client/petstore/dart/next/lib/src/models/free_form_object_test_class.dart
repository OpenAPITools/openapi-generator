// Model def

import 'package:petstore_api/_internal.dart';


part 'free_form_object_test_class.reflection.dart';


/// FreeFormObjectTestClassMixin
///
/// Properties:
/// * [name] 
/// * [properties] 
mixin FreeFormObjectTestClassMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get name;
UndefinedWrapper<
            FreeFormObjectTestClassProperties
> get properties;
  
}

/// FreeFormObjectTestClass
///
/// Properties:
/// * [name] 
/// * [properties] 
class FreeFormObjectTestClass with
$OpenApiObjectMixin,

FreeFormObjectTestClassMixin {
  @override
  UndefinedWrapper<
            String
> name;
  @override
  UndefinedWrapper<
            FreeFormObjectTestClassProperties
> properties;

  AdditionalProperties<Object
?> additionalProperties;

  

  FreeFormObjectTestClass.$all({
        required this.name,
    required this.properties,
    required this.additionalProperties,
    
  });

  FreeFormObjectTestClass({
      this.name = const UndefinedWrapper
        .undefined()
,
  this.properties = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = FreeFormObjectTestClassReflection.instance;
  FreeFormObjectTestClassReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory FreeFormObjectTestClass.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  FreeFormObjectTestClass clone() {
    return $reflection.clone(this);
  }
}








