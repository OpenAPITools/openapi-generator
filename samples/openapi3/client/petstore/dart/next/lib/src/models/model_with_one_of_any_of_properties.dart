// Model def

import 'package:petstore_api/_internal.dart';


part 'model_with_one_of_any_of_properties.reflection.dart';


/// ModelWithOneOfAnyOfPropertiesMixin
///
/// Properties:
/// * [oneofProp] 
/// * [anyofProp] 
mixin ModelWithOneOfAnyOfPropertiesMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            ArrayOneOf
> get oneofProp;
UndefinedWrapper<
            ArrayAnyOf
> get anyofProp;
  
}

/// ModelWithOneOfAnyOfProperties
///
/// Properties:
/// * [oneofProp] 
/// * [anyofProp] 
class ModelWithOneOfAnyOfProperties with
$OpenApiObjectMixin,

ModelWithOneOfAnyOfPropertiesMixin {
  @override
  UndefinedWrapper<
            ArrayOneOf
> oneofProp;
  @override
  UndefinedWrapper<
            ArrayAnyOf
> anyofProp;

  AdditionalProperties<Object
?> additionalProperties;

  

  ModelWithOneOfAnyOfProperties.$all({
        required this.oneofProp,
    required this.anyofProp,
    required this.additionalProperties,
    
  });

  ModelWithOneOfAnyOfProperties({
      this.oneofProp = const UndefinedWrapper
        .undefined()
,
  this.anyofProp = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ModelWithOneOfAnyOfPropertiesReflection.instance;
  ModelWithOneOfAnyOfPropertiesReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory ModelWithOneOfAnyOfProperties.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  ModelWithOneOfAnyOfProperties clone() {
    return $reflection.clone(this);
  }
}








