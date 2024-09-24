// Model def

import 'package:petstore_api/_internal.dart';


part 'array_of_inline_all_of_array_allof_dog_property_inner.reflection.dart';


/// ArrayOfInlineAllOfArrayAllofDogPropertyInnerMixin
///
/// Properties:
/// * [breed] 
/// * [color] 
mixin ArrayOfInlineAllOfArrayAllofDogPropertyInnerMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get breed;
UndefinedWrapper<
            String
> get color;
  
}

/// ArrayOfInlineAllOfArrayAllofDogPropertyInner
///
/// Properties:
/// * [breed] 
/// * [color] 
class ArrayOfInlineAllOfArrayAllofDogPropertyInner with
$OpenApiObjectMixin,

ArrayOfInlineAllOfArrayAllofDogPropertyInnerMixin {
  @override
  UndefinedWrapper<
            String
> breed;
  @override
  UndefinedWrapper<
            String
> color;

  AdditionalProperties<Object
?> additionalProperties;

  

  ArrayOfInlineAllOfArrayAllofDogPropertyInner.$all({
        required this.breed,
    required this.color,
    required this.additionalProperties,
    
  });

  ArrayOfInlineAllOfArrayAllofDogPropertyInner({
      this.breed = const UndefinedWrapper
        .undefined()
,
  this.color = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection.instance;
  ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  factory ArrayOfInlineAllOfArrayAllofDogPropertyInner.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  ArrayOfInlineAllOfArrayAllofDogPropertyInner clone() {
    return $reflection.clone(this);
  }
}








