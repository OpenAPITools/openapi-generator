// Model def

import 'package:petstore_api/_internal.dart';


part 'array_of_inline_all_of.reflection.dart';


/// ArrayOfInlineAllOfMixin
///
/// Properties:
/// * [id] 
/// * [name] 
/// * [arrayAllofDogProperty] 
mixin ArrayOfInlineAllOfMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
> get id;

            String
 get name;
UndefinedWrapper<
    List<
        
            ArrayOfInlineAllOfArrayAllofDogPropertyInner
>
> get arrayAllofDogProperty;
  
}

/// ArrayOfInlineAllOf
///
/// Properties:
/// * [id] 
/// * [name] 
/// * [arrayAllofDogProperty] 
class ArrayOfInlineAllOf with
$OpenApiObjectMixin,

ArrayOfInlineAllOfMixin {
  @override
  UndefinedWrapper<
            int
> id;
  @override
  
            String
 name;
  @override
  UndefinedWrapper<
    List<
        
            ArrayOfInlineAllOfArrayAllofDogPropertyInner
>
> arrayAllofDogProperty;

  AdditionalProperties<Object
?> additionalProperties;

  

  ArrayOfInlineAllOf.$all({
        required this.id,
    required this.name,
    required this.arrayAllofDogProperty,
    required this.additionalProperties,
    
  });

  ArrayOfInlineAllOf({
      this.id = const UndefinedWrapper
        .undefined()
,
required  this.name     ,
  this.arrayAllofDogProperty = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ArrayOfInlineAllOfReflection.instance;
  ArrayOfInlineAllOfReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory ArrayOfInlineAllOf.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  ArrayOfInlineAllOf clone() {
    return $reflection.clone(this);
  }
}













