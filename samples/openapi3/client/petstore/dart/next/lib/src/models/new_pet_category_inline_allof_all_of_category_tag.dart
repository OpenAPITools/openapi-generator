// Model def

import 'package:petstore_api/_internal.dart';


part 'new_pet_category_inline_allof_all_of_category_tag.reflection.dart';


/// NewPetCategoryInlineAllofAllOfCategoryTagMixin
///
/// Properties:
/// * [id] 
/// * [name] 
mixin NewPetCategoryInlineAllofAllOfCategoryTagMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
> get id;
UndefinedWrapper<
            String
> get name;
  
}

/// NewPetCategoryInlineAllofAllOfCategoryTag
///
/// Properties:
/// * [id] 
/// * [name] 
class NewPetCategoryInlineAllofAllOfCategoryTag with
$OpenApiObjectMixin,

NewPetCategoryInlineAllofAllOfCategoryTagMixin {
  @override
  UndefinedWrapper<
            int
> id;
  @override
  UndefinedWrapper<
            String
> name;

  AdditionalProperties<Object
?> additionalProperties;

  

  NewPetCategoryInlineAllofAllOfCategoryTag.$all({
        required this.id,
    required this.name,
    required this.additionalProperties,
    
  });

  NewPetCategoryInlineAllofAllOfCategoryTag({
      this.id = const UndefinedWrapper
        .undefined()
,
  this.name = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = NewPetCategoryInlineAllofAllOfCategoryTagReflection.instance;
  NewPetCategoryInlineAllofAllOfCategoryTagReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory NewPetCategoryInlineAllofAllOfCategoryTag.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  NewPetCategoryInlineAllofAllOfCategoryTag clone() {
    return $reflection.clone(this);
  }
}








