// Model def

import 'package:petstore_api/_internal.dart';


part 'new_pet_category_inline_allof.reflection.dart';


/// NewPetCategoryInlineAllofMixin
///
/// Properties:
/// * [id] 
/// * [name] 
/// * [categoryTag] 
mixin NewPetCategoryInlineAllofMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
> get id;

            String
 get name;
UndefinedWrapper<
            NewPetCategoryInlineAllofAllOfCategoryTag
> get categoryTag;
  
}

/// NewPetCategoryInlineAllof
///
/// Properties:
/// * [id] 
/// * [name] 
/// * [categoryTag] 
class NewPetCategoryInlineAllof with
$OpenApiObjectMixin,

NewPetCategoryInlineAllofMixin {
  @override
  UndefinedWrapper<
            int
> id;
  @override
  
            String
 name;
  @override
  UndefinedWrapper<
            NewPetCategoryInlineAllofAllOfCategoryTag
> categoryTag;

  AdditionalProperties<Object
?> additionalProperties;

  

  NewPetCategoryInlineAllof.$all({
        required this.id,
    required this.name,
    required this.categoryTag,
    required this.additionalProperties,
    
  });

  NewPetCategoryInlineAllof({
      this.id = const UndefinedWrapper
        .undefined()
,
  this.name     =
        
        'default-name'
        
,
  this.categoryTag = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = NewPetCategoryInlineAllofReflection.instance;
  NewPetCategoryInlineAllofReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  factory NewPetCategoryInlineAllof.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  NewPetCategoryInlineAllof clone() {
    return $reflection.clone(this);
  }
}











