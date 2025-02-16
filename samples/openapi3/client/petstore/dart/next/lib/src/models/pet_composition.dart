// Model def

import 'package:petstore_api/_internal.dart';


part 'pet_composition.reflection.dart';


/// PetCompositionMixin
///
/// Properties:
mixin PetCompositionMixin on
  PetMixin, $OpenApiObjectMixin {
    
}

/// PetComposition
///
/// Properties:
/// * [photoUrls] 
/// * [name] 
/// * [id] 
/// * [category] 
/// * [tags] 
/// * [status] - pet status in the store
class PetComposition with
$OpenApiObjectMixin,
PetMixin,
PetCompositionMixin {
  @override
  
    List<
        
            String
>
 photoUrls;
  @override
  
            String
 name;
  @override
  UndefinedWrapper<
            int
> id;
  @override
  UndefinedWrapper<
            Category
> category;
  @override
  UndefinedWrapper<
    List<
        
            Tag
>
> tags;
  @override
  UndefinedWrapper<
            PetStatusEnum
> status;

  AdditionalProperties<Object
?> additionalProperties;

  

  PetComposition.$all({
        required this.photoUrls,
    required this.name,
    required this.id,
    required this.category,
    required this.tags,
    required this.status,
    required this.additionalProperties,
    
  });

  PetComposition({
    required  this.photoUrls     ,
required  this.name     ,
  this.id = const UndefinedWrapper
        .undefined()
,
  this.category = const UndefinedWrapper
        .undefined()
,
  this.tags = const UndefinedWrapper
        .undefined()
,
  this.status = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = PetCompositionReflection.instance;
  PetCompositionReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  factory PetComposition.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  PetComposition clone() {
    return $reflection.clone(this);
  }
}


