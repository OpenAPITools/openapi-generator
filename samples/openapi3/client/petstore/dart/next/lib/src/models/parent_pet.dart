// Model def

import 'package:petstore_api/_internal.dart';


part 'parent_pet.reflection.dart';


/// ParentPetMixin
mixin ParentPetMixin on
  GrandparentAnimalMixin, $OpenApiObjectMixin {
    
}

/// ParentPet
class ParentPet with
$OpenApiObjectMixin,
GrandparentAnimalMixin,
ParentPetMixin {
  @override
  
            String
 petType;

  AdditionalProperties<Object
?> additionalProperties;

  

  ParentPet.$all({
        required this.petType,
    required this.additionalProperties,
    
  });

  ParentPet({
    required  this.petType     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ParentPetReflection.instance;
  ParentPetReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  factory ParentPet.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  ParentPet clone() {
    return $reflection.clone(this);
  }
}


