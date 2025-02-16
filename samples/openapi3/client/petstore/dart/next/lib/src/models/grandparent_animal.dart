// Model def

import 'package:petstore_api/_internal.dart';


part 'grandparent_animal.reflection.dart';


/// GrandparentAnimalMixin
///
/// Properties:
/// * [petType] 
mixin GrandparentAnimalMixin on
  $OpenApiObjectMixin {
  
            String
 get petType;
  
}

/// GrandparentAnimal
///
/// Properties:
/// * [petType] 
class GrandparentAnimal with
$OpenApiObjectMixin,

GrandparentAnimalMixin {
  @override
  
            String
 petType;

  AdditionalProperties<Object
?> additionalProperties;

  

  GrandparentAnimal.$all({
        required this.petType,
    required this.additionalProperties,
    
  });

  GrandparentAnimal({
    required  this.petType     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = GrandparentAnimalReflection.instance;
  GrandparentAnimalReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory GrandparentAnimal.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  GrandparentAnimal clone() {
    return $reflection.clone(this);
  }
}





