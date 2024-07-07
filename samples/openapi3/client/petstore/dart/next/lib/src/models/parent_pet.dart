// Model def

import 'package:petstore_api/_internal.dart';


part 'parent_pet.reflection.dart';
part 'parent_pet.serialization.dart';


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
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = ParentPetReflection.instance;
  ParentPetReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
      
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$ParentPetToMap(this);
  }
  factory ParentPet.fromMap(Map<String, dynamic> src) {
    return _$ParentPetFromMap(src);
  }
  static ParentPet? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ParentPet.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ParentPetCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ParentPet.deserialize(Object? src) {
    return _$ParentPetDeserialize(src);
  }
  static ParentPet? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ParentPet.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ParentPetCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$ParentPetSerialize(this);
  }
}




