// Model def

import 'package:petstore_api/_internal.dart';


part 'grandparent_animal.reflection.dart';
part 'grandparent_animal.serialization.dart';


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
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = GrandparentAnimalReflection.instance;
  GrandparentAnimalReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$GrandparentAnimalToMap(this);
  }
  factory GrandparentAnimal.fromMap(Map<String, dynamic> src) {
    return _$GrandparentAnimalFromMap(src);
  }
  static GrandparentAnimal? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return GrandparentAnimal.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$GrandparentAnimalCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory GrandparentAnimal.deserialize(Object? src) {
    return _$GrandparentAnimalDeserialize(src);
  }
  static GrandparentAnimal? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return GrandparentAnimal.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$GrandparentAnimalCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$GrandparentAnimalSerialize(this);
  }
}




