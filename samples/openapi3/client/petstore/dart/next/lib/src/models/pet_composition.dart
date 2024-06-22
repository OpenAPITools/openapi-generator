// Model def

import 'package:openapi/_internal.dart';


part 'pet_composition.reflection.dart';
part 'pet_composition.serialization.dart';


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

  

  

  PetComposition.$all({
        required this.photoUrls,
    required this.name,
    required this.id,
    required this.category,
    required this.tags,
    required this.status,
    
    
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
    
    
  });

  static const $reflection = PetCompositionReflection.instance;

  @override
  bool validate() {
      
      
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$PetCompositionToMap(this);
  }
  factory PetComposition.fromMap(Map<String, dynamic> src) {
    return _$PetCompositionFromMap(src);
  }
  static PetComposition? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return PetComposition.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$PetCompositionCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory PetComposition.deserialize(Object? src) {
    return _$PetCompositionDeserialize(src);
  }
  static PetComposition? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return PetComposition.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$PetCompositionCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$PetCompositionSerialize(this);
  }
}




