// Model def

import 'package:openapi/_internal.dart';


part 'pet.reflection.dart';
part 'pet.serialization.dart';


/// PetMixin
///
/// Properties:
/// * [id] 
/// * [category] 
/// * [name] 
/// * [photoUrls] 
/// * [tags] 
/// * [status] - pet status in the store
mixin PetMixin on 
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
> get id;
UndefinedWrapper<
            Category
> get category;

            String
 get name;

    List<
        
            String
>
 get photoUrls;
UndefinedWrapper<
    List<
        
            Tag
>
> get tags;
UndefinedWrapper<
            PetStatusEnum
> get status;
  
}

/// Pet
///
/// Properties:
/// * [id] 
/// * [category] 
/// * [name] 
/// * [photoUrls] 
/// * [tags] 
/// * [status] - pet status in the store
class Pet with
$OpenApiObjectMixin,


PetMixin {
  @override
  UndefinedWrapper<
            int
> id;
  @override
  UndefinedWrapper<
            Category
> category;
  @override
  
            String
 name;
  @override
  
    List<
        
            String
>
 photoUrls;
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

  

  

  Pet.$all({
        required this.id,
    required this.category,
    required this.name,
    required this.photoUrls,
    required this.tags,
    required this.status,
    
    
  });

  Pet({
      this.id = const UndefinedWrapper
        .undefined()
,
  this.category = const UndefinedWrapper
        .undefined()
,
required  this.name     ,
required  this.photoUrls     ,
  this.tags = const UndefinedWrapper
        .undefined()
,
  this.status = const UndefinedWrapper
        .undefined()
,
    
    
  });

  static const $reflection = PetReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$PetToMap(this);
  }
  factory Pet.fromMap(Map<String, dynamic> src) {
    return _$PetFromMap(src);
  }
  static Pet? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Pet.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$PetCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Pet.deserialize(Object? src) {
    return _$PetDeserialize(src);
  }
  static Pet? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Pet.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$PetCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$PetSerialize(this);
  }
}




extension type const PetStatusEnum._(String value) {
  /// pet status in the store
      const PetStatusEnum.available() : this._(r'available');
  /// pet status in the store
      const PetStatusEnum.pending() : this._(r'pending');
  /// pet status in the store
      const PetStatusEnum.sold() : this._(r'sold');

  /// Creates a [PetStatusEnum] enum from a value and safely checking if it exists.
  factory PetStatusEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [PetStatusEnum] enum from a value without checking if it exists.
  const PetStatusEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<PetStatusEnum> values = [
    PetStatusEnum.available(),
    PetStatusEnum.pending(),
    PetStatusEnum.sold(),
    
  ];
}

