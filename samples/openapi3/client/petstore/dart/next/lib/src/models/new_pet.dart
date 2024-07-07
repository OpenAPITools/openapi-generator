// Model def

import 'package:petstore_api/_internal.dart';


part 'new_pet.reflection.dart';
part 'new_pet.serialization.dart';


/// NewPetMixin
///
/// Properties:
/// * [id] 
/// * [categoryInlineAllof] 
/// * [categoryAllOfRef] 
/// * [name] 
/// * [photoUrls] 
/// * [tags] 
/// * [status] - pet status in the store
mixin NewPetMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
> get id;
UndefinedWrapper<
            NewPetCategoryInlineAllof
> get categoryInlineAllof;
UndefinedWrapper<
            Category
> get categoryAllOfRef;

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
            NewPetStatusEnum
> get status;
  
}

/// NewPet
///
/// Properties:
/// * [id] 
/// * [categoryInlineAllof] 
/// * [categoryAllOfRef] 
/// * [name] 
/// * [photoUrls] 
/// * [tags] 
/// * [status] - pet status in the store
class NewPet with
$OpenApiObjectMixin,

NewPetMixin {
  @override
  UndefinedWrapper<
            int
> id;
  @override
  UndefinedWrapper<
            NewPetCategoryInlineAllof
> categoryInlineAllof;
  @override
  UndefinedWrapper<
            Category
> categoryAllOfRef;
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
            NewPetStatusEnum
> status;

  AdditionalProperties<Object
?> additionalProperties;

  

  NewPet.$all({
        required this.id,
    required this.categoryInlineAllof,
    required this.categoryAllOfRef,
    required this.name,
    required this.photoUrls,
    required this.tags,
    required this.status,
    required this.additionalProperties,
    
  });

  NewPet({
      this.id = const UndefinedWrapper
        .undefined()
,
  this.categoryInlineAllof = const UndefinedWrapper
        .undefined()
,
  this.categoryAllOfRef = const UndefinedWrapper
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
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = NewPetReflection.instance;
  NewPetReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$NewPetToMap(this);
  }
  factory NewPet.fromMap(Map<String, dynamic> src) {
    return _$NewPetFromMap(src);
  }
  static NewPet? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return NewPet.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$NewPetCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory NewPet.deserialize(Object? src) {
    return _$NewPetDeserialize(src);
  }
  static NewPet? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return NewPet.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$NewPetCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$NewPetSerialize(this);
  }
}




extension type const NewPetStatusEnum._(String value) {
  /// pet status in the store
      const NewPetStatusEnum.available() : this._(r'available');
  /// pet status in the store
      const NewPetStatusEnum.pending() : this._(r'pending');
  /// pet status in the store
      const NewPetStatusEnum.sold() : this._(r'sold');

  /// Creates a [NewPetStatusEnum] enum from a value and safely checking if it exists.
  factory NewPetStatusEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static bool canDeserialize(Object? value) {
    return value is String && values.where((element) => element.value == value).firstOrNull != null;
  }

  /// Creates a [NewPetStatusEnum] enum from a value without checking if it exists.
  const NewPetStatusEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<NewPetStatusEnum> values = [
    NewPetStatusEnum.available(),
    NewPetStatusEnum.pending(),
    NewPetStatusEnum.sold(),
    
  ];
}

