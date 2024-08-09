// Model def

import 'package:petstore_api/_internal.dart';


part 'pet_with_required_tags.reflection.dart';
part 'pet_with_required_tags.serialization.dart';


/// PetWithRequiredTagsMixin
///
/// Properties:
/// * [id] 
/// * [category] 
/// * [name] 
/// * [photoUrls] 
/// * [tags] 
/// * [status] - pet status in the store
mixin PetWithRequiredTagsMixin on
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

    List<
        
            Tag

>

 get tags;
UndefinedWrapper<
            PetWithRequiredTagsStatusEnum

> get status;
  
}

/// PetWithRequiredTags
///
/// Properties:
/// * [id] 
/// * [category] 
/// * [name] 
/// * [photoUrls] 
/// * [tags] 
/// * [status] - pet status in the store
class PetWithRequiredTags with
$OpenApiObjectMixin,

PetWithRequiredTagsMixin {
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
  
    List<
        
            Tag

>

 tags;
  @override
  UndefinedWrapper<
            PetWithRequiredTagsStatusEnum

> status;

  AdditionalProperties<Object

?> additionalProperties;

  

  PetWithRequiredTags.$all({
        required this.id,
    required this.category,
    required this.name,
    required this.photoUrls,
    required this.tags,
    required this.status,
    required this.additionalProperties,
    
  });

  PetWithRequiredTags({
      this.id = const UndefinedWrapper
        .undefined()
,
  this.category = const UndefinedWrapper
        .undefined()
,
required  this.name     ,
required  this.photoUrls     ,
required  this.tags     ,
  this.status = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = PetWithRequiredTagsReflection.instance;
  PetWithRequiredTagsReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$PetWithRequiredTagsToMap(this);
  }
  factory PetWithRequiredTags.fromMap(Map<String, dynamic> src) {
    return _$PetWithRequiredTagsFromMap(src);
  }
  static PetWithRequiredTags? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return PetWithRequiredTags.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$PetWithRequiredTagsCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory PetWithRequiredTags.deserialize(Object? src) {
    return _$PetWithRequiredTagsDeserialize(src);
  }
  static PetWithRequiredTags? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return PetWithRequiredTags.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$PetWithRequiredTagsCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$PetWithRequiredTagsSerialize(this);
  }
}




extension type const PetWithRequiredTagsStatusEnum._(String value) {
  /// pet status in the store
      const PetWithRequiredTagsStatusEnum.available() : this._(r'available');
  /// pet status in the store
      const PetWithRequiredTagsStatusEnum.pending() : this._(r'pending');
  /// pet status in the store
      const PetWithRequiredTagsStatusEnum.sold() : this._(r'sold');

  /// Creates a [PetWithRequiredTagsStatusEnum] enum from a value and safely checking if it exists.
  factory PetWithRequiredTagsStatusEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static bool canDeserialize(Object? value) {
    return value is String && values.where((element) => element.value == value).firstOrNull != null;
  }

  /// Creates a [PetWithRequiredTagsStatusEnum] enum from a value without checking if it exists.
  const PetWithRequiredTagsStatusEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<PetWithRequiredTagsStatusEnum> values = [
    PetWithRequiredTagsStatusEnum.available(),
    PetWithRequiredTagsStatusEnum.pending(),
    PetWithRequiredTagsStatusEnum.sold(),
    
  ];
}

