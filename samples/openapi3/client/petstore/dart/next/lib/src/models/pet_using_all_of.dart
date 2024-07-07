// Model def

import 'package:petstore_api/_internal.dart';


part 'pet_using_all_of.reflection.dart';
part 'pet_using_all_of.serialization.dart';


/// PetUsingAllOfMixin
///
/// Properties:
/// * [id] 
/// * [category] - multi line description 2nd line last line 
/// * [name] 
/// * [photoUrls] 
/// * [tags] 
/// * [status] - pet status in the store
mixin PetUsingAllOfMixin on
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
            PetUsingAllOfStatusEnum
> get status;
  
}

/// PetUsingAllOf
///
/// Properties:
/// * [id] 
/// * [category] - multi line description 2nd line last line 
/// * [name] 
/// * [photoUrls] 
/// * [tags] 
/// * [status] - pet status in the store
class PetUsingAllOf with
$OpenApiObjectMixin,

PetUsingAllOfMixin {
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
            PetUsingAllOfStatusEnum
> status;

  AdditionalProperties<Object
?> additionalProperties;

  

  PetUsingAllOf.$all({
        required this.id,
    required this.category,
    required this.name,
    required this.photoUrls,
    required this.tags,
    required this.status,
    required this.additionalProperties,
    
  });

  PetUsingAllOf({
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
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = PetUsingAllOfReflection.instance;
  PetUsingAllOfReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$PetUsingAllOfToMap(this);
  }
  factory PetUsingAllOf.fromMap(Map<String, dynamic> src) {
    return _$PetUsingAllOfFromMap(src);
  }
  static PetUsingAllOf? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return PetUsingAllOf.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$PetUsingAllOfCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory PetUsingAllOf.deserialize(Object? src) {
    return _$PetUsingAllOfDeserialize(src);
  }
  static PetUsingAllOf? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return PetUsingAllOf.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$PetUsingAllOfCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$PetUsingAllOfSerialize(this);
  }
}




extension type const PetUsingAllOfStatusEnum._(String value) {
  /// pet status in the store
      const PetUsingAllOfStatusEnum.available() : this._(r'available');
  /// pet status in the store
      const PetUsingAllOfStatusEnum.pending() : this._(r'pending');
  /// pet status in the store
      const PetUsingAllOfStatusEnum.sold() : this._(r'sold');

  /// Creates a [PetUsingAllOfStatusEnum] enum from a value and safely checking if it exists.
  factory PetUsingAllOfStatusEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static bool canDeserialize(Object? value) {
    return value is String && values.where((element) => element.value == value).firstOrNull != null;
  }

  /// Creates a [PetUsingAllOfStatusEnum] enum from a value without checking if it exists.
  const PetUsingAllOfStatusEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<PetUsingAllOfStatusEnum> values = [
    PetUsingAllOfStatusEnum.available(),
    PetUsingAllOfStatusEnum.pending(),
    PetUsingAllOfStatusEnum.sold(),
    
  ];
}

