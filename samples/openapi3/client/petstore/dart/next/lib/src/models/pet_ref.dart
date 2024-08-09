// Model def

import 'package:petstore_api/_internal.dart';


part 'pet_ref.reflection.dart';
part 'pet_ref.serialization.dart';


/// PetRefMixin
///
/// Properties:
/// * [id] 
/// * [category] 
/// * [name] 
/// * [photoUrls] 
/// * [tags] 
/// * [status] - pet status in the store
mixin PetRefMixin on
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
            PetRefStatusEnum

> get status;
  
}

/// PetRef
///
/// Properties:
/// * [id] 
/// * [category] 
/// * [name] 
/// * [photoUrls] 
/// * [tags] 
/// * [status] - pet status in the store
class PetRef with
$OpenApiObjectMixin,

PetRefMixin {
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
            PetRefStatusEnum

> status;

  AdditionalProperties<Object

?> additionalProperties;

  

  PetRef.$all({
        required this.id,
    required this.category,
    required this.name,
    required this.photoUrls,
    required this.tags,
    required this.status,
    required this.additionalProperties,
    
  });

  PetRef({
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

  static const $reflection = PetRefReflection.instance;
  PetRefReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$PetRefToMap(this);
  }
  factory PetRef.fromMap(Map<String, dynamic> src) {
    return _$PetRefFromMap(src);
  }
  static PetRef? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return PetRef.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$PetRefCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory PetRef.deserialize(Object? src) {
    return _$PetRefDeserialize(src);
  }
  static PetRef? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return PetRef.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$PetRefCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$PetRefSerialize(this);
  }
}




extension type const PetRefStatusEnum._(String value) {
  /// pet status in the store
      const PetRefStatusEnum.available() : this._(r'available');
  /// pet status in the store
      const PetRefStatusEnum.pending() : this._(r'pending');
  /// pet status in the store
      const PetRefStatusEnum.sold() : this._(r'sold');

  /// Creates a [PetRefStatusEnum] enum from a value and safely checking if it exists.
  factory PetRefStatusEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static bool canDeserialize(Object? value) {
    return value is String && values.where((element) => element.value == value).firstOrNull != null;
  }

  /// Creates a [PetRefStatusEnum] enum from a value without checking if it exists.
  const PetRefStatusEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<PetRefStatusEnum> values = [
    PetRefStatusEnum.available(),
    PetRefStatusEnum.pending(),
    PetRefStatusEnum.sold(),
    
  ];
}

