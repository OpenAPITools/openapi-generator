// Model def

import 'package:petstore_api/_internal.dart';


part 'pet_with_required_tags.reflection.dart';


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
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = PetWithRequiredTagsReflection.instance;
  PetWithRequiredTagsReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory PetWithRequiredTags.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  PetWithRequiredTags clone() {
    return $reflection.clone(this);
  }
}





















extension type const PetWithRequiredTagsStatusEnum._(String value) implements String {
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

  static const $reflection = EnumReflection<PetWithRequiredTagsStatusEnum, String>(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'available', oasValue: r'available', value: PetWithRequiredTagsStatusEnum.available()),
      
        EnumMemberReflection(dartName: r'pending', oasValue: r'pending', value: PetWithRequiredTagsStatusEnum.pending()),
      
        EnumMemberReflection(dartName: r'sold', oasValue: r'sold', value: PetWithRequiredTagsStatusEnum.sold()),
      
    ],
  );

  factory PetWithRequiredTagsStatusEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
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


