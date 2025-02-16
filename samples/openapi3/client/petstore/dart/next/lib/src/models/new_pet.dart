// Model def

import 'package:petstore_api/_internal.dart';


part 'new_pet.reflection.dart';


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
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = NewPetReflection.instance;
  NewPetReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory NewPet.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  NewPet clone() {
    return $reflection.clone(this);
  }
}
























extension type const NewPetStatusEnum._(String value) implements String {
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

  static const $reflection = EnumReflection<NewPetStatusEnum, String>(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'available', oasValue: r'available', value: NewPetStatusEnum.available()),
      
        EnumMemberReflection(dartName: r'pending', oasValue: r'pending', value: NewPetStatusEnum.pending()),
      
        EnumMemberReflection(dartName: r'sold', oasValue: r'sold', value: NewPetStatusEnum.sold()),
      
    ],
  );

  factory NewPetStatusEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
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


