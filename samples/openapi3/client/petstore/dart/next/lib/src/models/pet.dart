// Model def

import 'package:petstore_api/_internal.dart';


part 'pet.reflection.dart';


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

  AdditionalProperties<Object
?> additionalProperties;

  

  Pet.$all({
        required this.id,
    required this.category,
    required this.name,
    required this.photoUrls,
    required this.tags,
    required this.status,
    required this.additionalProperties,
    
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
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = PetReflection.instance;
  PetReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory Pet.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Pet clone() {
    return $reflection.clone(this);
  }
}





















extension type const PetStatusEnum._(String value) implements String {
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

  static const $reflection = EnumReflection<PetStatusEnum, String>(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'available', oasValue: r'available', value: PetStatusEnum.available()),
      
        EnumMemberReflection(dartName: r'pending', oasValue: r'pending', value: PetStatusEnum.pending()),
      
        EnumMemberReflection(dartName: r'sold', oasValue: r'sold', value: PetStatusEnum.sold()),
      
    ],
  );

  factory PetStatusEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
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


