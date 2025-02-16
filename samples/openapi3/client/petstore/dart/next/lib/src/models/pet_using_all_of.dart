// Model def

import 'package:petstore_api/_internal.dart';


part 'pet_using_all_of.reflection.dart';


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
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = PetUsingAllOfReflection.instance;
  PetUsingAllOfReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory PetUsingAllOf.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  PetUsingAllOf clone() {
    return $reflection.clone(this);
  }
}





















extension type const PetUsingAllOfStatusEnum._(String value) implements String {
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

  static const $reflection = EnumReflection<PetUsingAllOfStatusEnum, String>(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'available', oasValue: r'available', value: PetUsingAllOfStatusEnum.available()),
      
        EnumMemberReflection(dartName: r'pending', oasValue: r'pending', value: PetUsingAllOfStatusEnum.pending()),
      
        EnumMemberReflection(dartName: r'sold', oasValue: r'sold', value: PetUsingAllOfStatusEnum.sold()),
      
    ],
  );

  factory PetUsingAllOfStatusEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
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


