// Model def

import 'package:petstore_api/_internal.dart';


part 'pet_ref.reflection.dart';


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
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = PetRefReflection.instance;
  PetRefReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory PetRef.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  PetRef clone() {
    return $reflection.clone(this);
  }
}





















extension type const PetRefStatusEnum._(String value) implements String {
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

  static const $reflection = EnumReflection<PetRefStatusEnum, String>(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'available', oasValue: r'available', value: PetRefStatusEnum.available()),
      
        EnumMemberReflection(dartName: r'pending', oasValue: r'pending', value: PetRefStatusEnum.pending()),
      
        EnumMemberReflection(dartName: r'sold', oasValue: r'sold', value: PetRefStatusEnum.sold()),
      
    ],
  );

  factory PetRefStatusEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
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


