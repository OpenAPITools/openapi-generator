// Model def

import 'package:petstore_api/_internal.dart';


part 'enum_string_discriminator.reflection.dart';


/// An object to test discriminator of enum string
///
/// Properties:
/// * [enumStrType] - enum string type
mixin EnumStringDiscriminatorMixin on
  $OpenApiObjectMixin {
  
            EnumStringDiscriminatorEnumStrTypeEnum
 get enumStrType;
  
}

/// An object to test discriminator of enum string
///
/// Properties:
/// * [enumStrType] - enum string type
class EnumStringDiscriminator with
$OpenApiObjectMixin,

EnumStringDiscriminatorMixin {
  @override
  
            EnumStringDiscriminatorEnumStrTypeEnum
 enumStrType;

  AdditionalProperties<Object
?> additionalProperties;

  

  EnumStringDiscriminator.$all({
        required this.enumStrType,
    required this.additionalProperties,
    
  });

  EnumStringDiscriminator({
    required  this.enumStrType     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = EnumStringDiscriminatorReflection.instance;
  EnumStringDiscriminatorReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory EnumStringDiscriminator.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  EnumStringDiscriminator clone() {
    return $reflection.clone(this);
  }
}


extension type const EnumStringDiscriminatorEnumStrTypeEnum._(String value) implements String {
  /// enum string type
      const EnumStringDiscriminatorEnumStrTypeEnum.typeA() : this._(r'type_a');
  /// enum string type
      const EnumStringDiscriminatorEnumStrTypeEnum.typeB() : this._(r'type_b');

  /// Creates a [EnumStringDiscriminatorEnumStrTypeEnum] enum from a value and safely checking if it exists.
  factory EnumStringDiscriminatorEnumStrTypeEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static const $reflection = EnumReflection<EnumStringDiscriminatorEnumStrTypeEnum, String>(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'typeA', oasValue: r'type_a', value: EnumStringDiscriminatorEnumStrTypeEnum.typeA()),
      
        EnumMemberReflection(dartName: r'typeB', oasValue: r'type_b', value: EnumStringDiscriminatorEnumStrTypeEnum.typeB()),
      
    ],
  );

  factory EnumStringDiscriminatorEnumStrTypeEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
  }

  /// Creates a [EnumStringDiscriminatorEnumStrTypeEnum] enum from a value without checking if it exists.
  const EnumStringDiscriminatorEnumStrTypeEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumStringDiscriminatorEnumStrTypeEnum> values = [
    EnumStringDiscriminatorEnumStrTypeEnum.typeA(),
    EnumStringDiscriminatorEnumStrTypeEnum.typeB(),
    
  ];
}


