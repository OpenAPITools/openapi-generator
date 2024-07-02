// Model reflection

part of 'enum_string_discriminator.dart';


//class reflection

class EnumStringDiscriminatorReflection extends ClassReflection<EnumStringDiscriminator> {
  static const instance = EnumStringDiscriminatorReflection._(
    enumStrType: PropertyReflection(
      dartName: r'enumStrType',
      nullable: false,
      required: true,
      oasName: r'enum_str_type',
      oasType: r'string',
      pattern: null,
    ),
  );
  const EnumStringDiscriminatorReflection._({
    required this.enumStrType,
  });

  final PropertyReflection<
            EnumStringDiscriminatorEnumStrTypeEnum
> enumStrType;

  @override
  List<PropertyReflection> get members => [
    enumStrType,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => EnumStringDiscriminator.canDeserialize(src);
  @override
  EnumStringDiscriminator Function(Object? src) get deserializeFunction =>
      (src) => EnumStringDiscriminator.deserialize(src);

  @override
  Object? Function(EnumStringDiscriminator src) get serializeFunction =>
      (src) => src.serialize();
}

class EnumStringDiscriminatorXmlReflection {
    const EnumStringDiscriminatorXmlReflection();
}

