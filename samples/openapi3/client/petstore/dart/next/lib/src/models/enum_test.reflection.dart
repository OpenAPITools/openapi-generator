// Model reflection

part of 'enum_test.dart';


//class reflection

class EnumTestReflection extends ClassReflection<EnumTest> {
  static const instance = EnumTestReflection._(
    enumString: PropertyReflection(
      dartName: r'enumString',
      nullable: false,
      required: false,
      oasName: r'enum_string',
      oasType: r'string',
      pattern: null,
    ),
    enumStringRequired: PropertyReflection(
      dartName: r'enumStringRequired',
      nullable: false,
      required: true,
      oasName: r'enum_string_required',
      oasType: r'string',
      pattern: null,
    ),
    enumInteger: PropertyReflection(
      dartName: r'enumInteger',
      nullable: false,
      required: false,
      oasName: r'enum_integer',
      oasType: r'integer',
      pattern: null,
    ),
    enumIntegerOnly: PropertyReflection(
      dartName: r'enumIntegerOnly',
      nullable: false,
      required: false,
      oasName: r'enum_integer_only',
      oasType: r'integer',
      pattern: null,
    ),
    enumNumber: PropertyReflection(
      dartName: r'enumNumber',
      nullable: false,
      required: false,
      oasName: r'enum_number',
      oasType: r'number',
      pattern: null,
    ),
    outerEnum: PropertyReflection(
      dartName: r'outerEnum',
      nullable: true,
      required: false,
      oasName: r'outerEnum',
      oasType: r'OuterEnum',
      pattern: null,
    ),
    outerEnumInteger: PropertyReflection(
      dartName: r'outerEnumInteger',
      nullable: false,
      required: false,
      oasName: r'outerEnumInteger',
      oasType: r'OuterEnumInteger',
      pattern: null,
    ),
    outerEnumDefaultValue: PropertyReflection(
      dartName: r'outerEnumDefaultValue',
      nullable: false,
      required: false,
      oasName: r'outerEnumDefaultValue',
      oasType: r'OuterEnumDefaultValue',
      pattern: null,
    ),
    outerEnumIntegerDefaultValue: PropertyReflection(
      dartName: r'outerEnumIntegerDefaultValue',
      nullable: false,
      required: false,
      oasName: r'outerEnumIntegerDefaultValue',
      oasType: r'OuterEnumIntegerDefaultValue',
      pattern: null,
    ),
  );
  const EnumTestReflection._({
    required this.enumString,
  
    required this.enumStringRequired,
  
    required this.enumInteger,
  
    required this.enumIntegerOnly,
  
    required this.enumNumber,
  
    required this.outerEnum,
  
    required this.outerEnumInteger,
  
    required this.outerEnumDefaultValue,
  
    required this.outerEnumIntegerDefaultValue,
  });

  final PropertyReflection<UndefinedWrapper<
            EnumTestEnumStringEnum
>> enumString;
  final PropertyReflection<
            EnumTestEnumStringRequiredEnum
> enumStringRequired;
  final PropertyReflection<UndefinedWrapper<
            EnumTestEnumIntegerEnum
>> enumInteger;
  final PropertyReflection<UndefinedWrapper<
            EnumTestEnumIntegerOnlyEnum
>> enumIntegerOnly;
  final PropertyReflection<UndefinedWrapper<
            EnumTestEnumNumberEnum
>> enumNumber;
  final PropertyReflection<UndefinedWrapper<
            OuterEnum
?>> outerEnum;
  final PropertyReflection<UndefinedWrapper<
            OuterEnumInteger
>> outerEnumInteger;
  final PropertyReflection<UndefinedWrapper<
            OuterEnumDefaultValue
>> outerEnumDefaultValue;
  final PropertyReflection<UndefinedWrapper<
            OuterEnumIntegerDefaultValue
>> outerEnumIntegerDefaultValue;

  @override
  List<PropertyReflection> get members => [
    enumString,
enumStringRequired,
enumInteger,
enumIntegerOnly,
enumNumber,
outerEnum,
outerEnumInteger,
outerEnumDefaultValue,
outerEnumIntegerDefaultValue,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => EnumTest.canDeserialize(src);
  @override
  EnumTest Function(Object? src) get deserializeFunction =>
      (src) => EnumTest.deserialize(src);

  @override
  Object? Function(EnumTest src) get serializeFunction =>
      (src) => src.serialize();
}

class EnumTestXmlReflection {
    const EnumTestXmlReflection();
}

