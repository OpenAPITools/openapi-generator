// Model reflection

part of 'enum_arrays.dart';


//class reflection

class EnumArraysReflection extends ClassReflection<EnumArrays> {
  static const instance = EnumArraysReflection._(
    justSymbol: PropertyReflection(
      dartName: r'justSymbol',
      nullable: false,
      required: false,
      oasName: r'just_symbol',
      oasType: r'string',
      pattern: null,
    ),
    arrayEnum: PropertyReflection(
      dartName: r'arrayEnum',
      nullable: false,
      required: false,
      oasName: r'array_enum',
      oasType: r'array',
      pattern: null,
    ),
  );
  const EnumArraysReflection._({
    required this.justSymbol,
  
    required this.arrayEnum,
  });

  final PropertyReflection<UndefinedWrapper<
            EnumArraysJustSymbolEnum
>> justSymbol;
  final PropertyReflection<UndefinedWrapper<
    List<
        
            EnumArraysArrayEnumEnum
>
>> arrayEnum;

  @override
  List<PropertyReflection> get members => [
    justSymbol,
arrayEnum,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => EnumArrays.canDeserialize(src);
  @override
  EnumArrays Function(Object? src) get deserializeFunction =>
      (src) => EnumArrays.deserialize(src);

  @override
  Object? Function(EnumArrays src) get serializeFunction =>
      (src) => src.serialize();
}

class EnumArraysXmlReflection {
    const EnumArraysXmlReflection();
}

