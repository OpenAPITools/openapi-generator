// Model reflection

part of 'enum_test.dart';


//class reflection

class EnumTestReflection extends ClassReflection<EnumTest> {
  static EnumTestReflection instanceGetter() => instance;
  static const instance = EnumTestReflection._(
    modelName: r'Enum_Test',
    className: r'EnumTest',
    enumStringPart: PropertyReflection<EnumTest, UndefinedWrapper<
            EnumTestEnumStringEnum

>>(
      dartName: r'enumString',
      nullable: false,
      required: false,
      oasName: r'enum_string',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _enumStringGetter,
      setter: _enumStringSetter,
    ),
    enumStringRequiredPart: PropertyReflection<EnumTest, 
            EnumTestEnumStringRequiredEnum

>(
      dartName: r'enumStringRequired',
      nullable: false,
      required: true,
      oasName: r'enum_string_required',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _enumStringRequiredGetter,
      setter: _enumStringRequiredSetter,
    ),
    enumIntegerPart: PropertyReflection<EnumTest, UndefinedWrapper<
            EnumTestEnumIntegerEnum

>>(
      dartName: r'enumInteger',
      nullable: false,
      required: false,
      oasName: r'enum_integer',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _enumIntegerGetter,
      setter: _enumIntegerSetter,
    ),
    enumIntegerOnlyPart: PropertyReflection<EnumTest, UndefinedWrapper<
            EnumTestEnumIntegerOnlyEnum

>>(
      dartName: r'enumIntegerOnly',
      nullable: false,
      required: false,
      oasName: r'enum_integer_only',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _enumIntegerOnlyGetter,
      setter: _enumIntegerOnlySetter,
    ),
    enumNumberPart: PropertyReflection<EnumTest, UndefinedWrapper<
            EnumTestEnumNumberEnum

>>(
      dartName: r'enumNumber',
      nullable: false,
      required: false,
      oasName: r'enum_number',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _enumNumberGetter,
      setter: _enumNumberSetter,
    ),
    outerEnumPart: PropertyReflection<EnumTest, UndefinedWrapper<
            OuterEnum

?>>(
      dartName: r'outerEnum',
      nullable: true,
      required: false,
      oasName: r'outerEnum',
      oasType: r'OuterEnum',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _outerEnumGetter,
      setter: _outerEnumSetter,
    ),
    outerEnumIntegerPart: PropertyReflection<EnumTest, UndefinedWrapper<
            OuterEnumInteger

>>(
      dartName: r'outerEnumInteger',
      nullable: false,
      required: false,
      oasName: r'outerEnumInteger',
      oasType: r'OuterEnumInteger',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _outerEnumIntegerGetter,
      setter: _outerEnumIntegerSetter,
    ),
    outerEnumDefaultValuePart: PropertyReflection<EnumTest, UndefinedWrapper<
            OuterEnumDefaultValue

>>(
      dartName: r'outerEnumDefaultValue',
      nullable: false,
      required: false,
      oasName: r'outerEnumDefaultValue',
      oasType: r'OuterEnumDefaultValue',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _outerEnumDefaultValueGetter,
      setter: _outerEnumDefaultValueSetter,
    ),
    outerEnumIntegerDefaultValuePart: PropertyReflection<EnumTest, UndefinedWrapper<
            OuterEnumIntegerDefaultValue

>>(
      dartName: r'outerEnumIntegerDefaultValue',
      nullable: false,
      required: false,
      oasName: r'outerEnumIntegerDefaultValue',
      oasType: r'OuterEnumIntegerDefaultValue',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _outerEnumIntegerDefaultValueGetter,
      setter: _outerEnumIntegerDefaultValueSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<EnumTest, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const EnumTestReflection._({
    required this.modelName,
    required this.className,
    required this.enumStringPart,
    required this.enumStringRequiredPart,
    required this.enumIntegerPart,
    required this.enumIntegerOnlyPart,
    required this.enumNumberPart,
    required this.outerEnumPart,
    required this.outerEnumIntegerPart,
    required this.outerEnumDefaultValuePart,
    required this.outerEnumIntegerDefaultValuePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<EnumTest, UndefinedWrapper<
            EnumTestEnumStringEnum

>> enumStringPart;
  static UndefinedWrapper<
            EnumTestEnumStringEnum

> _enumStringGetter(EnumTest parent) {
    return parent.enumString;
  }
  static void _enumStringSetter(EnumTest parent, UndefinedWrapper<
            EnumTestEnumStringEnum

> value) {
    parent.enumString = value;
  }
  final PropertyReflection<EnumTest, 
            EnumTestEnumStringRequiredEnum

> enumStringRequiredPart;
  static 
            EnumTestEnumStringRequiredEnum

 _enumStringRequiredGetter(EnumTest parent) {
    return parent.enumStringRequired;
  }
  static void _enumStringRequiredSetter(EnumTest parent, 
            EnumTestEnumStringRequiredEnum

 value) {
    parent.enumStringRequired = value;
  }
  final PropertyReflection<EnumTest, UndefinedWrapper<
            EnumTestEnumIntegerEnum

>> enumIntegerPart;
  static UndefinedWrapper<
            EnumTestEnumIntegerEnum

> _enumIntegerGetter(EnumTest parent) {
    return parent.enumInteger;
  }
  static void _enumIntegerSetter(EnumTest parent, UndefinedWrapper<
            EnumTestEnumIntegerEnum

> value) {
    parent.enumInteger = value;
  }
  final PropertyReflection<EnumTest, UndefinedWrapper<
            EnumTestEnumIntegerOnlyEnum

>> enumIntegerOnlyPart;
  static UndefinedWrapper<
            EnumTestEnumIntegerOnlyEnum

> _enumIntegerOnlyGetter(EnumTest parent) {
    return parent.enumIntegerOnly;
  }
  static void _enumIntegerOnlySetter(EnumTest parent, UndefinedWrapper<
            EnumTestEnumIntegerOnlyEnum

> value) {
    parent.enumIntegerOnly = value;
  }
  final PropertyReflection<EnumTest, UndefinedWrapper<
            EnumTestEnumNumberEnum

>> enumNumberPart;
  static UndefinedWrapper<
            EnumTestEnumNumberEnum

> _enumNumberGetter(EnumTest parent) {
    return parent.enumNumber;
  }
  static void _enumNumberSetter(EnumTest parent, UndefinedWrapper<
            EnumTestEnumNumberEnum

> value) {
    parent.enumNumber = value;
  }
  final PropertyReflection<EnumTest, UndefinedWrapper<
            OuterEnum

?>> outerEnumPart;
  static UndefinedWrapper<
            OuterEnum

?> _outerEnumGetter(EnumTest parent) {
    return parent.outerEnum;
  }
  static void _outerEnumSetter(EnumTest parent, UndefinedWrapper<
            OuterEnum

?> value) {
    parent.outerEnum = value;
  }
  final PropertyReflection<EnumTest, UndefinedWrapper<
            OuterEnumInteger

>> outerEnumIntegerPart;
  static UndefinedWrapper<
            OuterEnumInteger

> _outerEnumIntegerGetter(EnumTest parent) {
    return parent.outerEnumInteger;
  }
  static void _outerEnumIntegerSetter(EnumTest parent, UndefinedWrapper<
            OuterEnumInteger

> value) {
    parent.outerEnumInteger = value;
  }
  final PropertyReflection<EnumTest, UndefinedWrapper<
            OuterEnumDefaultValue

>> outerEnumDefaultValuePart;
  static UndefinedWrapper<
            OuterEnumDefaultValue

> _outerEnumDefaultValueGetter(EnumTest parent) {
    return parent.outerEnumDefaultValue;
  }
  static void _outerEnumDefaultValueSetter(EnumTest parent, UndefinedWrapper<
            OuterEnumDefaultValue

> value) {
    parent.outerEnumDefaultValue = value;
  }
  final PropertyReflection<EnumTest, UndefinedWrapper<
            OuterEnumIntegerDefaultValue

>> outerEnumIntegerDefaultValuePart;
  static UndefinedWrapper<
            OuterEnumIntegerDefaultValue

> _outerEnumIntegerDefaultValueGetter(EnumTest parent) {
    return parent.outerEnumIntegerDefaultValue;
  }
  static void _outerEnumIntegerDefaultValueSetter(EnumTest parent, UndefinedWrapper<
            OuterEnumIntegerDefaultValue

> value) {
    parent.outerEnumIntegerDefaultValue = value;
  }



  @override
  final Map<String, ClassReflection> discriminatorMappings;
  @override
  final Map<String, ClassReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;


  @override
  List<PropertyReflection<EnumTest, dynamic>> get properties => [
    enumStringPart,
enumStringRequiredPart,
enumIntegerPart,
enumIntegerOnlyPart,
enumNumberPart,
outerEnumPart,
outerEnumIntegerPart,
outerEnumDefaultValuePart,
outerEnumIntegerDefaultValuePart,
  ];

  final AdditionalPropertiesReflection<EnumTest, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<EnumTest, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<EnumTest, dynamic>> get allOfs => [
    
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

  /// Gets an example of EnumTest.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  EnumTest example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
        discriminatorExampleResults = const {},}) {
    final _reflection = this;
    final actualDiscriminators = discriminators ?? _reflection.aggregatedDiscriminators;
    discriminatorExampleResults = Map.from(discriminatorExampleResults);
    for (final MapEntry(key: propName, value: mappings) in actualDiscriminators.entries) {
      if (discriminatorExampleResults.containsKey(propName)) {
        continue;
      }
      final r =  exampleDiscriminator(mappings);
      if (r != null){
        discriminatorExampleResults[propName] = r;
      }
    }

    final exampleResult = EnumTest(
      enumString: () {
        var result = 


            exampleEnum(EnumTestEnumStringEnum.values)



;
        return UndefinedWrapper(result);
      } (),
      enumStringRequired: () {
        var result = 


            exampleEnum(EnumTestEnumStringRequiredEnum.values)



;
        return result;
      } (),
      enumInteger: () {
        var result = 


            exampleEnum(EnumTestEnumIntegerEnum.values)



;
        return UndefinedWrapper(result);
      } (),
      enumIntegerOnly: () {
        var result = 


            exampleEnum(EnumTestEnumIntegerOnlyEnum.values)



;
        return UndefinedWrapper(result);
      } (),
      enumNumber: () {
        var result = 


            exampleEnum(EnumTestEnumNumberEnum.values)



;
        return UndefinedWrapper(result);
      } (),
      outerEnum: () {
        var result = exampleNullable(() =>


            
            exampleEnum(OuterEnum.values)



 ) ;
        return UndefinedWrapper(result);
      } (),
      outerEnumInteger: () {
        var result = 


            
            exampleEnum(OuterEnumInteger.values)



;
        return UndefinedWrapper(result);
      } (),
      outerEnumDefaultValue: () {
        var result = 


            
            exampleEnum(OuterEnumDefaultValue.values)



;
        return UndefinedWrapper(result);
      } (),
      outerEnumIntegerDefaultValue: () {
        var result = 


            
            exampleEnum(OuterEnumIntegerDefaultValue.values)



;
        return UndefinedWrapper(result);
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class EnumTestXmlReflection {
    const EnumTestXmlReflection();
}

