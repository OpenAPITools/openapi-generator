// Model reflection

part of 'enum_test.dart';


//class reflection

class EnumTestReflection extends ModelReflection<EnumTest> {
  static EnumTestReflection instanceGetter() => instance;
  static const instance = EnumTestReflection._(
    modelName: r'Enum_Test',
    className: r'EnumTest',
    xml: XmlReflection(
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_enumStringGetter),
      setter: FunctionWrapper2(_enumStringSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            EnumTestEnumStringEnum.$reflection
        
        
,
)
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_enumStringRequiredGetter),
      setter: FunctionWrapper2(_enumStringRequiredSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            EnumTestEnumStringRequiredEnum.$reflection
        
        
,
)
,
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_enumIntegerGetter),
      setter: FunctionWrapper2(_enumIntegerSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            EnumTestEnumIntegerEnum.$reflection
        
        
,
)
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_enumIntegerOnlyGetter),
      setter: FunctionWrapper2(_enumIntegerOnlySetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            EnumTestEnumIntegerOnlyEnum.$reflection
        
        
,
)
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_enumNumberGetter),
      setter: FunctionWrapper2(_enumNumberSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            EnumTestEnumNumberEnum.$reflection
        
        
,
)
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_outerEnumGetter),
      setter: FunctionWrapper2(_outerEnumSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
            
        
        
            OuterEnum.$reflection
        
),
)
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_outerEnumIntegerGetter),
      setter: FunctionWrapper2(_outerEnumIntegerSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            OuterEnumInteger.$reflection
        
,
)
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_outerEnumDefaultValueGetter),
      setter: FunctionWrapper2(_outerEnumDefaultValueSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            OuterEnumDefaultValue.$reflection
        
,
)
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_outerEnumIntegerDefaultValueGetter),
      setter: FunctionWrapper2(_outerEnumIntegerDefaultValueSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            OuterEnumIntegerDefaultValue.$reflection
        
,
)
),
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesPart(
      parentReflectionGetter: instanceGetter,
      itemReflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(ObjectReflection()
),
)
,
      getter: FunctionWrapper1(_AdditionalPropertiesGetter),
      setter: FunctionWrapper2(_AdditionalPropertiesSetter),
    ),
  );
  const EnumTestReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
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
  final Map<String, ModelReflection> discriminatorMappings;
  @override
  final Map<String, ModelReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;
  @override
  final XmlReflection xml;

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

  @override
  final AdditionalPropertiesPart<EnumTest, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(EnumTest instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(EnumTest instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<EnumTest, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  EnumTest empty() {
    return EnumTest(
      enumStringRequired: enumStringRequiredPart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is EnumTestReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


