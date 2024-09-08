// Model reflection

part of 'enum_arrays.dart';


//class reflection

class EnumArraysReflection extends ModelReflection<EnumArrays> {
  static EnumArraysReflection instanceGetter() => instance;
  static const instance = EnumArraysReflection._(
    modelName: r'EnumArrays',
    className: r'EnumArrays',
    xml: XmlReflection(
),
    justSymbolPart: PropertyReflection<EnumArrays, UndefinedWrapper<
            EnumArraysJustSymbolEnum
>>(
      dartName: r'justSymbol',
      nullable: false,
      required: false,
      oasName: r'just_symbol',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_justSymbolGetter),
      setter: FunctionWrapper2(_justSymbolSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            EnumArraysJustSymbolEnum.$reflection
        
        
,
)
),
    ),
    arrayEnumPart: PropertyReflection<EnumArrays, UndefinedWrapper<
    List<
        
            EnumArraysArrayEnumEnum
>
>>(
      dartName: r'arrayEnum',
      nullable: false,
      required: false,
      oasName: r'array_enum',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_arrayEnumGetter),
      setter: FunctionWrapper2(_arrayEnumSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            EnumArraysArrayEnumEnum.$reflection
        
        
,
)
)
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
  const EnumArraysReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.justSymbolPart,
    required this.arrayEnumPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<EnumArrays, UndefinedWrapper<
            EnumArraysJustSymbolEnum
>> justSymbolPart;
  static UndefinedWrapper<
            EnumArraysJustSymbolEnum
> _justSymbolGetter(EnumArrays parent) {
    return parent.justSymbol;
  }
  static void _justSymbolSetter(EnumArrays parent, UndefinedWrapper<
            EnumArraysJustSymbolEnum
> value) {
    parent.justSymbol = value;
  }

  final PropertyReflection<EnumArrays, UndefinedWrapper<
    List<
        
            EnumArraysArrayEnumEnum
>
>> arrayEnumPart;
  static UndefinedWrapper<
    List<
        
            EnumArraysArrayEnumEnum
>
> _arrayEnumGetter(EnumArrays parent) {
    return parent.arrayEnum;
  }
  static void _arrayEnumSetter(EnumArrays parent, UndefinedWrapper<
    List<
        
            EnumArraysArrayEnumEnum
>
> value) {
    parent.arrayEnum = value;
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
  List<PropertyReflection<EnumArrays, dynamic>> get properties => [
    justSymbolPart,
arrayEnumPart,
  ];

  @override
  final AdditionalPropertiesPart<EnumArrays, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(EnumArrays instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(EnumArrays instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<EnumArrays, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  EnumArrays empty() {
    return EnumArrays(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is EnumArraysReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


