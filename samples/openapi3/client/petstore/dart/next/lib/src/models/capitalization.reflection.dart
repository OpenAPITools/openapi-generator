// Model reflection

part of 'capitalization.dart';


//class reflection

class CapitalizationReflection extends ModelReflection<Capitalization> {
  static CapitalizationReflection instanceGetter() => instance;
  static const instance = CapitalizationReflection._(
    modelName: r'Capitalization',
    className: r'Capitalization',
    xml: XmlReflection(
),
    smallCamelPart: PropertyReflection<Capitalization, UndefinedWrapper<
            String
>>(
      dartName: r'smallCamel',
      nullable: false,
      required: false,
      oasName: r'smallCamel',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_smallCamelGetter),
      setter: FunctionWrapper2(_smallCamelSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    capitalCamelPart: PropertyReflection<Capitalization, UndefinedWrapper<
            String
>>(
      dartName: r'capitalCamel',
      nullable: false,
      required: false,
      oasName: r'CapitalCamel',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_capitalCamelGetter),
      setter: FunctionWrapper2(_capitalCamelSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    smallSnakePart: PropertyReflection<Capitalization, UndefinedWrapper<
            String
>>(
      dartName: r'smallSnake',
      nullable: false,
      required: false,
      oasName: r'small_Snake',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_smallSnakeGetter),
      setter: FunctionWrapper2(_smallSnakeSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    capitalSnakePart: PropertyReflection<Capitalization, UndefinedWrapper<
            String
>>(
      dartName: r'capitalSnake',
      nullable: false,
      required: false,
      oasName: r'Capital_Snake',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_capitalSnakeGetter),
      setter: FunctionWrapper2(_capitalSnakeSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    scAETHFlowPointsPart: PropertyReflection<Capitalization, UndefinedWrapper<
            String
>>(
      dartName: r'scAETHFlowPoints',
      nullable: false,
      required: false,
      oasName: r'SCA_ETH_Flow_Points',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_scAETHFlowPointsGetter),
      setter: FunctionWrapper2(_scAETHFlowPointsSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    ATT_NAMEPart: PropertyReflection<Capitalization, UndefinedWrapper<
            String
>>(
      dartName: r'ATT_NAME',
      nullable: false,
      required: false,
      oasName: r'ATT_NAME',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_ATT_NAMEGetter),
      setter: FunctionWrapper2(_ATT_NAMESetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
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
  const CapitalizationReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.smallCamelPart,
    required this.capitalCamelPart,
    required this.smallSnakePart,
    required this.capitalSnakePart,
    required this.scAETHFlowPointsPart,
    required this.ATT_NAMEPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Capitalization, UndefinedWrapper<
            String
>> smallCamelPart;
  static UndefinedWrapper<
            String
> _smallCamelGetter(Capitalization parent) {
    return parent.smallCamel;
  }
  static void _smallCamelSetter(Capitalization parent, UndefinedWrapper<
            String
> value) {
    parent.smallCamel = value;
  }

  final PropertyReflection<Capitalization, UndefinedWrapper<
            String
>> capitalCamelPart;
  static UndefinedWrapper<
            String
> _capitalCamelGetter(Capitalization parent) {
    return parent.capitalCamel;
  }
  static void _capitalCamelSetter(Capitalization parent, UndefinedWrapper<
            String
> value) {
    parent.capitalCamel = value;
  }

  final PropertyReflection<Capitalization, UndefinedWrapper<
            String
>> smallSnakePart;
  static UndefinedWrapper<
            String
> _smallSnakeGetter(Capitalization parent) {
    return parent.smallSnake;
  }
  static void _smallSnakeSetter(Capitalization parent, UndefinedWrapper<
            String
> value) {
    parent.smallSnake = value;
  }

  final PropertyReflection<Capitalization, UndefinedWrapper<
            String
>> capitalSnakePart;
  static UndefinedWrapper<
            String
> _capitalSnakeGetter(Capitalization parent) {
    return parent.capitalSnake;
  }
  static void _capitalSnakeSetter(Capitalization parent, UndefinedWrapper<
            String
> value) {
    parent.capitalSnake = value;
  }

  final PropertyReflection<Capitalization, UndefinedWrapper<
            String
>> scAETHFlowPointsPart;
  static UndefinedWrapper<
            String
> _scAETHFlowPointsGetter(Capitalization parent) {
    return parent.scAETHFlowPoints;
  }
  static void _scAETHFlowPointsSetter(Capitalization parent, UndefinedWrapper<
            String
> value) {
    parent.scAETHFlowPoints = value;
  }

  final PropertyReflection<Capitalization, UndefinedWrapper<
            String
>> ATT_NAMEPart;
  static UndefinedWrapper<
            String
> _ATT_NAMEGetter(Capitalization parent) {
    return parent.ATT_NAME;
  }
  static void _ATT_NAMESetter(Capitalization parent, UndefinedWrapper<
            String
> value) {
    parent.ATT_NAME = value;
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
  List<PropertyReflection<Capitalization, dynamic>> get properties => [
    smallCamelPart,
capitalCamelPart,
smallSnakePart,
capitalSnakePart,
scAETHFlowPointsPart,
ATT_NAMEPart,
  ];

  @override
  final AdditionalPropertiesPart<Capitalization, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Capitalization instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Capitalization instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<Capitalization, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Capitalization empty() {
    return Capitalization(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is CapitalizationReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


