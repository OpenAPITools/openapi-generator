// Model reflection

part of 'number_only.dart';


//class reflection

class NumberOnlyReflection extends ModelReflection<NumberOnly> {
  static NumberOnlyReflection instanceGetter() => instance;
  static const instance = NumberOnlyReflection._(
    modelName: r'NumberOnly',
    className: r'NumberOnly',
    xml: XmlReflection(
),
    justNumberPart: PropertyReflection<NumberOnly, UndefinedWrapper<
            num
>>(
      dartName: r'justNumber',
      nullable: false,
      required: false,
      oasName: r'JustNumber',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_justNumberGetter),
      setter: FunctionWrapper2(_justNumberSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.fornum
        
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
  const NumberOnlyReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.justNumberPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<NumberOnly, UndefinedWrapper<
            num
>> justNumberPart;
  static UndefinedWrapper<
            num
> _justNumberGetter(NumberOnly parent) {
    return parent.justNumber;
  }
  static void _justNumberSetter(NumberOnly parent, UndefinedWrapper<
            num
> value) {
    parent.justNumber = value;
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
  List<PropertyReflection<NumberOnly, dynamic>> get properties => [
    justNumberPart,
  ];

  @override
  final AdditionalPropertiesPart<NumberOnly, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(NumberOnly instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(NumberOnly instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<NumberOnly, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  NumberOnly empty() {
    return NumberOnly(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is NumberOnlyReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


