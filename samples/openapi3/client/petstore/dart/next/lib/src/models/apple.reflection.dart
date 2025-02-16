// Model reflection

part of 'apple.dart';


//class reflection

class AppleReflection extends ModelReflection<Apple> {
  static AppleReflection instanceGetter() => instance;
  static const instance = AppleReflection._(
    modelName: r'apple',
    className: r'Apple',
    xml: XmlReflection(
),
    cultivarPart: PropertyReflection<Apple, UndefinedWrapper<
            String
>>(
      dartName: r'cultivar',
      nullable: false,
      required: false,
      oasName: r'cultivar',
      oasType: r'string',
      pattern: r'/^[a-zA-Z\\s]*$/',
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_cultivarGetter),
      setter: FunctionWrapper2(_cultivarSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    originPart: PropertyReflection<Apple, UndefinedWrapper<
            String
>>(
      dartName: r'origin',
      nullable: false,
      required: false,
      oasName: r'origin',
      oasType: r'string',
      pattern: r'/^[A-Z\\s]*$/i',
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_originGetter),
      setter: FunctionWrapper2(_originSetter),
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
  const AppleReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.cultivarPart,
    required this.originPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Apple, UndefinedWrapper<
            String
>> cultivarPart;
  static UndefinedWrapper<
            String
> _cultivarGetter(Apple parent) {
    return parent.cultivar;
  }
  static void _cultivarSetter(Apple parent, UndefinedWrapper<
            String
> value) {
    parent.cultivar = value;
  }

  final PropertyReflection<Apple, UndefinedWrapper<
            String
>> originPart;
  static UndefinedWrapper<
            String
> _originGetter(Apple parent) {
    return parent.origin;
  }
  static void _originSetter(Apple parent, UndefinedWrapper<
            String
> value) {
    parent.origin = value;
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
  List<PropertyReflection<Apple, dynamic>> get properties => [
    cultivarPart,
originPart,
  ];

  @override
  final AdditionalPropertiesPart<Apple, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Apple instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Apple instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<Apple, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Apple empty() {
    return Apple(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is AppleReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


