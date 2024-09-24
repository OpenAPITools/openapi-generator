// Model reflection

part of 'array_default.dart';


//class reflection

class ArrayDefaultReflection extends ModelReflection<ArrayDefault> {
  static ArrayDefaultReflection instanceGetter() => instance;
  static const instance = ArrayDefaultReflection._(
    modelName: r'ArrayDefault',
    className: r'ArrayDefault',
    xml: XmlReflection(
),
    withDefaultEmptyBracketPart: PropertyReflection<ArrayDefault, UndefinedWrapper<
    List<
        
            String
>
>>(
      dartName: r'withDefaultEmptyBracket',
      nullable: false,
      required: false,
      oasName: r'WithDefaultEmptyBracket',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_withDefaultEmptyBracketGetter),
      setter: FunctionWrapper2(_withDefaultEmptyBracketSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
)
,
)
),
    ),
    withoutDefaultPart: PropertyReflection<ArrayDefault, UndefinedWrapper<
    List<
        
            String
>
>>(
      dartName: r'withoutDefault',
      nullable: false,
      required: false,
      oasName: r'WithoutDefault',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_withoutDefaultGetter),
      setter: FunctionWrapper2(_withoutDefaultSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
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
  const ArrayDefaultReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.withDefaultEmptyBracketPart,
    required this.withoutDefaultPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ArrayDefault, UndefinedWrapper<
    List<
        
            String
>
>> withDefaultEmptyBracketPart;
  static UndefinedWrapper<
    List<
        
            String
>
> _withDefaultEmptyBracketGetter(ArrayDefault parent) {
    return parent.withDefaultEmptyBracket;
  }
  static void _withDefaultEmptyBracketSetter(ArrayDefault parent, UndefinedWrapper<
    List<
        
            String
>
> value) {
    parent.withDefaultEmptyBracket = value;
  }

  final PropertyReflection<ArrayDefault, UndefinedWrapper<
    List<
        
            String
>
>> withoutDefaultPart;
  static UndefinedWrapper<
    List<
        
            String
>
> _withoutDefaultGetter(ArrayDefault parent) {
    return parent.withoutDefault;
  }
  static void _withoutDefaultSetter(ArrayDefault parent, UndefinedWrapper<
    List<
        
            String
>
> value) {
    parent.withoutDefault = value;
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
  List<PropertyReflection<ArrayDefault, dynamic>> get properties => [
    withDefaultEmptyBracketPart,
withoutDefaultPart,
  ];

  @override
  final AdditionalPropertiesPart<ArrayDefault, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ArrayDefault instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ArrayDefault instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<ArrayDefault, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ArrayDefault empty() {
    return ArrayDefault(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ArrayDefaultReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


