// Model reflection

part of 'whale.dart';


//class reflection

class WhaleReflection extends ModelReflection<Whale> {
  static WhaleReflection instanceGetter() => instance;
  static const instance = WhaleReflection._(
    modelName: r'whale',
    className: r'Whale',
    xml: XmlReflection(
),
    hasBaleenPart: PropertyReflection<Whale, UndefinedWrapper<
            bool
>>(
      dartName: r'hasBaleen',
      nullable: false,
      required: false,
      oasName: r'hasBaleen',
      oasType: r'boolean',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_hasBaleenGetter),
      setter: FunctionWrapper2(_hasBaleenSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forbool
        
,
)
),
    ),
    hasTeethPart: PropertyReflection<Whale, UndefinedWrapper<
            bool
>>(
      dartName: r'hasTeeth',
      nullable: false,
      required: false,
      oasName: r'hasTeeth',
      oasType: r'boolean',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_hasTeethGetter),
      setter: FunctionWrapper2(_hasTeethSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forbool
        
,
)
),
    ),
    classNamePart: PropertyReflection<Whale, 
            String
>(
      dartName: r'className',
      nullable: false,
      required: true,
      oasName: r'className',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_classNameGetter),
      setter: FunctionWrapper2(_classNameSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
,
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
  const WhaleReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.hasBaleenPart,
    required this.hasTeethPart,
    required this.classNamePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Whale, UndefinedWrapper<
            bool
>> hasBaleenPart;
  static UndefinedWrapper<
            bool
> _hasBaleenGetter(Whale parent) {
    return parent.hasBaleen;
  }
  static void _hasBaleenSetter(Whale parent, UndefinedWrapper<
            bool
> value) {
    parent.hasBaleen = value;
  }

  final PropertyReflection<Whale, UndefinedWrapper<
            bool
>> hasTeethPart;
  static UndefinedWrapper<
            bool
> _hasTeethGetter(Whale parent) {
    return parent.hasTeeth;
  }
  static void _hasTeethSetter(Whale parent, UndefinedWrapper<
            bool
> value) {
    parent.hasTeeth = value;
  }

  final PropertyReflection<Whale, 
            String
> classNamePart;
  static 
            String
 _classNameGetter(Whale parent) {
    return parent.className;
  }
  static void _classNameSetter(Whale parent, 
            String
 value) {
    parent.className = value;
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
  List<PropertyReflection<Whale, dynamic>> get properties => [
    hasBaleenPart,
hasTeethPart,
classNamePart,
  ];

  @override
  final AdditionalPropertiesPart<Whale, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Whale instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Whale instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<Whale, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Whale empty() {
    return Whale(
      className: classNamePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is WhaleReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


