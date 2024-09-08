// Model reflection

part of 'danish_pig.dart';


//class reflection

class DanishPigReflection extends ModelReflection<DanishPig> {
  static DanishPigReflection instanceGetter() => instance;
  static const instance = DanishPigReflection._(
    modelName: r'DanishPig',
    className: r'DanishPig',
    xml: XmlReflection(
),
    classNamePart: PropertyReflection<DanishPig, 
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
  const DanishPigReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.classNamePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<DanishPig, 
            String
> classNamePart;
  static 
            String
 _classNameGetter(DanishPig parent) {
    return parent.className;
  }
  static void _classNameSetter(DanishPig parent, 
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
  List<PropertyReflection<DanishPig, dynamic>> get properties => [
    classNamePart,
  ];

  @override
  final AdditionalPropertiesPart<DanishPig, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(DanishPig instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(DanishPig instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<DanishPig, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  DanishPig empty() {
    return DanishPig(
      className: classNamePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is DanishPigReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


