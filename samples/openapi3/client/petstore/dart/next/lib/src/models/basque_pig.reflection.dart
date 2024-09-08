// Model reflection

part of 'basque_pig.dart';


//class reflection

class BasquePigReflection extends ModelReflection<BasquePig> {
  static BasquePigReflection instanceGetter() => instance;
  static const instance = BasquePigReflection._(
    modelName: r'BasquePig',
    className: r'BasquePig',
    xml: XmlReflection(
),
    classNamePart: PropertyReflection<BasquePig, 
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
  const BasquePigReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.classNamePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<BasquePig, 
            String
> classNamePart;
  static 
            String
 _classNameGetter(BasquePig parent) {
    return parent.className;
  }
  static void _classNameSetter(BasquePig parent, 
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
  List<PropertyReflection<BasquePig, dynamic>> get properties => [
    classNamePart,
  ];

  @override
  final AdditionalPropertiesPart<BasquePig, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(BasquePig instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(BasquePig instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<BasquePig, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  BasquePig empty() {
    return BasquePig(
      className: classNamePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is BasquePigReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


