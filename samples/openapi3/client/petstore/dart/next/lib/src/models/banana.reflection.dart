// Model reflection

part of 'banana.dart';


//class reflection

class BananaReflection extends ModelReflection<Banana> {
  static BananaReflection instanceGetter() => instance;
  static const instance = BananaReflection._(
    modelName: r'banana',
    className: r'Banana',
    xml: XmlReflection(
),
    lengthCmPart: PropertyReflection<Banana, 
            num
>(
      dartName: r'lengthCm',
      nullable: false,
      required: true,
      oasName: r'lengthCm',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_lengthCmGetter),
      setter: FunctionWrapper2(_lengthCmSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.fornum
        
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
  const BananaReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.lengthCmPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Banana, 
            num
> lengthCmPart;
  static 
            num
 _lengthCmGetter(Banana parent) {
    return parent.lengthCm;
  }
  static void _lengthCmSetter(Banana parent, 
            num
 value) {
    parent.lengthCm = value;
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
  List<PropertyReflection<Banana, dynamic>> get properties => [
    lengthCmPart,
  ];

  @override
  final AdditionalPropertiesPart<Banana, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Banana instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Banana instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<Banana, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Banana empty() {
    return Banana(
      lengthCm: lengthCmPart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is BananaReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


