// Model reflection

part of 'mixed_type_one_of_object_one_of1.dart';


//class reflection

class MixedTypeOneOfObjectOneOf1Reflection extends ModelReflection<MixedTypeOneOfObjectOneOf1> {
  static MixedTypeOneOfObjectOneOf1Reflection instanceGetter() => instance;
  static const instance = MixedTypeOneOfObjectOneOf1Reflection._(
    modelName: r'MixedTypeOneOfObject_oneOf_1',
    className: r'MixedTypeOneOfObjectOneOf1',
    xml: XmlReflection(
),
    urlPart: PropertyReflection<MixedTypeOneOfObjectOneOf1, 
            Uri
>(
      dartName: r'url',
      nullable: false,
      required: true,
      oasName: r'url',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_urlGetter),
      setter: FunctionWrapper2(_urlSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forUri
        
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
  const MixedTypeOneOfObjectOneOf1Reflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.urlPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<MixedTypeOneOfObjectOneOf1, 
            Uri
> urlPart;
  static 
            Uri
 _urlGetter(MixedTypeOneOfObjectOneOf1 parent) {
    return parent.url;
  }
  static void _urlSetter(MixedTypeOneOfObjectOneOf1 parent, 
            Uri
 value) {
    parent.url = value;
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
  List<PropertyReflection<MixedTypeOneOfObjectOneOf1, dynamic>> get properties => [
    urlPart,
  ];

  @override
  final AdditionalPropertiesPart<MixedTypeOneOfObjectOneOf1, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(MixedTypeOneOfObjectOneOf1 instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(MixedTypeOneOfObjectOneOf1 instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<MixedTypeOneOfObjectOneOf1, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  MixedTypeOneOfObjectOneOf1 empty() {
    return MixedTypeOneOfObjectOneOf1(
      url: urlPart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is MixedTypeOneOfObjectOneOf1Reflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


