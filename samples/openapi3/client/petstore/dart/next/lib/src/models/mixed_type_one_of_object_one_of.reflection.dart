// Model reflection

part of 'mixed_type_one_of_object_one_of.dart';


//class reflection

class MixedTypeOneOfObjectOneOfReflection extends ModelReflection<MixedTypeOneOfObjectOneOf> {
  static MixedTypeOneOfObjectOneOfReflection instanceGetter() => instance;
  static const instance = MixedTypeOneOfObjectOneOfReflection._(
    modelName: r'MixedTypeOneOfObject_oneOf',
    className: r'MixedTypeOneOfObjectOneOf',
    xml: XmlReflection(
),
    contentPart: PropertyReflection<MixedTypeOneOfObjectOneOf, 
            String
>(
      dartName: r'content',
      nullable: false,
      required: true,
      oasName: r'content',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_contentGetter),
      setter: FunctionWrapper2(_contentSetter),
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
  const MixedTypeOneOfObjectOneOfReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.contentPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<MixedTypeOneOfObjectOneOf, 
            String
> contentPart;
  static 
            String
 _contentGetter(MixedTypeOneOfObjectOneOf parent) {
    return parent.content;
  }
  static void _contentSetter(MixedTypeOneOfObjectOneOf parent, 
            String
 value) {
    parent.content = value;
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
  List<PropertyReflection<MixedTypeOneOfObjectOneOf, dynamic>> get properties => [
    contentPart,
  ];

  @override
  final AdditionalPropertiesPart<MixedTypeOneOfObjectOneOf, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(MixedTypeOneOfObjectOneOf instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(MixedTypeOneOfObjectOneOf instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<MixedTypeOneOfObjectOneOf, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  MixedTypeOneOfObjectOneOf empty() {
    return MixedTypeOneOfObjectOneOf(
      content: contentPart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is MixedTypeOneOfObjectOneOfReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


