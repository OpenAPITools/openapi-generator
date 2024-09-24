// Model reflection

part of 'deprecated_object.dart';


//class reflection

class DeprecatedObjectReflection extends ModelReflection<DeprecatedObject> {
  static DeprecatedObjectReflection instanceGetter() => instance;
  static const instance = DeprecatedObjectReflection._(
    modelName: r'DeprecatedObject',
    className: r'DeprecatedObject',
    xml: XmlReflection(
),
    namePart: PropertyReflection<DeprecatedObject, UndefinedWrapper<
            String
>>(
      dartName: r'name',
      nullable: false,
      required: false,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_nameGetter),
      setter: FunctionWrapper2(_nameSetter),
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
  const DeprecatedObjectReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.namePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<DeprecatedObject, UndefinedWrapper<
            String
>> namePart;
  static UndefinedWrapper<
            String
> _nameGetter(DeprecatedObject parent) {
    return parent.name;
  }
  static void _nameSetter(DeprecatedObject parent, UndefinedWrapper<
            String
> value) {
    parent.name = value;
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
  List<PropertyReflection<DeprecatedObject, dynamic>> get properties => [
    namePart,
  ];

  @override
  final AdditionalPropertiesPart<DeprecatedObject, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(DeprecatedObject instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(DeprecatedObject instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<DeprecatedObject, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  DeprecatedObject empty() {
    return DeprecatedObject(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is DeprecatedObjectReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


