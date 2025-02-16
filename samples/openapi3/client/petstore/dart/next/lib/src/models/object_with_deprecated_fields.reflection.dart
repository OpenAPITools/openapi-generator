// Model reflection

part of 'object_with_deprecated_fields.dart';


//class reflection

class ObjectWithDeprecatedFieldsReflection extends ModelReflection<ObjectWithDeprecatedFields> {
  static ObjectWithDeprecatedFieldsReflection instanceGetter() => instance;
  static const instance = ObjectWithDeprecatedFieldsReflection._(
    modelName: r'ObjectWithDeprecatedFields',
    className: r'ObjectWithDeprecatedFields',
    xml: XmlReflection(
),
    uuidPart: PropertyReflection<ObjectWithDeprecatedFields, UndefinedWrapper<
            String
>>(
      dartName: r'uuid',
      nullable: false,
      required: false,
      oasName: r'uuid',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_uuidGetter),
      setter: FunctionWrapper2(_uuidSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    idPart: PropertyReflection<ObjectWithDeprecatedFields, UndefinedWrapper<
            num
>>(
      dartName: r'id',
      nullable: false,
      required: false,
      oasName: r'id',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_idGetter),
      setter: FunctionWrapper2(_idSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.fornum
        
,
)
),
    ),
    deprecatedRefPart: PropertyReflection<ObjectWithDeprecatedFields, UndefinedWrapper<
            DeprecatedObject
>>(
      dartName: r'deprecatedRef',
      nullable: false,
      required: false,
      oasName: r'deprecatedRef',
      oasType: r'DeprecatedObject',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_deprecatedRefGetter),
      setter: FunctionWrapper2(_deprecatedRefSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                DeprecatedObject.$reflection
        
,
)
),
    ),
    barsPart: PropertyReflection<ObjectWithDeprecatedFields, UndefinedWrapper<
    List<
        
            String
>
>>(
      dartName: r'bars',
      nullable: false,
      required: false,
      oasName: r'bars',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_barsGetter),
      setter: FunctionWrapper2(_barsSetter),
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
  const ObjectWithDeprecatedFieldsReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.uuidPart,
    required this.idPart,
    required this.deprecatedRefPart,
    required this.barsPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ObjectWithDeprecatedFields, UndefinedWrapper<
            String
>> uuidPart;
  static UndefinedWrapper<
            String
> _uuidGetter(ObjectWithDeprecatedFields parent) {
    return parent.uuid;
  }
  static void _uuidSetter(ObjectWithDeprecatedFields parent, UndefinedWrapper<
            String
> value) {
    parent.uuid = value;
  }

  final PropertyReflection<ObjectWithDeprecatedFields, UndefinedWrapper<
            num
>> idPart;
  static UndefinedWrapper<
            num
> _idGetter(ObjectWithDeprecatedFields parent) {
    return parent.id;
  }
  static void _idSetter(ObjectWithDeprecatedFields parent, UndefinedWrapper<
            num
> value) {
    parent.id = value;
  }

  final PropertyReflection<ObjectWithDeprecatedFields, UndefinedWrapper<
            DeprecatedObject
>> deprecatedRefPart;
  static UndefinedWrapper<
            DeprecatedObject
> _deprecatedRefGetter(ObjectWithDeprecatedFields parent) {
    return parent.deprecatedRef;
  }
  static void _deprecatedRefSetter(ObjectWithDeprecatedFields parent, UndefinedWrapper<
            DeprecatedObject
> value) {
    parent.deprecatedRef = value;
  }

  final PropertyReflection<ObjectWithDeprecatedFields, UndefinedWrapper<
    List<
        
            String
>
>> barsPart;
  static UndefinedWrapper<
    List<
        
            String
>
> _barsGetter(ObjectWithDeprecatedFields parent) {
    return parent.bars;
  }
  static void _barsSetter(ObjectWithDeprecatedFields parent, UndefinedWrapper<
    List<
        
            String
>
> value) {
    parent.bars = value;
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
  List<PropertyReflection<ObjectWithDeprecatedFields, dynamic>> get properties => [
    uuidPart,
idPart,
deprecatedRefPart,
barsPart,
  ];

  @override
  final AdditionalPropertiesPart<ObjectWithDeprecatedFields, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ObjectWithDeprecatedFields instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ObjectWithDeprecatedFields instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<ObjectWithDeprecatedFields, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ObjectWithDeprecatedFields empty() {
    return ObjectWithDeprecatedFields(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ObjectWithDeprecatedFieldsReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


