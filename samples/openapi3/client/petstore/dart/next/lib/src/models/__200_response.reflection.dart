// Model reflection

part of '__200_response.dart';


//class reflection

class $200ResponseReflection extends ModelReflection<$200Response> {
  static $200ResponseReflection instanceGetter() => instance;
  static const instance = $200ResponseReflection._(
    modelName: r'200_response',
    className: r'$200Response',
    xml: XmlReflection(
    xmlName: r'Name',
),
    namePart: PropertyReflection<$200Response, UndefinedWrapper<
            int
>>(
      dartName: r'name',
      nullable: false,
      required: false,
      oasName: r'name',
      oasType: r'integer',
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
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
),
    ),
    propertyClassPart: PropertyReflection<$200Response, UndefinedWrapper<
            String
>>(
      dartName: r'propertyClass',
      nullable: false,
      required: false,
      oasName: r'class',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_propertyClassGetter),
      setter: FunctionWrapper2(_propertyClassSetter),
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
  const $200ResponseReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.namePart,
    required this.propertyClassPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<$200Response, UndefinedWrapper<
            int
>> namePart;
  static UndefinedWrapper<
            int
> _nameGetter($200Response parent) {
    return parent.name;
  }
  static void _nameSetter($200Response parent, UndefinedWrapper<
            int
> value) {
    parent.name = value;
  }

  final PropertyReflection<$200Response, UndefinedWrapper<
            String
>> propertyClassPart;
  static UndefinedWrapper<
            String
> _propertyClassGetter($200Response parent) {
    return parent.propertyClass;
  }
  static void _propertyClassSetter($200Response parent, UndefinedWrapper<
            String
> value) {
    parent.propertyClass = value;
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
  List<PropertyReflection<$200Response, dynamic>> get properties => [
    namePart,
propertyClassPart,
  ];

  @override
  final AdditionalPropertiesPart<$200Response, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter($200Response instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter($200Response instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<$200Response, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  $200Response empty() {
    return $200Response(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is $200ResponseReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


