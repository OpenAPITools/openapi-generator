// Model reflection

part of 'test_inline_freeform_additional_properties_request.dart';


//class reflection

class TestInlineFreeformAdditionalPropertiesRequestReflection extends ModelReflection<TestInlineFreeformAdditionalPropertiesRequest> {
  static TestInlineFreeformAdditionalPropertiesRequestReflection instanceGetter() => instance;
  static const instance = TestInlineFreeformAdditionalPropertiesRequestReflection._(
    modelName: r'testInlineFreeformAdditionalProperties_request',
    className: r'TestInlineFreeformAdditionalPropertiesRequest',
    xml: XmlReflection(
),
    somePropertyPart: PropertyReflection<TestInlineFreeformAdditionalPropertiesRequest, UndefinedWrapper<
            String
>>(
      dartName: r'someProperty',
      nullable: false,
      required: false,
      oasName: r'someProperty',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_somePropertyGetter),
      setter: FunctionWrapper2(_somePropertySetter),
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
  const TestInlineFreeformAdditionalPropertiesRequestReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.somePropertyPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<TestInlineFreeformAdditionalPropertiesRequest, UndefinedWrapper<
            String
>> somePropertyPart;
  static UndefinedWrapper<
            String
> _somePropertyGetter(TestInlineFreeformAdditionalPropertiesRequest parent) {
    return parent.someProperty;
  }
  static void _somePropertySetter(TestInlineFreeformAdditionalPropertiesRequest parent, UndefinedWrapper<
            String
> value) {
    parent.someProperty = value;
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
  List<PropertyReflection<TestInlineFreeformAdditionalPropertiesRequest, dynamic>> get properties => [
    somePropertyPart,
  ];

  @override
  final AdditionalPropertiesPart<TestInlineFreeformAdditionalPropertiesRequest, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(TestInlineFreeformAdditionalPropertiesRequest instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(TestInlineFreeformAdditionalPropertiesRequest instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<TestInlineFreeformAdditionalPropertiesRequest, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  TestInlineFreeformAdditionalPropertiesRequest empty() {
    return TestInlineFreeformAdditionalPropertiesRequest(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is TestInlineFreeformAdditionalPropertiesRequestReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


