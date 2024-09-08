// Model reflection

part of 'test_enum_parameters_request.dart';


//class reflection

class TestEnumParametersRequestReflection extends ModelReflection<TestEnumParametersRequest> {
  static TestEnumParametersRequestReflection instanceGetter() => instance;
  static const instance = TestEnumParametersRequestReflection._(
    modelName: r'testEnumParameters_request',
    className: r'TestEnumParametersRequest',
    xml: XmlReflection(
),
    enumFormStringArrayPart: PropertyReflection<TestEnumParametersRequest, UndefinedWrapper<
    List<
        
            TestEnumParametersRequestEnumFormStringArrayEnum
>
>>(
      dartName: r'enumFormStringArray',
      nullable: false,
      required: false,
      oasName: r'enum_form_string_array',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_enumFormStringArrayGetter),
      setter: FunctionWrapper2(_enumFormStringArraySetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            TestEnumParametersRequestEnumFormStringArrayEnum.$reflection
        
        
,
)
)
,
)
),
    ),
    enumFormStringPart: PropertyReflection<TestEnumParametersRequest, UndefinedWrapper<
            TestEnumParametersRequestEnumFormStringEnum
>>(
      dartName: r'enumFormString',
      nullable: false,
      required: false,
      oasName: r'enum_form_string',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_enumFormStringGetter),
      setter: FunctionWrapper2(_enumFormStringSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            TestEnumParametersRequestEnumFormStringEnum.$reflection
        
        
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
  const TestEnumParametersRequestReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.enumFormStringArrayPart,
    required this.enumFormStringPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<TestEnumParametersRequest, UndefinedWrapper<
    List<
        
            TestEnumParametersRequestEnumFormStringArrayEnum
>
>> enumFormStringArrayPart;
  static UndefinedWrapper<
    List<
        
            TestEnumParametersRequestEnumFormStringArrayEnum
>
> _enumFormStringArrayGetter(TestEnumParametersRequest parent) {
    return parent.enumFormStringArray;
  }
  static void _enumFormStringArraySetter(TestEnumParametersRequest parent, UndefinedWrapper<
    List<
        
            TestEnumParametersRequestEnumFormStringArrayEnum
>
> value) {
    parent.enumFormStringArray = value;
  }

  final PropertyReflection<TestEnumParametersRequest, UndefinedWrapper<
            TestEnumParametersRequestEnumFormStringEnum
>> enumFormStringPart;
  static UndefinedWrapper<
            TestEnumParametersRequestEnumFormStringEnum
> _enumFormStringGetter(TestEnumParametersRequest parent) {
    return parent.enumFormString;
  }
  static void _enumFormStringSetter(TestEnumParametersRequest parent, UndefinedWrapper<
            TestEnumParametersRequestEnumFormStringEnum
> value) {
    parent.enumFormString = value;
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
  List<PropertyReflection<TestEnumParametersRequest, dynamic>> get properties => [
    enumFormStringArrayPart,
enumFormStringPart,
  ];

  @override
  final AdditionalPropertiesPart<TestEnumParametersRequest, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(TestEnumParametersRequest instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(TestEnumParametersRequest instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<TestEnumParametersRequest, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  TestEnumParametersRequest empty() {
    return TestEnumParametersRequest(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is TestEnumParametersRequestReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


