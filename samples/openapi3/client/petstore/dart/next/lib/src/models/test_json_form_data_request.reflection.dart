// Model reflection

part of 'test_json_form_data_request.dart';


//class reflection

class TestJsonFormDataRequestReflection extends ModelReflection<TestJsonFormDataRequest> {
  static TestJsonFormDataRequestReflection instanceGetter() => instance;
  static const instance = TestJsonFormDataRequestReflection._(
    modelName: r'testJsonFormData_request',
    className: r'TestJsonFormDataRequest',
    xml: XmlReflection(
),
    paramPart: PropertyReflection<TestJsonFormDataRequest, 
            String
>(
      dartName: r'param',
      nullable: false,
      required: true,
      oasName: r'param',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_paramGetter),
      setter: FunctionWrapper2(_paramSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
,
    ),
    param2Part: PropertyReflection<TestJsonFormDataRequest, 
            String
>(
      dartName: r'param2',
      nullable: false,
      required: true,
      oasName: r'param2',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_param2Getter),
      setter: FunctionWrapper2(_param2Setter),
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
  const TestJsonFormDataRequestReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.paramPart,
    required this.param2Part,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<TestJsonFormDataRequest, 
            String
> paramPart;
  static 
            String
 _paramGetter(TestJsonFormDataRequest parent) {
    return parent.param;
  }
  static void _paramSetter(TestJsonFormDataRequest parent, 
            String
 value) {
    parent.param = value;
  }

  final PropertyReflection<TestJsonFormDataRequest, 
            String
> param2Part;
  static 
            String
 _param2Getter(TestJsonFormDataRequest parent) {
    return parent.param2;
  }
  static void _param2Setter(TestJsonFormDataRequest parent, 
            String
 value) {
    parent.param2 = value;
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
  List<PropertyReflection<TestJsonFormDataRequest, dynamic>> get properties => [
    paramPart,
param2Part,
  ];

  @override
  final AdditionalPropertiesPart<TestJsonFormDataRequest, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(TestJsonFormDataRequest instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(TestJsonFormDataRequest instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<TestJsonFormDataRequest, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  TestJsonFormDataRequest empty() {
    return TestJsonFormDataRequest(
      param: paramPart.reflection.emptyFunction(),
      param2: param2Part.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is TestJsonFormDataRequestReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


