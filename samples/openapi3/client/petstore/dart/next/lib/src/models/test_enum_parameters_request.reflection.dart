// Model reflection

part of 'test_enum_parameters_request.dart';


//class reflection

class TestEnumParametersRequestReflection extends ClassReflection<TestEnumParametersRequest> {
  static TestEnumParametersRequestReflection instanceGetter() => instance;
  static const instance = TestEnumParametersRequestReflection._(
    modelName: r'testEnumParameters_request',
    className: r'TestEnumParametersRequest',
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
      itemsReflection: ItemsReflection<TestEnumParametersRequest, 
            TestEnumParametersRequestEnumFormStringArrayEnum

>(parentReflectionGetter: instanceGetter,),
      getter: _enumFormStringArrayGetter,
      setter: _enumFormStringArraySetter,
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
      getter: _enumFormStringGetter,
      setter: _enumFormStringSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<TestEnumParametersRequest, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const TestEnumParametersRequestReflection._({
    required this.modelName,
    required this.className,
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
  final Map<String, ClassReflection> discriminatorMappings;
  @override
  final Map<String, ClassReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;


  @override
  List<PropertyReflection<TestEnumParametersRequest, dynamic>> get properties => [
    enumFormStringArrayPart,
enumFormStringPart,
  ];

  final AdditionalPropertiesReflection<TestEnumParametersRequest, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<TestEnumParametersRequest, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<TestEnumParametersRequest, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => TestEnumParametersRequest.canDeserialize(src);
  @override
  TestEnumParametersRequest Function(Object? src) get deserializeFunction =>
      (src) => TestEnumParametersRequest.deserialize(src);

  @override
  Object? Function(TestEnumParametersRequest src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of TestEnumParametersRequest.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  TestEnumParametersRequest example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
        discriminatorExampleResults = const {},}) {
    final _reflection = this;
    final actualDiscriminators = discriminators ?? _reflection.aggregatedDiscriminators;
    discriminatorExampleResults = Map.from(discriminatorExampleResults);
    for (final MapEntry(key: propName, value: mappings) in actualDiscriminators.entries) {
      if (discriminatorExampleResults.containsKey(propName)) {
        continue;
      }
      final r =  exampleDiscriminator(mappings);
      if (r != null){
        discriminatorExampleResults[propName] = r;
      }
    }

    final exampleResult = TestEnumParametersRequest(
      enumFormStringArray: () {
        var result = 


    exampleList(() { return 


            exampleEnum(TestEnumParametersRequestEnumFormStringArrayEnum.values)



; })



;
        return UndefinedWrapper(result);
      } (),
      enumFormString: () {
        var result = 


            exampleEnum(TestEnumParametersRequestEnumFormStringEnum.values)



;
        return UndefinedWrapper(result);
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class TestEnumParametersRequestXmlReflection {
    const TestEnumParametersRequestXmlReflection();
}

