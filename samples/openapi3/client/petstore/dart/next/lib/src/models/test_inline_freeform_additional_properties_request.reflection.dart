// Model reflection

part of 'test_inline_freeform_additional_properties_request.dart';


//class reflection

class TestInlineFreeformAdditionalPropertiesRequestReflection extends ClassReflection<TestInlineFreeformAdditionalPropertiesRequest> {
  static TestInlineFreeformAdditionalPropertiesRequestReflection instanceGetter() => instance;
  static const instance = TestInlineFreeformAdditionalPropertiesRequestReflection._(
    modelName: r'testInlineFreeformAdditionalProperties_request',
    className: r'TestInlineFreeformAdditionalPropertiesRequest',
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
      getter: _somePropertyGetter,
      setter: _somePropertySetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<TestInlineFreeformAdditionalPropertiesRequest, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const TestInlineFreeformAdditionalPropertiesRequestReflection._({
    required this.modelName,
    required this.className,
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
  List<PropertyReflection<TestInlineFreeformAdditionalPropertiesRequest, dynamic>> get properties => [
    somePropertyPart,
  ];

  final AdditionalPropertiesReflection<TestInlineFreeformAdditionalPropertiesRequest, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<TestInlineFreeformAdditionalPropertiesRequest, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<TestInlineFreeformAdditionalPropertiesRequest, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => TestInlineFreeformAdditionalPropertiesRequest.canDeserialize(src);
  @override
  TestInlineFreeformAdditionalPropertiesRequest Function(Object? src) get deserializeFunction =>
      (src) => TestInlineFreeformAdditionalPropertiesRequest.deserialize(src);

  @override
  Object? Function(TestInlineFreeformAdditionalPropertiesRequest src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of TestInlineFreeformAdditionalPropertiesRequest.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  TestInlineFreeformAdditionalPropertiesRequest example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = TestInlineFreeformAdditionalPropertiesRequest(
      someProperty: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[somePropertyPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class TestInlineFreeformAdditionalPropertiesRequestXmlReflection {
    const TestInlineFreeformAdditionalPropertiesRequestXmlReflection();
}

