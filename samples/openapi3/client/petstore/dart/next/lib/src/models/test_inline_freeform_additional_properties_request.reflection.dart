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
  TestInlineFreeformAdditionalPropertiesRequest example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return TestInlineFreeformAdditionalPropertiesRequest(
      someProperty: () {
        PartReflection? _partReflection = _reflection.somePropertyPart;
        
        final disc = discriminators[r'someProperty'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return UndefinedWrapper(result);
          }
        }
        
        return UndefinedWrapper(


            
            


    
    exampleString()


);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class TestInlineFreeformAdditionalPropertiesRequestXmlReflection {
    const TestInlineFreeformAdditionalPropertiesRequestXmlReflection();
}

