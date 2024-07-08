// Model reflection

part of 'fake_any_of_w_ith_same_erasure_get200_response.dart';


//class reflection

class FakeAnyOfWIthSameErasureGet200ResponseReflection extends ClassReflection<FakeAnyOfWIthSameErasureGet200Response> {
  static FakeAnyOfWIthSameErasureGet200ResponseReflection instanceGetter() => instance;
  static const instance = FakeAnyOfWIthSameErasureGet200ResponseReflection._(
    modelName: r'_fake_anyOfWIthSameErasure_get_200_response',
    className: r'FakeAnyOfWIthSameErasureGet200Response',
    
    
    anyOf0Part: FakeAnyOfWIthSameErasureGet200ResponseAnyOf0(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FakeAnyOfWIthSameErasureGet200Response, 
            String

>(parentReflectionGetter: instanceGetter,),
          ),
    
    anyOf1Part: FakeAnyOfWIthSameErasureGet200ResponseAnyOf1(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FakeAnyOfWIthSameErasureGet200Response, 
            int

>(parentReflectionGetter: instanceGetter,),
          ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FakeAnyOfWIthSameErasureGet200Response, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const FakeAnyOfWIthSameErasureGet200ResponseReflection._({
    required this.modelName,
    required this.className,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.anyOf0Part,
    
    required this.anyOf1Part,
    
    required this.additionalPropertiesPart,
  });




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
  List<PropertyReflection<FakeAnyOfWIthSameErasureGet200Response, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<FakeAnyOfWIthSameErasureGet200Response, Object

?> additionalPropertiesPart;

  
  
  final FakeAnyOfWIthSameErasureGet200ResponseAnyOf0 anyOf0Part;
  
  final FakeAnyOfWIthSameErasureGet200ResponseAnyOf1 anyOf1Part;
  
  @override
  List<PartReflection<FakeAnyOfWIthSameErasureGet200Response, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<FakeAnyOfWIthSameErasureGet200Response, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<FakeAnyOfWIthSameErasureGet200Response, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<FakeAnyOfWIthSameErasureGet200Response, dynamic>> get anyOfs => [
    anyOf0Part,anyOf1Part,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => FakeAnyOfWIthSameErasureGet200Response.canDeserialize(src);
  @override
  FakeAnyOfWIthSameErasureGet200Response Function(Object? src) get deserializeFunction =>
      (src) => FakeAnyOfWIthSameErasureGet200Response.deserialize(src);

  @override
  Object? Function(FakeAnyOfWIthSameErasureGet200Response src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of FakeAnyOfWIthSameErasureGet200Response.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  FakeAnyOfWIthSameErasureGet200Response example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = FakeAnyOfWIthSameErasureGet200Response(
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    exampleResult.anyOf0 = anyOf0Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.anyOf1 = anyOf1Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    return exampleResult;
  }
}

class FakeAnyOfWIthSameErasureGet200ResponseAnyOf0 extends AnyOfReflection<FakeAnyOfWIthSameErasureGet200Response, 
    List<
        
            String

>
> {
  const FakeAnyOfWIthSameErasureGet200ResponseAnyOf0({
    super.classReflection,
    required FakeAnyOfWIthSameErasureGet200ResponseReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
    List<
        
            String

>
> example({required AggregatedDiscriminatorsResult discriminators, required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>> discriminatorExampleResults}) {
    if (discriminatorExampleResults.isNotEmpty) {
      if (!discriminatorExampleResults.values
          .any((e) => e.value == classReflection)) {
        return UndefinedWrapper.undefined();
      }
    }
    return UndefinedWrapper(
    exampleList(() { return 


            
            


    
    exampleString()


; })

);
  }
}
class FakeAnyOfWIthSameErasureGet200ResponseAnyOf1 extends AnyOfReflection<FakeAnyOfWIthSameErasureGet200Response, 
    List<
        
            int

>
> {
  const FakeAnyOfWIthSameErasureGet200ResponseAnyOf1({
    super.classReflection,
    required FakeAnyOfWIthSameErasureGet200ResponseReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
    List<
        
            int

>
> example({required AggregatedDiscriminatorsResult discriminators, required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>> discriminatorExampleResults}) {
    if (discriminatorExampleResults.isNotEmpty) {
      if (!discriminatorExampleResults.values
          .any((e) => e.value == classReflection)) {
        return UndefinedWrapper.undefined();
      }
    }
    return UndefinedWrapper(
    exampleList(() { return 


            
            


    
    exampleint()


; })

);
  }
}

class FakeAnyOfWIthSameErasureGet200ResponseXmlReflection {
    const FakeAnyOfWIthSameErasureGet200ResponseXmlReflection();
}

