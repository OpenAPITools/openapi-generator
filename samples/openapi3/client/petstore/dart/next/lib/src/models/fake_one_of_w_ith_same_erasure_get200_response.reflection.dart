// Model reflection

part of 'fake_one_of_w_ith_same_erasure_get200_response.dart';


//class reflection

class FakeOneOfWIthSameErasureGet200ResponseReflection extends ClassReflection<FakeOneOfWIthSameErasureGet200Response> {
  static FakeOneOfWIthSameErasureGet200ResponseReflection instanceGetter() => instance;
  static const instance = FakeOneOfWIthSameErasureGet200ResponseReflection._(
    modelName: r'_fake_oneOfWIthSameErasure_get_200_response',
    className: r'FakeOneOfWIthSameErasureGet200Response',
    
    
    oneOf0Part: FakeOneOfWIthSameErasureGet200ResponseOneOf0(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FakeOneOfWIthSameErasureGet200Response, 
            String

>(parentReflectionGetter: instanceGetter,),
          ),
    
    oneOf1Part: FakeOneOfWIthSameErasureGet200ResponseOneOf1(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FakeOneOfWIthSameErasureGet200Response, 
            int

>(parentReflectionGetter: instanceGetter,),
          ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FakeOneOfWIthSameErasureGet200Response, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const FakeOneOfWIthSameErasureGet200ResponseReflection._({
    required this.modelName,
    required this.className,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
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
  List<PropertyReflection<FakeOneOfWIthSameErasureGet200Response, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<FakeOneOfWIthSameErasureGet200Response, Object

?> additionalPropertiesPart;

  
  
  final FakeOneOfWIthSameErasureGet200ResponseOneOf0 oneOf0Part;
  
  final FakeOneOfWIthSameErasureGet200ResponseOneOf1 oneOf1Part;
  
  @override
  List<PartReflection<FakeOneOfWIthSameErasureGet200Response, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<FakeOneOfWIthSameErasureGet200Response, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<FakeOneOfWIthSameErasureGet200Response, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<FakeOneOfWIthSameErasureGet200Response, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => FakeOneOfWIthSameErasureGet200Response.canDeserialize(src);
  @override
  FakeOneOfWIthSameErasureGet200Response Function(Object? src) get deserializeFunction =>
      (src) => FakeOneOfWIthSameErasureGet200Response.deserialize(src);

  @override
  Object? Function(FakeOneOfWIthSameErasureGet200Response src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of FakeOneOfWIthSameErasureGet200Response.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  FakeOneOfWIthSameErasureGet200Response example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = FakeOneOfWIthSameErasureGet200Response(
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    exampleResult.oneOf0 = oneOf0Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.oneOf1 = oneOf1Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    return exampleResult;
  }
}


class FakeOneOfWIthSameErasureGet200ResponseOneOf0 extends OneOfReflection<FakeOneOfWIthSameErasureGet200Response, 
    List<
        
            String

>
> {
  const FakeOneOfWIthSameErasureGet200ResponseOneOf0({
    super.classReflection,
    required FakeOneOfWIthSameErasureGet200ResponseReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
    List<
        
            String

>
> example({required AggregatedDiscriminatorsResult discriminators, required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>> discriminatorExampleResults}) {
    if (discriminatorExampleResults.isEmpty) {
      //return undefined for non-first oneOfs.
      // An example SHOULD be generated
    } else {
      // if this reflection wasn't a result of any property, don't generate an example.

      if (!discriminatorExampleResults.values
          .any((e) => e.value == classReflection)) {
        // if there are no discriminator examples targetting the current class:
        return UndefinedWrapper.undefined();
      } else {
        // An example SHOULD be generated
      }
    }
    return UndefinedWrapper(
    exampleList(() { return 


            
            


    
    exampleString()


; })

);
  }
}

class FakeOneOfWIthSameErasureGet200ResponseOneOf1 extends OneOfReflection<FakeOneOfWIthSameErasureGet200Response, 
    List<
        
            int

>
> {
  const FakeOneOfWIthSameErasureGet200ResponseOneOf1({
    super.classReflection,
    required FakeOneOfWIthSameErasureGet200ResponseReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
    List<
        
            int

>
> example({required AggregatedDiscriminatorsResult discriminators, required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>> discriminatorExampleResults}) {
    if (discriminatorExampleResults.isEmpty) {
      //return undefined for non-first oneOfs.
      return UndefinedWrapper.undefined();
    } else {
      // if this reflection wasn't a result of any property, don't generate an example.

      if (!discriminatorExampleResults.values
          .any((e) => e.value == classReflection)) {
        // if there are no discriminator examples targetting the current class:
        return UndefinedWrapper.undefined();
      } else {
        // An example SHOULD be generated
      }
    }
    return UndefinedWrapper(
    exampleList(() { return 


            
            


    
    exampleint()


; })

);
  }
}

class FakeOneOfWIthSameErasureGet200ResponseXmlReflection {
    const FakeOneOfWIthSameErasureGet200ResponseXmlReflection();
}

