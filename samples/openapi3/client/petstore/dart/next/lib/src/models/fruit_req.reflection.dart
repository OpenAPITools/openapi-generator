// Model reflection

part of 'fruit_req.dart';


//class reflection

class FruitReqReflection extends ClassReflection<FruitReq> {
  static FruitReqReflection instanceGetter() => instance;
  static const instance = FruitReqReflection._(
    modelName: r'fruitReq',
    className: r'FruitReq',
    
    
    oneOf0Part: FruitReqOneOf0(
      parentReflectionGetter: instanceGetter,
      classReflection: AppleReqReflection.instance,
    ),
    
    oneOf1Part: FruitReqOneOf1(
      parentReflectionGetter: instanceGetter,
      classReflection: BananaReqReflection.instance,
    ),
    
  );
  const FruitReqReflection._({
    required this.modelName,
    required this.className,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
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
  List<PropertyReflection<FruitReq, dynamic>> get properties => [
      ];

  
  
  
  final FruitReqOneOf0 oneOf0Part;
  
  final FruitReqOneOf1 oneOf1Part;
  
  @override
  List<PartReflection<FruitReq, dynamic>> get parts => [
    ...super.parts,
      ];
  @override
  List<AllOfReflection<FruitReq, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<FruitReq, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<FruitReq, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => FruitReq.canDeserialize(src);
  @override
  FruitReq Function(Object? src) get deserializeFunction =>
      (src) => FruitReq.deserialize(src);

  @override
  Object? Function(FruitReq src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of FruitReq.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  FruitReq example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = FruitReq(
      
    );
    
    exampleResult.oneOf0 = oneOf0Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.oneOf1 = oneOf1Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    return exampleResult;
  }
}


class FruitReqOneOf0 extends OneOfReflection<FruitReq, 
            AppleReq
> {
  const FruitReqOneOf0({
    super.classReflection,
    required FruitReqReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            AppleReq
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
            
            


    AppleReqReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class FruitReqOneOf1 extends OneOfReflection<FruitReq, 
            BananaReq
> {
  const FruitReqOneOf1({
    super.classReflection,
    required FruitReqReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            BananaReq
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
            
            


    BananaReqReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class FruitReqXmlReflection {
    const FruitReqXmlReflection();
}

