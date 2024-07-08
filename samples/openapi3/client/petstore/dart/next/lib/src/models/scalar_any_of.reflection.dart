// Model reflection

part of 'scalar_any_of.dart';


//class reflection

class ScalarAnyOfReflection extends ClassReflection<ScalarAnyOf> {
  static ScalarAnyOfReflection instanceGetter() => instance;
  static const instance = ScalarAnyOfReflection._(
    modelName: r'ScalarAnyOf',
    className: r'ScalarAnyOf',
    
    
    anyOf0Part: ScalarAnyOfAnyOf0(
      parentReflectionGetter: instanceGetter,
          ),
    
    anyOf1Part: ScalarAnyOfAnyOf1(
      parentReflectionGetter: instanceGetter,
          ),
    
    anyOf2Part: ScalarAnyOfAnyOf2(
      parentReflectionGetter: instanceGetter,
          ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ScalarAnyOf, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ScalarAnyOfReflection._({
    required this.modelName,
    required this.className,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.anyOf0Part,
    
    required this.anyOf1Part,
    
    required this.anyOf2Part,
    
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
  List<PropertyReflection<ScalarAnyOf, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<ScalarAnyOf, Object

?> additionalPropertiesPart;

  
  
  final ScalarAnyOfAnyOf0 anyOf0Part;
  
  final ScalarAnyOfAnyOf1 anyOf1Part;
  
  final ScalarAnyOfAnyOf2 anyOf2Part;
  
  @override
  List<PartReflection<ScalarAnyOf, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ScalarAnyOf, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<ScalarAnyOf, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<ScalarAnyOf, dynamic>> get anyOfs => [
    anyOf0Part,anyOf1Part,anyOf2Part,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ScalarAnyOf.canDeserialize(src);
  @override
  ScalarAnyOf Function(Object? src) get deserializeFunction =>
      (src) => ScalarAnyOf.deserialize(src);

  @override
  Object? Function(ScalarAnyOf src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ScalarAnyOf.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ScalarAnyOf example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = ScalarAnyOf(
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    exampleResult.anyOf0 = anyOf0Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.anyOf1 = anyOf1Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.anyOf2 = anyOf2Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    return exampleResult;
  }
}

class ScalarAnyOfAnyOf0 extends AnyOfReflection<ScalarAnyOf, 
            String
> {
  const ScalarAnyOfAnyOf0({
    super.classReflection,
    required ScalarAnyOfReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            String
> example({required AggregatedDiscriminatorsResult discriminators, required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>> discriminatorExampleResults}) {
    if (discriminatorExampleResults.isNotEmpty) {
      if (!discriminatorExampleResults.values
          .any((e) => e.value == classReflection)) {
        return UndefinedWrapper.undefined();
      }
    }
    return UndefinedWrapper(
            
            


    
    exampleString()
);
  }
}
class ScalarAnyOfAnyOf1 extends AnyOfReflection<ScalarAnyOf, 
            num
> {
  const ScalarAnyOfAnyOf1({
    super.classReflection,
    required ScalarAnyOfReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            num
> example({required AggregatedDiscriminatorsResult discriminators, required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>> discriminatorExampleResults}) {
    if (discriminatorExampleResults.isNotEmpty) {
      if (!discriminatorExampleResults.values
          .any((e) => e.value == classReflection)) {
        return UndefinedWrapper.undefined();
      }
    }
    return UndefinedWrapper(
            
            


    
    examplenum()
);
  }
}
class ScalarAnyOfAnyOf2 extends AnyOfReflection<ScalarAnyOf, 
            bool
> {
  const ScalarAnyOfAnyOf2({
    super.classReflection,
    required ScalarAnyOfReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            bool
> example({required AggregatedDiscriminatorsResult discriminators, required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>> discriminatorExampleResults}) {
    if (discriminatorExampleResults.isNotEmpty) {
      if (!discriminatorExampleResults.values
          .any((e) => e.value == classReflection)) {
        return UndefinedWrapper.undefined();
      }
    }
    return UndefinedWrapper(
            
            


    
    examplebool()
);
  }
}

class ScalarAnyOfXmlReflection {
    const ScalarAnyOfXmlReflection();
}

