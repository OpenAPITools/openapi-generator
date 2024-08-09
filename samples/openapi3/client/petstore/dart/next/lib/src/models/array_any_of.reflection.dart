// Model reflection

part of 'array_any_of.dart';


//class reflection

class ArrayAnyOfReflection extends ClassReflection<ArrayAnyOf> {
  static ArrayAnyOfReflection instanceGetter() => instance;
  static const instance = ArrayAnyOfReflection._(
    modelName: r'ArrayAnyOf',
    className: r'ArrayAnyOf',
    
    
    anyOf0Part: ArrayAnyOfAnyOf0(
      parentReflectionGetter: instanceGetter,
          ),
    
    anyOf1Part: ArrayAnyOfAnyOf1(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ArrayAnyOf, 
            String

>(parentReflectionGetter: instanceGetter,),
          ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ArrayAnyOf, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ArrayAnyOfReflection._({
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
  List<PropertyReflection<ArrayAnyOf, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<ArrayAnyOf, Object

?> additionalPropertiesPart;

  
  
  final ArrayAnyOfAnyOf0 anyOf0Part;
  
  final ArrayAnyOfAnyOf1 anyOf1Part;
  
  @override
  List<PartReflection<ArrayAnyOf, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ArrayAnyOf, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<ArrayAnyOf, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<ArrayAnyOf, dynamic>> get anyOfs => [
    anyOf0Part,anyOf1Part,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ArrayAnyOf.canDeserialize(src);
  @override
  ArrayAnyOf Function(Object? src) get deserializeFunction =>
      (src) => ArrayAnyOf.deserialize(src);

  @override
  Object? Function(ArrayAnyOf src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ArrayAnyOf.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ArrayAnyOf example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = ArrayAnyOf(
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    exampleResult.anyOf0 = anyOf0Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.anyOf1 = anyOf1Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    return exampleResult;
  }
}

class ArrayAnyOfAnyOf0 extends AnyOfReflection<ArrayAnyOf, 
            int
> {
  const ArrayAnyOfAnyOf0({
    super.classReflection,
    required ArrayAnyOfReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            int
> example({required AggregatedDiscriminatorsResult discriminators, required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>> discriminatorExampleResults}) {
    if (discriminatorExampleResults.isNotEmpty) {
      if (!discriminatorExampleResults.values
          .any((e) => e.value == classReflection)) {
        return UndefinedWrapper.undefined();
      }
    }
    return UndefinedWrapper(
            
            


    
    exampleint()
);
  }
}
class ArrayAnyOfAnyOf1 extends AnyOfReflection<ArrayAnyOf, 
    List<
        
            String

>
> {
  const ArrayAnyOfAnyOf1({
    super.classReflection,
    required ArrayAnyOfReflection Function() super.parentReflectionGetter,
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

class ArrayAnyOfXmlReflection {
    const ArrayAnyOfXmlReflection();
}

