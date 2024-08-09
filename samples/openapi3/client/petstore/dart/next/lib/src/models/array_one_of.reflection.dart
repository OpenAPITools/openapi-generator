// Model reflection

part of 'array_one_of.dart';


//class reflection

class ArrayOneOfReflection extends ClassReflection<ArrayOneOf> {
  static ArrayOneOfReflection instanceGetter() => instance;
  static const instance = ArrayOneOfReflection._(
    modelName: r'ArrayOneOf',
    className: r'ArrayOneOf',
    
    
    oneOf0Part: ArrayOneOfOneOf0(
      parentReflectionGetter: instanceGetter,
          ),
    
    oneOf1Part: ArrayOneOfOneOf1(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ArrayOneOf, 
            String

>(parentReflectionGetter: instanceGetter,),
          ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ArrayOneOf, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ArrayOneOfReflection._({
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
  List<PropertyReflection<ArrayOneOf, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<ArrayOneOf, Object

?> additionalPropertiesPart;

  
  
  final ArrayOneOfOneOf0 oneOf0Part;
  
  final ArrayOneOfOneOf1 oneOf1Part;
  
  @override
  List<PartReflection<ArrayOneOf, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ArrayOneOf, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<ArrayOneOf, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<ArrayOneOf, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ArrayOneOf.canDeserialize(src);
  @override
  ArrayOneOf Function(Object? src) get deserializeFunction =>
      (src) => ArrayOneOf.deserialize(src);

  @override
  Object? Function(ArrayOneOf src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ArrayOneOf.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ArrayOneOf example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = ArrayOneOf(
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    exampleResult.oneOf0 = oneOf0Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.oneOf1 = oneOf1Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    return exampleResult;
  }
}


class ArrayOneOfOneOf0 extends OneOfReflection<ArrayOneOf, 
            int
> {
  const ArrayOneOfOneOf0({
    super.classReflection,
    required ArrayOneOfReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            int
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
            
            


    
    exampleint()
);
  }
}

class ArrayOneOfOneOf1 extends OneOfReflection<ArrayOneOf, 
    List<
        
            String

>
> {
  const ArrayOneOfOneOf1({
    super.classReflection,
    required ArrayOneOfReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
    List<
        
            String

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


            
            


    
    exampleString()


; })

);
  }
}

class ArrayOneOfXmlReflection {
    const ArrayOneOfXmlReflection();
}

