// Model reflection

part of 'value.dart';


//class reflection

class ValueReflection extends ClassReflection<Value> {
  static ValueReflection instanceGetter() => instance;
  static const instance = ValueReflection._(
    modelName: r'Value',
    className: r'Value',
    
    
    oneOf0Part: ValueOneOf0(
      parentReflectionGetter: instanceGetter,
      classReflection: ScalarReflection.instance,
    ),
    
    oneOf1Part: ValueOneOf1(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Value, 
            Scalar

>(parentReflectionGetter: instanceGetter,classReflection: ScalarReflection.instance,),
          ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Value, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ValueReflection._({
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
  List<PropertyReflection<Value, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<Value, Object

?> additionalPropertiesPart;

  
  
  final ValueOneOf0 oneOf0Part;
  
  final ValueOneOf1 oneOf1Part;
  
  @override
  List<PartReflection<Value, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Value, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Value, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<Value, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Value.canDeserialize(src);
  @override
  Value Function(Object? src) get deserializeFunction =>
      (src) => Value.deserialize(src);

  @override
  Object? Function(Value src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Value.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Value example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = Value(
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    exampleResult.oneOf0 = oneOf0Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.oneOf1 = oneOf1Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    return exampleResult;
  }
}


class ValueOneOf0 extends OneOfReflection<Value, 
            Scalar
> {
  const ValueOneOf0({
    super.classReflection,
    required ValueReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            Scalar
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
            
            


    ScalarReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class ValueOneOf1 extends OneOfReflection<Value, 
    List<
        
            Scalar

>
> {
  const ValueOneOf1({
    super.classReflection,
    required ValueReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
    List<
        
            Scalar

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


            
            


    ScalarReflection.instance.example()
    


; })

);
  }
}

class ValueXmlReflection {
    const ValueXmlReflection();
}

