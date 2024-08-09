// Model reflection

part of 'free_form_object_test_class_properties.dart';


//class reflection

class FreeFormObjectTestClassPropertiesReflection extends ClassReflection<FreeFormObjectTestClassProperties> {
  static FreeFormObjectTestClassPropertiesReflection instanceGetter() => instance;
  static const instance = FreeFormObjectTestClassPropertiesReflection._(
    modelName: r'FreeFormObjectTestClass_properties',
    className: r'FreeFormObjectTestClassProperties',
    
    
    oneOf0Part: FreeFormObjectTestClassPropertiesOneOf0(
      parentReflectionGetter: instanceGetter,
          ),
    
    oneOf1Part: FreeFormObjectTestClassPropertiesOneOf1(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FreeFormObjectTestClassProperties, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FreeFormObjectTestClassProperties, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const FreeFormObjectTestClassPropertiesReflection._({
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
  List<PropertyReflection<FreeFormObjectTestClassProperties, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<FreeFormObjectTestClassProperties, Object

?> additionalPropertiesPart;

  
  
  final FreeFormObjectTestClassPropertiesOneOf0 oneOf0Part;
  
  final FreeFormObjectTestClassPropertiesOneOf1 oneOf1Part;
  
  @override
  List<PartReflection<FreeFormObjectTestClassProperties, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<FreeFormObjectTestClassProperties, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<FreeFormObjectTestClassProperties, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<FreeFormObjectTestClassProperties, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => FreeFormObjectTestClassProperties.canDeserialize(src);
  @override
  FreeFormObjectTestClassProperties Function(Object? src) get deserializeFunction =>
      (src) => FreeFormObjectTestClassProperties.deserialize(src);

  @override
  Object? Function(FreeFormObjectTestClassProperties src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of FreeFormObjectTestClassProperties.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  FreeFormObjectTestClassProperties example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = FreeFormObjectTestClassProperties(
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    exampleResult.oneOf0 = oneOf0Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.oneOf1 = oneOf1Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    return exampleResult;
  }
}


class FreeFormObjectTestClassPropertiesOneOf0 extends OneOfReflection<FreeFormObjectTestClassProperties, 
            String
> {
  const FreeFormObjectTestClassPropertiesOneOf0({
    super.classReflection,
    required FreeFormObjectTestClassPropertiesReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            String
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
            
            


    
    exampleString()
);
  }
}

class FreeFormObjectTestClassPropertiesOneOf1 extends OneOfReflection<FreeFormObjectTestClassProperties, 
    Map<String, 
        Object

?>
> {
  const FreeFormObjectTestClassPropertiesOneOf1({
    super.classReflection,
    required FreeFormObjectTestClassPropertiesReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
    Map<String, 
        Object

?>
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
    exampleMap(() { return exampleNullable(() =>

exampleObject()



 ) ; })

);
  }
}

class FreeFormObjectTestClassPropertiesXmlReflection {
    const FreeFormObjectTestClassPropertiesXmlReflection();
}

