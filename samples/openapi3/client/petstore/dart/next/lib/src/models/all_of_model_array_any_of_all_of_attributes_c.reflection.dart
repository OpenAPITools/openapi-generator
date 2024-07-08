// Model reflection

part of 'all_of_model_array_any_of_all_of_attributes_c.dart';


//class reflection

class AllOfModelArrayAnyOfAllOfAttributesCReflection extends ClassReflection<AllOfModelArrayAnyOfAllOfAttributesC> {
  static AllOfModelArrayAnyOfAllOfAttributesCReflection instanceGetter() => instance;
  static const instance = AllOfModelArrayAnyOfAllOfAttributesCReflection._(
    modelName: r'AllOfModelArrayAnyOf_allOf_attributes_C',
    className: r'AllOfModelArrayAnyOfAllOfAttributesC',
    
    
    oneOf0Part: AllOfModelArrayAnyOfAllOfAttributesCOneOf0(
      parentReflectionGetter: instanceGetter,
      classReflection: PetReflection.instance,
    ),
    
    oneOf1Part: AllOfModelArrayAnyOfAllOfAttributesCOneOf1(
      parentReflectionGetter: instanceGetter,
      classReflection: OrderReflection.instance,
    ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<AllOfModelArrayAnyOfAllOfAttributesC, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const AllOfModelArrayAnyOfAllOfAttributesCReflection._({
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
  List<PropertyReflection<AllOfModelArrayAnyOfAllOfAttributesC, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<AllOfModelArrayAnyOfAllOfAttributesC, Object

?> additionalPropertiesPart;

  
  
  final AllOfModelArrayAnyOfAllOfAttributesCOneOf0 oneOf0Part;
  
  final AllOfModelArrayAnyOfAllOfAttributesCOneOf1 oneOf1Part;
  
  @override
  List<PartReflection<AllOfModelArrayAnyOfAllOfAttributesC, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<AllOfModelArrayAnyOfAllOfAttributesC, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<AllOfModelArrayAnyOfAllOfAttributesC, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<AllOfModelArrayAnyOfAllOfAttributesC, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => AllOfModelArrayAnyOfAllOfAttributesC.canDeserialize(src);
  @override
  AllOfModelArrayAnyOfAllOfAttributesC Function(Object? src) get deserializeFunction =>
      (src) => AllOfModelArrayAnyOfAllOfAttributesC.deserialize(src);

  @override
  Object? Function(AllOfModelArrayAnyOfAllOfAttributesC src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of AllOfModelArrayAnyOfAllOfAttributesC.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  AllOfModelArrayAnyOfAllOfAttributesC example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = AllOfModelArrayAnyOfAllOfAttributesC(
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    exampleResult.oneOf0 = oneOf0Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.oneOf1 = oneOf1Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    return exampleResult;
  }
}


class AllOfModelArrayAnyOfAllOfAttributesCOneOf0 extends OneOfReflection<AllOfModelArrayAnyOfAllOfAttributesC, 
            Pet
> {
  const AllOfModelArrayAnyOfAllOfAttributesCOneOf0({
    super.classReflection,
    required AllOfModelArrayAnyOfAllOfAttributesCReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            Pet
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
            
            


    PetReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class AllOfModelArrayAnyOfAllOfAttributesCOneOf1 extends OneOfReflection<AllOfModelArrayAnyOfAllOfAttributesC, 
            Order
> {
  const AllOfModelArrayAnyOfAllOfAttributesCOneOf1({
    super.classReflection,
    required AllOfModelArrayAnyOfAllOfAttributesCReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            Order
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
            
            


    OrderReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class AllOfModelArrayAnyOfAllOfAttributesCXmlReflection {
    const AllOfModelArrayAnyOfAllOfAttributesCXmlReflection();
}

