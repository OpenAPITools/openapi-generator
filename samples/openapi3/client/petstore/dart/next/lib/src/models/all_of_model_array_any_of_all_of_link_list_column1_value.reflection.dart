// Model reflection

part of 'all_of_model_array_any_of_all_of_link_list_column1_value.dart';


//class reflection

class AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection extends ClassReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value> {
  static AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection instanceGetter() => instance;
  static const instance = AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection._(
    modelName: r'AllOfModelArrayAnyOf_allOf_linkListColumn1_value',
    className: r'AllOfModelArrayAnyOfAllOfLinkListColumn1Value',
    
    
    anyOf0Part: AllOfModelArrayAnyOfAllOfLinkListColumn1ValueAnyOf0(
      parentReflectionGetter: instanceGetter,
      classReflection: UserReflection.instance,
    ),
    
    anyOf1Part: AllOfModelArrayAnyOfAllOfLinkListColumn1ValueAnyOf1(
      parentReflectionGetter: instanceGetter,
      classReflection: TagReflection.instance,
    ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection._({
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
  List<PropertyReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, Object

?> additionalPropertiesPart;

  
  
  final AllOfModelArrayAnyOfAllOfLinkListColumn1ValueAnyOf0 anyOf0Part;
  
  final AllOfModelArrayAnyOfAllOfLinkListColumn1ValueAnyOf1 anyOf1Part;
  
  @override
  List<PartReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, dynamic>> get anyOfs => [
    anyOf0Part,anyOf1Part,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => AllOfModelArrayAnyOfAllOfLinkListColumn1Value.canDeserialize(src);
  @override
  AllOfModelArrayAnyOfAllOfLinkListColumn1Value Function(Object? src) get deserializeFunction =>
      (src) => AllOfModelArrayAnyOfAllOfLinkListColumn1Value.deserialize(src);

  @override
  Object? Function(AllOfModelArrayAnyOfAllOfLinkListColumn1Value src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of AllOfModelArrayAnyOfAllOfLinkListColumn1Value.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  AllOfModelArrayAnyOfAllOfLinkListColumn1Value example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = AllOfModelArrayAnyOfAllOfLinkListColumn1Value(
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    exampleResult.anyOf0 = anyOf0Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.anyOf1 = anyOf1Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    return exampleResult;
  }
}

class AllOfModelArrayAnyOfAllOfLinkListColumn1ValueAnyOf0 extends AnyOfReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, 
            User
> {
  const AllOfModelArrayAnyOfAllOfLinkListColumn1ValueAnyOf0({
    super.classReflection,
    required AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            User
> example({required AggregatedDiscriminatorsResult discriminators, required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>> discriminatorExampleResults}) {
    if (discriminatorExampleResults.isNotEmpty) {
      if (!discriminatorExampleResults.values
          .any((e) => e.value == classReflection)) {
        return UndefinedWrapper.undefined();
      }
    }
    return UndefinedWrapper(
            
            


    UserReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}
class AllOfModelArrayAnyOfAllOfLinkListColumn1ValueAnyOf1 extends AnyOfReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value, 
            Tag
> {
  const AllOfModelArrayAnyOfAllOfLinkListColumn1ValueAnyOf1({
    super.classReflection,
    required AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            Tag
> example({required AggregatedDiscriminatorsResult discriminators, required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>> discriminatorExampleResults}) {
    if (discriminatorExampleResults.isNotEmpty) {
      if (!discriminatorExampleResults.values
          .any((e) => e.value == classReflection)) {
        return UndefinedWrapper.undefined();
      }
    }
    return UndefinedWrapper(
            
            


    TagReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class AllOfModelArrayAnyOfAllOfLinkListColumn1ValueXmlReflection {
    const AllOfModelArrayAnyOfAllOfLinkListColumn1ValueXmlReflection();
}

