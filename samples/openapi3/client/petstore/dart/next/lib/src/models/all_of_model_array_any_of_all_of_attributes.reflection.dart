// Model reflection

part of 'all_of_model_array_any_of_all_of_attributes.dart';


//class reflection

class AllOfModelArrayAnyOfAllOfAttributesReflection extends ClassReflection<AllOfModelArrayAnyOfAllOfAttributes> {
  static AllOfModelArrayAnyOfAllOfAttributesReflection instanceGetter() => instance;
  static const instance = AllOfModelArrayAnyOfAllOfAttributesReflection._(
    modelName: r'AllOfModelArrayAnyOf_allOf_attributes',
    className: r'AllOfModelArrayAnyOfAllOfAttributes',
    CPart: PropertyReflection<AllOfModelArrayAnyOfAllOfAttributes, UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributesC

>>(
      dartName: r'C',
      nullable: false,
      required: false,
      oasName: r'C',
      oasType: r'AllOfModelArrayAnyOfAllOfAttributesC',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      classReflection: AllOfModelArrayAnyOfAllOfAttributesCReflection.instance,
      getter: _CGetter,
      setter: _CSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<AllOfModelArrayAnyOfAllOfAttributes, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const AllOfModelArrayAnyOfAllOfAttributesReflection._({
    required this.modelName,
    required this.className,
    required this.CPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<AllOfModelArrayAnyOfAllOfAttributes, UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributesC

>> CPart;
  static UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributesC

> _CGetter(AllOfModelArrayAnyOfAllOfAttributes parent) {
    return parent.C;
  }
  static void _CSetter(AllOfModelArrayAnyOfAllOfAttributes parent, UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributesC

> value) {
    parent.C = value;
  }



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
  List<PropertyReflection<AllOfModelArrayAnyOfAllOfAttributes, dynamic>> get properties => [
    CPart,
  ];

  final AdditionalPropertiesReflection<AllOfModelArrayAnyOfAllOfAttributes, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<AllOfModelArrayAnyOfAllOfAttributes, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<AllOfModelArrayAnyOfAllOfAttributes, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => AllOfModelArrayAnyOfAllOfAttributes.canDeserialize(src);
  @override
  AllOfModelArrayAnyOfAllOfAttributes Function(Object? src) get deserializeFunction =>
      (src) => AllOfModelArrayAnyOfAllOfAttributes.deserialize(src);

  @override
  Object? Function(AllOfModelArrayAnyOfAllOfAttributes src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of AllOfModelArrayAnyOfAllOfAttributes.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  AllOfModelArrayAnyOfAllOfAttributes example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = AllOfModelArrayAnyOfAllOfAttributes(
      C: () {
        var result = 


            
            


    AllOfModelArrayAnyOfAllOfAttributesCReflection.instance.example()
    


;
        return UndefinedWrapper(result);
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class AllOfModelArrayAnyOfAllOfAttributesXmlReflection {
    const AllOfModelArrayAnyOfAllOfAttributesXmlReflection();
}

