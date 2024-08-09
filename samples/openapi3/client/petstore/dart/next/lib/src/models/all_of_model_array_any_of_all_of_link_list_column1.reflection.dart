// Model reflection

part of 'all_of_model_array_any_of_all_of_link_list_column1.dart';


//class reflection

class AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection extends ClassReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1> {
  static AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection instanceGetter() => instance;
  static const instance = AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection._(
    modelName: r'AllOfModelArrayAnyOf_allOf_linkListColumn1',
    className: r'AllOfModelArrayAnyOfAllOfLinkListColumn1',
    valuePart: PropertyReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1, 
    List<
        
            AllOfModelArrayAnyOfAllOfLinkListColumn1Value

>

>(
      dartName: r'value',
      nullable: false,
      required: true,
      oasName: r'value',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1, 
            AllOfModelArrayAnyOfAllOfLinkListColumn1Value

>(parentReflectionGetter: instanceGetter,classReflection: AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection.instance,),
      getter: _valueGetter,
      setter: _valueSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection._({
    required this.modelName,
    required this.className,
    required this.valuePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1, 
    List<
        
            AllOfModelArrayAnyOfAllOfLinkListColumn1Value

>

> valuePart;
  static 
    List<
        
            AllOfModelArrayAnyOfAllOfLinkListColumn1Value

>

 _valueGetter(AllOfModelArrayAnyOfAllOfLinkListColumn1 parent) {
    return parent.value;
  }
  static void _valueSetter(AllOfModelArrayAnyOfAllOfLinkListColumn1 parent, 
    List<
        
            AllOfModelArrayAnyOfAllOfLinkListColumn1Value

>

 value) {
    parent.value = value;
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
  List<PropertyReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1, dynamic>> get properties => [
    valuePart,
  ];

  final AdditionalPropertiesReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => AllOfModelArrayAnyOfAllOfLinkListColumn1.canDeserialize(src);
  @override
  AllOfModelArrayAnyOfAllOfLinkListColumn1 Function(Object? src) get deserializeFunction =>
      (src) => AllOfModelArrayAnyOfAllOfLinkListColumn1.deserialize(src);

  @override
  Object? Function(AllOfModelArrayAnyOfAllOfLinkListColumn1 src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of AllOfModelArrayAnyOfAllOfLinkListColumn1.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  AllOfModelArrayAnyOfAllOfLinkListColumn1 example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = AllOfModelArrayAnyOfAllOfLinkListColumn1(
      value: () {
        var result = 


    exampleList(() { return 


            
            


    AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection.instance.example()
    


; })



;
        return result;
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class AllOfModelArrayAnyOfAllOfLinkListColumn1XmlReflection {
    const AllOfModelArrayAnyOfAllOfLinkListColumn1XmlReflection();
}

