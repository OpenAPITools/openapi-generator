// Model reflection

part of 'number_only.dart';


//class reflection

class NumberOnlyReflection extends ClassReflection<NumberOnly> {
  static NumberOnlyReflection instanceGetter() => instance;
  static const instance = NumberOnlyReflection._(
    modelName: r'NumberOnly',
    className: r'NumberOnly',
    justNumberPart: PropertyReflection<NumberOnly, UndefinedWrapper<
            num

>>(
      dartName: r'justNumber',
      nullable: false,
      required: false,
      oasName: r'JustNumber',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _justNumberGetter,
      setter: _justNumberSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<NumberOnly, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const NumberOnlyReflection._({
    required this.modelName,
    required this.className,
    required this.justNumberPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<NumberOnly, UndefinedWrapper<
            num

>> justNumberPart;
  static UndefinedWrapper<
            num

> _justNumberGetter(NumberOnly parent) {
    return parent.justNumber;
  }
  static void _justNumberSetter(NumberOnly parent, UndefinedWrapper<
            num

> value) {
    parent.justNumber = value;
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
  List<PropertyReflection<NumberOnly, dynamic>> get properties => [
    justNumberPart,
  ];

  final AdditionalPropertiesReflection<NumberOnly, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<NumberOnly, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<NumberOnly, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => NumberOnly.canDeserialize(src);
  @override
  NumberOnly Function(Object? src) get deserializeFunction =>
      (src) => NumberOnly.deserialize(src);

  @override
  Object? Function(NumberOnly src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of NumberOnly.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  NumberOnly example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = NumberOnly(
      justNumber: () {
        var result = 


            
            


    
    examplenum()


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


class NumberOnlyXmlReflection {
    const NumberOnlyXmlReflection();
}

