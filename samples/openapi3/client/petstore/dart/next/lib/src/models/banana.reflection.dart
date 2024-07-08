// Model reflection

part of 'banana.dart';


//class reflection

class BananaReflection extends ClassReflection<Banana> {
  static BananaReflection instanceGetter() => instance;
  static const instance = BananaReflection._(
    modelName: r'banana',
    className: r'Banana',
    lengthCmPart: PropertyReflection<Banana, 
            num

>(
      dartName: r'lengthCm',
      nullable: false,
      required: true,
      oasName: r'lengthCm',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _lengthCmGetter,
      setter: _lengthCmSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Banana, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const BananaReflection._({
    required this.modelName,
    required this.className,
    required this.lengthCmPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Banana, 
            num

> lengthCmPart;
  static 
            num

 _lengthCmGetter(Banana parent) {
    return parent.lengthCm;
  }
  static void _lengthCmSetter(Banana parent, 
            num

 value) {
    parent.lengthCm = value;
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
  List<PropertyReflection<Banana, dynamic>> get properties => [
    lengthCmPart,
  ];

  final AdditionalPropertiesReflection<Banana, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<Banana, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Banana, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Banana.canDeserialize(src);
  @override
  Banana Function(Object? src) get deserializeFunction =>
      (src) => Banana.deserialize(src);

  @override
  Object? Function(Banana src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Banana.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Banana example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = Banana(
      lengthCm: () {
        var result = 


            
            


    
    examplenum()


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


class BananaXmlReflection {
    const BananaXmlReflection();
}

