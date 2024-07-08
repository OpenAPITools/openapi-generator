// Model reflection

part of 'parent_pet.dart';


//class reflection

class ParentPetReflection extends ClassReflection<ParentPet> {
  static ParentPetReflection instanceGetter() => instance;
  static const instance = ParentPetReflection._(
    modelName: r'ParentPet',
    className: r'ParentPet',
    petTypePart: PropertyReflection<ParentPet, 
            String

>(
      dartName: r'petType',
      nullable: false,
      required: true,
      oasName: r'pet_type',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: true,
      getter: _petTypeGetter,
      setter: _petTypeSetter,
    ),
    discriminatorKey: r'pet_type',
    discriminatorImplicitMappings: const {
    },
    discriminatorMappings: const {
    },
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ParentPet, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ParentPetReflection._({
    required this.modelName,
    required this.className,
    required this.petTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ParentPet, 
            String

> petTypePart;
  static 
            String

 _petTypeGetter(ParentPet parent) {
    return parent.petType;
  }
  static void _petTypeSetter(ParentPet parent, 
            String

 value) {
    parent.petType = value;
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
  List<PropertyReflection<ParentPet, dynamic>> get properties => [
    petTypePart,
  ];

  final AdditionalPropertiesReflection<ParentPet, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<ParentPet, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ParentPet, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<ParentPet, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<ParentPet, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ParentPet.canDeserialize(src);
  @override
  ParentPet Function(Object? src) get deserializeFunction =>
      (src) => ParentPet.deserialize(src);

  @override
  Object? Function(ParentPet src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ParentPet.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ParentPet example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = ParentPet(
      petType: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[petTypePart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return result;
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class ParentPetXmlReflection {
    const ParentPetXmlReflection();
}

