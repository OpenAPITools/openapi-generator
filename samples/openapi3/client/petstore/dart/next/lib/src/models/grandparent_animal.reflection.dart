// Model reflection

part of 'grandparent_animal.dart';


//class reflection

class GrandparentAnimalReflection extends ClassReflection<GrandparentAnimal> {
  static GrandparentAnimalReflection instanceGetter() => instance;
  static const instance = GrandparentAnimalReflection._(
    modelName: r'GrandparentAnimal',
    className: r'GrandparentAnimal',
    petTypePart: PropertyReflection<GrandparentAnimal, 
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
      itemsReflection: ItemsReflection<GrandparentAnimal, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const GrandparentAnimalReflection._({
    required this.modelName,
    required this.className,
    required this.petTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<GrandparentAnimal, 
            String
> petTypePart;
  static 
            String
 _petTypeGetter(GrandparentAnimal parent) {
    return parent.petType;
  }
  static void _petTypeSetter(GrandparentAnimal parent, 
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
  List<PropertyReflection<GrandparentAnimal, dynamic>> get properties => [
    petTypePart,
  ];

  final AdditionalPropertiesReflection<GrandparentAnimal, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<GrandparentAnimal, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<GrandparentAnimal, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => GrandparentAnimal.canDeserialize(src);
  @override
  GrandparentAnimal Function(Object? src) get deserializeFunction =>
      (src) => GrandparentAnimal.deserialize(src);

  @override
  Object? Function(GrandparentAnimal src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of GrandparentAnimal.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  GrandparentAnimal example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return GrandparentAnimal(
      petType: () {
        PartReflection? _partReflection = _reflection.petTypePart;
        
        final disc = discriminators[r'pet_type'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return result;
          }
        }
        
        return 


            
            


    
    exampleString()


;
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class GrandparentAnimalXmlReflection {
    const GrandparentAnimalXmlReflection();
}

