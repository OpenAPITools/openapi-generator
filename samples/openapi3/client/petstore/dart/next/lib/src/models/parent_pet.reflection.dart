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
  ParentPet example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return ParentPet(
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

class ParentPetXmlReflection {
    const ParentPetXmlReflection();
}

