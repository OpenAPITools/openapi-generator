// Model reflection

part of 'parent_pet.dart';


//class reflection

class ParentPetReflection extends ModelReflection<ParentPet> {
  static ParentPetReflection instanceGetter() => instance;
  static const instance = ParentPetReflection._(
    modelName: r'ParentPet',
    className: r'ParentPet',
    xml: const XmlReflection(
),
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
      xml: const XmlReflection(
),
      getter: FunctionWrapper1(_petTypeGetter),
      setter: FunctionWrapper2(_petTypeSetter),
      reflection: 
            
        
        
            
                PrimitiveReflection.forString
        
,
    ),
    discriminatorKey: r'pet_type',
    discriminatorImplicitMappings: const {
      r'GrandparentAnimal': GrandparentAnimalReflection.instance,
    },
    discriminatorMappings: const {
    },
    allOfGrandparentAnimalPart: AllOfReflection(
      parentReflectionGetter: instanceGetter,
      reflection: GrandparentAnimalReflection.instance,
    ),

    
    additionalPropertiesPart: AdditionalPropertiesPart(
      parentReflectionGetter: instanceGetter,
      itemReflection: NullableReflection(ObjectReflection()
),
      getter: FunctionWrapper1(_AdditionalPropertiesGetter),
      setter: FunctionWrapper2(_AdditionalPropertiesSetter),
    ),
  );
  const ParentPetReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.petTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
    required this.allOfGrandparentAnimalPart,
    
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
  final Map<String, ModelReflection> discriminatorMappings;
  @override
  final Map<String, ModelReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;
  @override
  final XmlReflection xml;

  @override
  List<PropertyReflection<ParentPet, dynamic>> get properties => [
    petTypePart,
  ];

  @override
  final AdditionalPropertiesPart<ParentPet, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ParentPet instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ParentPet instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  final AllOfReflection<ParentPet, GrandparentAnimalMixin> allOfGrandparentAnimalPart;

  

  @override
  List<AllOfReflection<ParentPet, dynamic>> get allOfs => [
    allOfGrandparentAnimalPart,
  ];

  @override
  List<OneOfReflection<ParentPet, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<ParentPet, dynamic>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ParentPet empty() {
    return ParentPet(
      petType: petTypePart.reflection.emptyFunction(),
    );
  }
}


