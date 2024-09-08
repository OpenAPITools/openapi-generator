// Model reflection

part of 'grandparent_animal.dart';


//class reflection

class GrandparentAnimalReflection extends ModelReflection<GrandparentAnimal> {
  static GrandparentAnimalReflection instanceGetter() => instance;
  static const instance = GrandparentAnimalReflection._(
    modelName: r'GrandparentAnimal',
    className: r'GrandparentAnimal',
    xml: XmlReflection(
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_petTypeGetter),
      setter: FunctionWrapper2(_petTypeSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
,
    ),
    discriminatorKey: r'pet_type',
    discriminatorImplicitMappings: const {
    },
    discriminatorMappings: const {
    },
    
    
    additionalPropertiesPart: AdditionalPropertiesPart(
      parentReflectionGetter: instanceGetter,
      itemReflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(ObjectReflection()
),
)
,
      getter: FunctionWrapper1(_AdditionalPropertiesGetter),
      setter: FunctionWrapper2(_AdditionalPropertiesSetter),
    ),
  );
  const GrandparentAnimalReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
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
  List<PropertyReflection<GrandparentAnimal, dynamic>> get properties => [
    petTypePart,
  ];

  @override
  final AdditionalPropertiesPart<GrandparentAnimal, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(GrandparentAnimal instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(GrandparentAnimal instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<GrandparentAnimal, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  GrandparentAnimal empty() {
    return GrandparentAnimal(
      petType: petTypePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is GrandparentAnimalReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


