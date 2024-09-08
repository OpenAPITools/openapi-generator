// Model reflection

part of 'dog.dart';


//class reflection

class DogReflection extends ModelReflection<Dog> {
  static DogReflection instanceGetter() => instance;
  static const instance = DogReflection._(
    modelName: r'Dog',
    className: r'Dog',
    xml: XmlReflection(
),
    colorPart: PropertyReflection<Dog, UndefinedWrapper<
            String
>>(
      dartName: r'color',
      nullable: false,
      required: false,
      oasName: r'color',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_colorGetter),
      setter: FunctionWrapper2(_colorSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    breedPart: PropertyReflection<Dog, UndefinedWrapper<
            String
>>(
      dartName: r'breed',
      nullable: false,
      required: false,
      oasName: r'breed',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_breedGetter),
      setter: FunctionWrapper2(_breedSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    classNamePart: PropertyReflection<Dog, 
            String
>(
      dartName: r'className',
      nullable: false,
      required: true,
      oasName: r'className',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: true,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_classNameGetter),
      setter: FunctionWrapper2(_classNameSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
,
    ),
    discriminatorKey: r'className',
    discriminatorImplicitMappings: const {
      r'Animal': AnimalReflection.instance,
    },
    discriminatorMappings: const {
    },
    allOfAnimalPart: AllOfReflection(
      parentReflectionGetter: instanceGetter,
      reflection: AnimalReflection.instance,
    ),

    
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
  const DogReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.colorPart,
    required this.breedPart,
    required this.classNamePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
    required this.allOfAnimalPart,
    
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Dog, UndefinedWrapper<
            String
>> colorPart;
  static UndefinedWrapper<
            String
> _colorGetter(Dog parent) {
    return parent.color;
  }
  static void _colorSetter(Dog parent, UndefinedWrapper<
            String
> value) {
    parent.color = value;
  }

  final PropertyReflection<Dog, UndefinedWrapper<
            String
>> breedPart;
  static UndefinedWrapper<
            String
> _breedGetter(Dog parent) {
    return parent.breed;
  }
  static void _breedSetter(Dog parent, UndefinedWrapper<
            String
> value) {
    parent.breed = value;
  }

  final PropertyReflection<Dog, 
            String
> classNamePart;
  static 
            String
 _classNameGetter(Dog parent) {
    return parent.className;
  }
  static void _classNameSetter(Dog parent, 
            String
 value) {
    parent.className = value;
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
  List<PropertyReflection<Dog, dynamic>> get properties => [
    colorPart,
breedPart,
classNamePart,
  ];

  @override
  final AdditionalPropertiesPart<Dog, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Dog instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Dog instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  final AllOfReflection<Dog, AnimalMixin> allOfAnimalPart;

  

  @override
  List<AllOfReflection<Dog, Object>> get allOfs => [
    allOfAnimalPart,
  ];

  @override
  List<OneOfReflection<Dog, Object>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<Dog, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Dog empty() {
    return Dog(
      className: classNamePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is DogReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


