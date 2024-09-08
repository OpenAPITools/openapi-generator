// Model reflection

part of 'cat.dart';


//class reflection

class CatReflection extends ModelReflection<Cat> {
  static CatReflection instanceGetter() => instance;
  static const instance = CatReflection._(
    modelName: r'Cat',
    className: r'Cat',
    xml: XmlReflection(
),
    colorPart: PropertyReflection<Cat, UndefinedWrapper<
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
    declawedPart: PropertyReflection<Cat, UndefinedWrapper<
            bool
>>(
      dartName: r'declawed',
      nullable: false,
      required: false,
      oasName: r'declawed',
      oasType: r'boolean',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_declawedGetter),
      setter: FunctionWrapper2(_declawedSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forbool
        
,
)
),
    ),
    classNamePart: PropertyReflection<Cat, 
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
  const CatReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.colorPart,
    required this.declawedPart,
    required this.classNamePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
    required this.allOfAnimalPart,
    
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Cat, UndefinedWrapper<
            String
>> colorPart;
  static UndefinedWrapper<
            String
> _colorGetter(Cat parent) {
    return parent.color;
  }
  static void _colorSetter(Cat parent, UndefinedWrapper<
            String
> value) {
    parent.color = value;
  }

  final PropertyReflection<Cat, UndefinedWrapper<
            bool
>> declawedPart;
  static UndefinedWrapper<
            bool
> _declawedGetter(Cat parent) {
    return parent.declawed;
  }
  static void _declawedSetter(Cat parent, UndefinedWrapper<
            bool
> value) {
    parent.declawed = value;
  }

  final PropertyReflection<Cat, 
            String
> classNamePart;
  static 
            String
 _classNameGetter(Cat parent) {
    return parent.className;
  }
  static void _classNameSetter(Cat parent, 
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
  List<PropertyReflection<Cat, dynamic>> get properties => [
    colorPart,
declawedPart,
classNamePart,
  ];

  @override
  final AdditionalPropertiesPart<Cat, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Cat instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Cat instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  final AllOfReflection<Cat, AnimalMixin> allOfAnimalPart;

  

  @override
  List<AllOfReflection<Cat, Object>> get allOfs => [
    allOfAnimalPart,
  ];

  @override
  List<OneOfReflection<Cat, Object>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<Cat, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Cat empty() {
    return Cat(
      className: classNamePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is CatReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


