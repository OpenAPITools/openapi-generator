// Model reflection

part of 'animal.dart';


//class reflection

class AnimalReflection extends ModelReflection<Animal> {
  static AnimalReflection instanceGetter() => instance;
  static const instance = AnimalReflection._(
    modelName: r'Animal',
    className: r'Animal',
    xml: XmlReflection(
),
    classNamePart: PropertyReflection<Animal, 
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
    colorPart: PropertyReflection<Animal, UndefinedWrapper<
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
    discriminatorKey: r'className',
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
  const AnimalReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.classNamePart,
    required this.colorPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Animal, 
            String
> classNamePart;
  static 
            String
 _classNameGetter(Animal parent) {
    return parent.className;
  }
  static void _classNameSetter(Animal parent, 
            String
 value) {
    parent.className = value;
  }

  final PropertyReflection<Animal, UndefinedWrapper<
            String
>> colorPart;
  static UndefinedWrapper<
            String
> _colorGetter(Animal parent) {
    return parent.color;
  }
  static void _colorSetter(Animal parent, UndefinedWrapper<
            String
> value) {
    parent.color = value;
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
  List<PropertyReflection<Animal, dynamic>> get properties => [
    classNamePart,
colorPart,
  ];

  @override
  final AdditionalPropertiesPart<Animal, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Animal instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Animal instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<Animal, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Animal empty() {
    return Animal(
      className: classNamePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is AnimalReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


