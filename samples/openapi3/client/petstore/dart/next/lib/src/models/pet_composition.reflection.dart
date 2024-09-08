// Model reflection

part of 'pet_composition.dart';


//class reflection

class PetCompositionReflection extends ModelReflection<PetComposition> {
  static PetCompositionReflection instanceGetter() => instance;
  static const instance = PetCompositionReflection._(
    modelName: r'PetComposition',
    className: r'PetComposition',
    xml: XmlReflection(
),
    photoUrlsPart: PropertyReflection<PetComposition, 
    List<
        
            String
>
>(
      dartName: r'photoUrls',
      nullable: false,
      required: true,
      oasName: r'photoUrls',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    xmlName: r'photoUrl',
    wrapped: true,
),
      getter: FunctionWrapper1(_photoUrlsGetter),
      setter: FunctionWrapper2(_photoUrlsSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'photoUrl',
    wrapped: true,
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
)
,
)
,
    ),
    namePart: PropertyReflection<PetComposition, 
            String
>(
      dartName: r'name',
      nullable: false,
      required: true,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_nameGetter),
      setter: FunctionWrapper2(_nameSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
,
    ),
    idPart: PropertyReflection<PetComposition, UndefinedWrapper<
            int
>>(
      dartName: r'id',
      nullable: false,
      required: false,
      oasName: r'id',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_idGetter),
      setter: FunctionWrapper2(_idSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
),
    ),
    categoryPart: PropertyReflection<PetComposition, UndefinedWrapper<
            Category
>>(
      dartName: r'category',
      nullable: false,
      required: false,
      oasName: r'category',
      oasType: r'Category',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    xmlName: r'Category',
),
      getter: FunctionWrapper1(_categoryGetter),
      setter: FunctionWrapper2(_categorySetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'Category',
),
    
            
        
        
            
                Category.$reflection
        
,
)
),
    ),
    tagsPart: PropertyReflection<PetComposition, UndefinedWrapper<
    List<
        
            Tag
>
>>(
      dartName: r'tags',
      nullable: false,
      required: false,
      oasName: r'tags',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    xmlName: r'tag',
    wrapped: true,
),
      getter: FunctionWrapper1(_tagsGetter),
      setter: FunctionWrapper2(_tagsSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'tag',
    wrapped: true,
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'Tag',
),
    
            
        
        
            
                Tag.$reflection
        
,
)
)
,
)
),
    ),
    statusPart: PropertyReflection<PetComposition, UndefinedWrapper<
            PetStatusEnum
>>(
      dartName: r'status',
      nullable: false,
      required: false,
      oasName: r'status',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_statusGetter),
      setter: FunctionWrapper2(_statusSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            PetStatusEnum.$reflection
        
        
,
)
),
    ),
    allOfPetPart: AllOfReflection(
      parentReflectionGetter: instanceGetter,
      reflection: PetReflection.instance,
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
  const PetCompositionReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.photoUrlsPart,
    required this.namePart,
    required this.idPart,
    required this.categoryPart,
    required this.tagsPart,
    required this.statusPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
    required this.allOfPetPart,
    
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<PetComposition, 
    List<
        
            String
>
> photoUrlsPart;
  static 
    List<
        
            String
>
 _photoUrlsGetter(PetComposition parent) {
    return parent.photoUrls;
  }
  static void _photoUrlsSetter(PetComposition parent, 
    List<
        
            String
>
 value) {
    parent.photoUrls = value;
  }

  final PropertyReflection<PetComposition, 
            String
> namePart;
  static 
            String
 _nameGetter(PetComposition parent) {
    return parent.name;
  }
  static void _nameSetter(PetComposition parent, 
            String
 value) {
    parent.name = value;
  }

  final PropertyReflection<PetComposition, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(PetComposition parent) {
    return parent.id;
  }
  static void _idSetter(PetComposition parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }

  final PropertyReflection<PetComposition, UndefinedWrapper<
            Category
>> categoryPart;
  static UndefinedWrapper<
            Category
> _categoryGetter(PetComposition parent) {
    return parent.category;
  }
  static void _categorySetter(PetComposition parent, UndefinedWrapper<
            Category
> value) {
    parent.category = value;
  }

  final PropertyReflection<PetComposition, UndefinedWrapper<
    List<
        
            Tag
>
>> tagsPart;
  static UndefinedWrapper<
    List<
        
            Tag
>
> _tagsGetter(PetComposition parent) {
    return parent.tags;
  }
  static void _tagsSetter(PetComposition parent, UndefinedWrapper<
    List<
        
            Tag
>
> value) {
    parent.tags = value;
  }

  final PropertyReflection<PetComposition, UndefinedWrapper<
            PetStatusEnum
>> statusPart;
  static UndefinedWrapper<
            PetStatusEnum
> _statusGetter(PetComposition parent) {
    return parent.status;
  }
  static void _statusSetter(PetComposition parent, UndefinedWrapper<
            PetStatusEnum
> value) {
    parent.status = value;
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
  List<PropertyReflection<PetComposition, dynamic>> get properties => [
    photoUrlsPart,
namePart,
idPart,
categoryPart,
tagsPart,
statusPart,
  ];

  @override
  final AdditionalPropertiesPart<PetComposition, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(PetComposition instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(PetComposition instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  final AllOfReflection<PetComposition, PetMixin> allOfPetPart;

  

  @override
  List<AllOfReflection<PetComposition, Object>> get allOfs => [
    allOfPetPart,
  ];

  @override
  List<OneOfReflection<PetComposition, Object>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<PetComposition, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  PetComposition empty() {
    return PetComposition(
      photoUrls: photoUrlsPart.reflection.emptyFunction(),
      name: namePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is PetCompositionReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


