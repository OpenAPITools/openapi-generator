// Model reflection

part of 'pet.dart';


//class reflection

class PetReflection extends ModelReflection<Pet> {
  static PetReflection instanceGetter() => instance;
  static const instance = PetReflection._(
    modelName: r'Pet',
    className: r'Pet',
    xml: XmlReflection(
    xmlName: r'Pet',
),
    idPart: PropertyReflection<Pet, UndefinedWrapper<
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
    categoryPart: PropertyReflection<Pet, UndefinedWrapper<
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
    namePart: PropertyReflection<Pet, 
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
    photoUrlsPart: PropertyReflection<Pet, 
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
    tagsPart: PropertyReflection<Pet, UndefinedWrapper<
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
    statusPart: PropertyReflection<Pet, UndefinedWrapper<
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
  const PetReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.idPart,
    required this.categoryPart,
    required this.namePart,
    required this.photoUrlsPart,
    required this.tagsPart,
    required this.statusPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Pet, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(Pet parent) {
    return parent.id;
  }
  static void _idSetter(Pet parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }

  final PropertyReflection<Pet, UndefinedWrapper<
            Category
>> categoryPart;
  static UndefinedWrapper<
            Category
> _categoryGetter(Pet parent) {
    return parent.category;
  }
  static void _categorySetter(Pet parent, UndefinedWrapper<
            Category
> value) {
    parent.category = value;
  }

  final PropertyReflection<Pet, 
            String
> namePart;
  static 
            String
 _nameGetter(Pet parent) {
    return parent.name;
  }
  static void _nameSetter(Pet parent, 
            String
 value) {
    parent.name = value;
  }

  final PropertyReflection<Pet, 
    List<
        
            String
>
> photoUrlsPart;
  static 
    List<
        
            String
>
 _photoUrlsGetter(Pet parent) {
    return parent.photoUrls;
  }
  static void _photoUrlsSetter(Pet parent, 
    List<
        
            String
>
 value) {
    parent.photoUrls = value;
  }

  final PropertyReflection<Pet, UndefinedWrapper<
    List<
        
            Tag
>
>> tagsPart;
  static UndefinedWrapper<
    List<
        
            Tag
>
> _tagsGetter(Pet parent) {
    return parent.tags;
  }
  static void _tagsSetter(Pet parent, UndefinedWrapper<
    List<
        
            Tag
>
> value) {
    parent.tags = value;
  }

  final PropertyReflection<Pet, UndefinedWrapper<
            PetStatusEnum
>> statusPart;
  static UndefinedWrapper<
            PetStatusEnum
> _statusGetter(Pet parent) {
    return parent.status;
  }
  static void _statusSetter(Pet parent, UndefinedWrapper<
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
  List<PropertyReflection<Pet, dynamic>> get properties => [
    idPart,
categoryPart,
namePart,
photoUrlsPart,
tagsPart,
statusPart,
  ];

  @override
  final AdditionalPropertiesPart<Pet, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Pet instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Pet instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<Pet, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Pet empty() {
    return Pet(
      name: namePart.reflection.emptyFunction(),
      photoUrls: photoUrlsPart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is PetReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


