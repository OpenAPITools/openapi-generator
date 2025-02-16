// Model reflection

part of 'new_pet.dart';


//class reflection

class NewPetReflection extends ModelReflection<NewPet> {
  static NewPetReflection instanceGetter() => instance;
  static const instance = NewPetReflection._(
    modelName: r'NewPet',
    className: r'NewPet',
    xml: XmlReflection(
),
    idPart: PropertyReflection<NewPet, UndefinedWrapper<
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
    categoryInlineAllofPart: PropertyReflection<NewPet, UndefinedWrapper<
            NewPetCategoryInlineAllof
>>(
      dartName: r'categoryInlineAllof',
      nullable: false,
      required: false,
      oasName: r'category_inline_allof',
      oasType: r'NewPetCategoryInlineAllof',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_categoryInlineAllofGetter),
      setter: FunctionWrapper2(_categoryInlineAllofSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                NewPetCategoryInlineAllof.$reflection
        
,
)
),
    ),
    categoryAllOfRefPart: PropertyReflection<NewPet, UndefinedWrapper<
            Category
>>(
      dartName: r'categoryAllOfRef',
      nullable: false,
      required: false,
      oasName: r'category_allOf_ref',
      oasType: r'Category',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    xmlName: r'Category',
),
      getter: FunctionWrapper1(_categoryAllOfRefGetter),
      setter: FunctionWrapper2(_categoryAllOfRefSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'Category',
),
    
            
        
        
            
                Category.$reflection
        
,
)
),
    ),
    namePart: PropertyReflection<NewPet, 
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
    photoUrlsPart: PropertyReflection<NewPet, 
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
    tagsPart: PropertyReflection<NewPet, UndefinedWrapper<
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
    statusPart: PropertyReflection<NewPet, UndefinedWrapper<
            NewPetStatusEnum
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
    
            NewPetStatusEnum.$reflection
        
        
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
  const NewPetReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.idPart,
    required this.categoryInlineAllofPart,
    required this.categoryAllOfRefPart,
    required this.namePart,
    required this.photoUrlsPart,
    required this.tagsPart,
    required this.statusPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<NewPet, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(NewPet parent) {
    return parent.id;
  }
  static void _idSetter(NewPet parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }

  final PropertyReflection<NewPet, UndefinedWrapper<
            NewPetCategoryInlineAllof
>> categoryInlineAllofPart;
  static UndefinedWrapper<
            NewPetCategoryInlineAllof
> _categoryInlineAllofGetter(NewPet parent) {
    return parent.categoryInlineAllof;
  }
  static void _categoryInlineAllofSetter(NewPet parent, UndefinedWrapper<
            NewPetCategoryInlineAllof
> value) {
    parent.categoryInlineAllof = value;
  }

  final PropertyReflection<NewPet, UndefinedWrapper<
            Category
>> categoryAllOfRefPart;
  static UndefinedWrapper<
            Category
> _categoryAllOfRefGetter(NewPet parent) {
    return parent.categoryAllOfRef;
  }
  static void _categoryAllOfRefSetter(NewPet parent, UndefinedWrapper<
            Category
> value) {
    parent.categoryAllOfRef = value;
  }

  final PropertyReflection<NewPet, 
            String
> namePart;
  static 
            String
 _nameGetter(NewPet parent) {
    return parent.name;
  }
  static void _nameSetter(NewPet parent, 
            String
 value) {
    parent.name = value;
  }

  final PropertyReflection<NewPet, 
    List<
        
            String
>
> photoUrlsPart;
  static 
    List<
        
            String
>
 _photoUrlsGetter(NewPet parent) {
    return parent.photoUrls;
  }
  static void _photoUrlsSetter(NewPet parent, 
    List<
        
            String
>
 value) {
    parent.photoUrls = value;
  }

  final PropertyReflection<NewPet, UndefinedWrapper<
    List<
        
            Tag
>
>> tagsPart;
  static UndefinedWrapper<
    List<
        
            Tag
>
> _tagsGetter(NewPet parent) {
    return parent.tags;
  }
  static void _tagsSetter(NewPet parent, UndefinedWrapper<
    List<
        
            Tag
>
> value) {
    parent.tags = value;
  }

  final PropertyReflection<NewPet, UndefinedWrapper<
            NewPetStatusEnum
>> statusPart;
  static UndefinedWrapper<
            NewPetStatusEnum
> _statusGetter(NewPet parent) {
    return parent.status;
  }
  static void _statusSetter(NewPet parent, UndefinedWrapper<
            NewPetStatusEnum
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
  List<PropertyReflection<NewPet, dynamic>> get properties => [
    idPart,
categoryInlineAllofPart,
categoryAllOfRefPart,
namePart,
photoUrlsPart,
tagsPart,
statusPart,
  ];

  @override
  final AdditionalPropertiesPart<NewPet, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(NewPet instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(NewPet instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<NewPet, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  NewPet empty() {
    return NewPet(
      name: namePart.reflection.emptyFunction(),
      photoUrls: photoUrlsPart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is NewPetReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


