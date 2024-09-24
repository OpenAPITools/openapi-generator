// Model reflection

part of 'pet_with_required_tags.dart';


//class reflection

class PetWithRequiredTagsReflection extends ModelReflection<PetWithRequiredTags> {
  static PetWithRequiredTagsReflection instanceGetter() => instance;
  static const instance = PetWithRequiredTagsReflection._(
    modelName: r'PetWithRequiredTags',
    className: r'PetWithRequiredTags',
    xml: XmlReflection(
    xmlName: r'Pet',
),
    idPart: PropertyReflection<PetWithRequiredTags, UndefinedWrapper<
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
    categoryPart: PropertyReflection<PetWithRequiredTags, UndefinedWrapper<
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
    namePart: PropertyReflection<PetWithRequiredTags, 
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
    photoUrlsPart: PropertyReflection<PetWithRequiredTags, 
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
    tagsPart: PropertyReflection<PetWithRequiredTags, 
    List<
        
            Tag
>
>(
      dartName: r'tags',
      nullable: false,
      required: true,
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
      reflection: XmlReflectionWrapper(
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
,
    ),
    statusPart: PropertyReflection<PetWithRequiredTags, UndefinedWrapper<
            PetWithRequiredTagsStatusEnum
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
    
            PetWithRequiredTagsStatusEnum.$reflection
        
        
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
  const PetWithRequiredTagsReflection._({
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

  final PropertyReflection<PetWithRequiredTags, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(PetWithRequiredTags parent) {
    return parent.id;
  }
  static void _idSetter(PetWithRequiredTags parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }

  final PropertyReflection<PetWithRequiredTags, UndefinedWrapper<
            Category
>> categoryPart;
  static UndefinedWrapper<
            Category
> _categoryGetter(PetWithRequiredTags parent) {
    return parent.category;
  }
  static void _categorySetter(PetWithRequiredTags parent, UndefinedWrapper<
            Category
> value) {
    parent.category = value;
  }

  final PropertyReflection<PetWithRequiredTags, 
            String
> namePart;
  static 
            String
 _nameGetter(PetWithRequiredTags parent) {
    return parent.name;
  }
  static void _nameSetter(PetWithRequiredTags parent, 
            String
 value) {
    parent.name = value;
  }

  final PropertyReflection<PetWithRequiredTags, 
    List<
        
            String
>
> photoUrlsPart;
  static 
    List<
        
            String
>
 _photoUrlsGetter(PetWithRequiredTags parent) {
    return parent.photoUrls;
  }
  static void _photoUrlsSetter(PetWithRequiredTags parent, 
    List<
        
            String
>
 value) {
    parent.photoUrls = value;
  }

  final PropertyReflection<PetWithRequiredTags, 
    List<
        
            Tag
>
> tagsPart;
  static 
    List<
        
            Tag
>
 _tagsGetter(PetWithRequiredTags parent) {
    return parent.tags;
  }
  static void _tagsSetter(PetWithRequiredTags parent, 
    List<
        
            Tag
>
 value) {
    parent.tags = value;
  }

  final PropertyReflection<PetWithRequiredTags, UndefinedWrapper<
            PetWithRequiredTagsStatusEnum
>> statusPart;
  static UndefinedWrapper<
            PetWithRequiredTagsStatusEnum
> _statusGetter(PetWithRequiredTags parent) {
    return parent.status;
  }
  static void _statusSetter(PetWithRequiredTags parent, UndefinedWrapper<
            PetWithRequiredTagsStatusEnum
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
  List<PropertyReflection<PetWithRequiredTags, dynamic>> get properties => [
    idPart,
categoryPart,
namePart,
photoUrlsPart,
tagsPart,
statusPart,
  ];

  @override
  final AdditionalPropertiesPart<PetWithRequiredTags, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(PetWithRequiredTags instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(PetWithRequiredTags instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<PetWithRequiredTags, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  PetWithRequiredTags empty() {
    return PetWithRequiredTags(
      name: namePart.reflection.emptyFunction(),
      photoUrls: photoUrlsPart.reflection.emptyFunction(),
      tags: tagsPart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is PetWithRequiredTagsReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


