// Model reflection

part of 'pet_using_all_of.dart';


//class reflection

class PetUsingAllOfReflection extends ModelReflection<PetUsingAllOf> {
  static PetUsingAllOfReflection instanceGetter() => instance;
  static const instance = PetUsingAllOfReflection._(
    modelName: r'PetUsingAllOf',
    className: r'PetUsingAllOf',
    xml: XmlReflection(
),
    idPart: PropertyReflection<PetUsingAllOf, UndefinedWrapper<
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
    categoryPart: PropertyReflection<PetUsingAllOf, UndefinedWrapper<
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
    namePart: PropertyReflection<PetUsingAllOf, 
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
    photoUrlsPart: PropertyReflection<PetUsingAllOf, 
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
    tagsPart: PropertyReflection<PetUsingAllOf, UndefinedWrapper<
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
    statusPart: PropertyReflection<PetUsingAllOf, UndefinedWrapper<
            PetUsingAllOfStatusEnum
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
    
            PetUsingAllOfStatusEnum.$reflection
        
        
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
  const PetUsingAllOfReflection._({
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

  final PropertyReflection<PetUsingAllOf, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(PetUsingAllOf parent) {
    return parent.id;
  }
  static void _idSetter(PetUsingAllOf parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }

  final PropertyReflection<PetUsingAllOf, UndefinedWrapper<
            Category
>> categoryPart;
  static UndefinedWrapper<
            Category
> _categoryGetter(PetUsingAllOf parent) {
    return parent.category;
  }
  static void _categorySetter(PetUsingAllOf parent, UndefinedWrapper<
            Category
> value) {
    parent.category = value;
  }

  final PropertyReflection<PetUsingAllOf, 
            String
> namePart;
  static 
            String
 _nameGetter(PetUsingAllOf parent) {
    return parent.name;
  }
  static void _nameSetter(PetUsingAllOf parent, 
            String
 value) {
    parent.name = value;
  }

  final PropertyReflection<PetUsingAllOf, 
    List<
        
            String
>
> photoUrlsPart;
  static 
    List<
        
            String
>
 _photoUrlsGetter(PetUsingAllOf parent) {
    return parent.photoUrls;
  }
  static void _photoUrlsSetter(PetUsingAllOf parent, 
    List<
        
            String
>
 value) {
    parent.photoUrls = value;
  }

  final PropertyReflection<PetUsingAllOf, UndefinedWrapper<
    List<
        
            Tag
>
>> tagsPart;
  static UndefinedWrapper<
    List<
        
            Tag
>
> _tagsGetter(PetUsingAllOf parent) {
    return parent.tags;
  }
  static void _tagsSetter(PetUsingAllOf parent, UndefinedWrapper<
    List<
        
            Tag
>
> value) {
    parent.tags = value;
  }

  final PropertyReflection<PetUsingAllOf, UndefinedWrapper<
            PetUsingAllOfStatusEnum
>> statusPart;
  static UndefinedWrapper<
            PetUsingAllOfStatusEnum
> _statusGetter(PetUsingAllOf parent) {
    return parent.status;
  }
  static void _statusSetter(PetUsingAllOf parent, UndefinedWrapper<
            PetUsingAllOfStatusEnum
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
  List<PropertyReflection<PetUsingAllOf, dynamic>> get properties => [
    idPart,
categoryPart,
namePart,
photoUrlsPart,
tagsPart,
statusPart,
  ];

  @override
  final AdditionalPropertiesPart<PetUsingAllOf, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(PetUsingAllOf instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(PetUsingAllOf instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<PetUsingAllOf, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  PetUsingAllOf empty() {
    return PetUsingAllOf(
      name: namePart.reflection.emptyFunction(),
      photoUrls: photoUrlsPart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is PetUsingAllOfReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


