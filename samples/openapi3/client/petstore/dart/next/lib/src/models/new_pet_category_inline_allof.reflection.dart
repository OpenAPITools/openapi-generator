// Model reflection

part of 'new_pet_category_inline_allof.dart';


//class reflection

class NewPetCategoryInlineAllofReflection extends ModelReflection<NewPetCategoryInlineAllof> {
  static NewPetCategoryInlineAllofReflection instanceGetter() => instance;
  static const instance = NewPetCategoryInlineAllofReflection._(
    modelName: r'NewPet_category_inline_allof',
    className: r'NewPetCategoryInlineAllof',
    xml: XmlReflection(
),
    idPart: PropertyReflection<NewPetCategoryInlineAllof, UndefinedWrapper<
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
    namePart: PropertyReflection<NewPetCategoryInlineAllof, 
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
    categoryTagPart: PropertyReflection<NewPetCategoryInlineAllof, UndefinedWrapper<
            NewPetCategoryInlineAllofAllOfCategoryTag
>>(
      dartName: r'categoryTag',
      nullable: false,
      required: false,
      oasName: r'category_tag',
      oasType: r'NewPetCategoryInlineAllofAllOfCategoryTag',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_categoryTagGetter),
      setter: FunctionWrapper2(_categoryTagSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                NewPetCategoryInlineAllofAllOfCategoryTag.$reflection
        
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
  const NewPetCategoryInlineAllofReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.idPart,
    required this.namePart,
    required this.categoryTagPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<NewPetCategoryInlineAllof, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(NewPetCategoryInlineAllof parent) {
    return parent.id;
  }
  static void _idSetter(NewPetCategoryInlineAllof parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }

  final PropertyReflection<NewPetCategoryInlineAllof, 
            String
> namePart;
  static 
            String
 _nameGetter(NewPetCategoryInlineAllof parent) {
    return parent.name;
  }
  static void _nameSetter(NewPetCategoryInlineAllof parent, 
            String
 value) {
    parent.name = value;
  }

  final PropertyReflection<NewPetCategoryInlineAllof, UndefinedWrapper<
            NewPetCategoryInlineAllofAllOfCategoryTag
>> categoryTagPart;
  static UndefinedWrapper<
            NewPetCategoryInlineAllofAllOfCategoryTag
> _categoryTagGetter(NewPetCategoryInlineAllof parent) {
    return parent.categoryTag;
  }
  static void _categoryTagSetter(NewPetCategoryInlineAllof parent, UndefinedWrapper<
            NewPetCategoryInlineAllofAllOfCategoryTag
> value) {
    parent.categoryTag = value;
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
  List<PropertyReflection<NewPetCategoryInlineAllof, dynamic>> get properties => [
    idPart,
namePart,
categoryTagPart,
  ];

  @override
  final AdditionalPropertiesPart<NewPetCategoryInlineAllof, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(NewPetCategoryInlineAllof instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(NewPetCategoryInlineAllof instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<NewPetCategoryInlineAllof, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<NewPetCategoryInlineAllof, Object>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<NewPetCategoryInlineAllof, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  NewPetCategoryInlineAllof empty() {
    return NewPetCategoryInlineAllof(
      name: namePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is NewPetCategoryInlineAllofReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


