// Model reflection

part of 'new_pet_category_inline_allof_all_of_category_tag.dart';


//class reflection

class NewPetCategoryInlineAllofAllOfCategoryTagReflection extends ModelReflection<NewPetCategoryInlineAllofAllOfCategoryTag> {
  static NewPetCategoryInlineAllofAllOfCategoryTagReflection instanceGetter() => instance;
  static const instance = NewPetCategoryInlineAllofAllOfCategoryTagReflection._(
    modelName: r'NewPet_category_inline_allof_allOf_category_tag',
    className: r'NewPetCategoryInlineAllofAllOfCategoryTag',
    xml: XmlReflection(
),
    idPart: PropertyReflection<NewPetCategoryInlineAllofAllOfCategoryTag, UndefinedWrapper<
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
    namePart: PropertyReflection<NewPetCategoryInlineAllofAllOfCategoryTag, UndefinedWrapper<
            String
>>(
      dartName: r'name',
      nullable: false,
      required: false,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_nameGetter),
      setter: FunctionWrapper2(_nameSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
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
  const NewPetCategoryInlineAllofAllOfCategoryTagReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.idPart,
    required this.namePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<NewPetCategoryInlineAllofAllOfCategoryTag, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(NewPetCategoryInlineAllofAllOfCategoryTag parent) {
    return parent.id;
  }
  static void _idSetter(NewPetCategoryInlineAllofAllOfCategoryTag parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }

  final PropertyReflection<NewPetCategoryInlineAllofAllOfCategoryTag, UndefinedWrapper<
            String
>> namePart;
  static UndefinedWrapper<
            String
> _nameGetter(NewPetCategoryInlineAllofAllOfCategoryTag parent) {
    return parent.name;
  }
  static void _nameSetter(NewPetCategoryInlineAllofAllOfCategoryTag parent, UndefinedWrapper<
            String
> value) {
    parent.name = value;
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
  List<PropertyReflection<NewPetCategoryInlineAllofAllOfCategoryTag, dynamic>> get properties => [
    idPart,
namePart,
  ];

  @override
  final AdditionalPropertiesPart<NewPetCategoryInlineAllofAllOfCategoryTag, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(NewPetCategoryInlineAllofAllOfCategoryTag instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(NewPetCategoryInlineAllofAllOfCategoryTag instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<NewPetCategoryInlineAllofAllOfCategoryTag, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  NewPetCategoryInlineAllofAllOfCategoryTag empty() {
    return NewPetCategoryInlineAllofAllOfCategoryTag(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is NewPetCategoryInlineAllofAllOfCategoryTagReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


