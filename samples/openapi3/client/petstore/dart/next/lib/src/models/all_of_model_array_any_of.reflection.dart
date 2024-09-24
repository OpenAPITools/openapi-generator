// Model reflection

part of 'all_of_model_array_any_of.dart';


//class reflection

class AllOfModelArrayAnyOfReflection extends ModelReflection<AllOfModelArrayAnyOf> {
  static AllOfModelArrayAnyOfReflection instanceGetter() => instance;
  static const instance = AllOfModelArrayAnyOfReflection._(
    modelName: r'AllOfModelArrayAnyOf',
    className: r'AllOfModelArrayAnyOf',
    xml: XmlReflection(
),
    namePart: PropertyReflection<AllOfModelArrayAnyOf, 
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
    attributesPart: PropertyReflection<AllOfModelArrayAnyOf, UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributes
>>(
      dartName: r'attributes',
      nullable: false,
      required: false,
      oasName: r'attributes',
      oasType: r'AllOfModelArrayAnyOfAllOfAttributes',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_attributesGetter),
      setter: FunctionWrapper2(_attributesSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                AllOfModelArrayAnyOfAllOfAttributes.$reflection
        
,
)
),
    ),
    idPart: PropertyReflection<AllOfModelArrayAnyOf, UndefinedWrapper<
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
    linkListColumn1Part: PropertyReflection<AllOfModelArrayAnyOf, UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfLinkListColumn1
>>(
      dartName: r'linkListColumn1',
      nullable: false,
      required: false,
      oasName: r'linkListColumn1',
      oasType: r'AllOfModelArrayAnyOfAllOfLinkListColumn1',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_linkListColumn1Getter),
      setter: FunctionWrapper2(_linkListColumn1Setter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                AllOfModelArrayAnyOfAllOfLinkListColumn1.$reflection
        
,
)
),
    ),
    allOfCategoryPart: AllOfReflection(
      parentReflectionGetter: instanceGetter,
      reflection: CategoryReflection.instance,
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
  const AllOfModelArrayAnyOfReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.namePart,
    required this.attributesPart,
    required this.idPart,
    required this.linkListColumn1Part,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
    required this.allOfCategoryPart,
    
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<AllOfModelArrayAnyOf, 
            String
> namePart;
  static 
            String
 _nameGetter(AllOfModelArrayAnyOf parent) {
    return parent.name;
  }
  static void _nameSetter(AllOfModelArrayAnyOf parent, 
            String
 value) {
    parent.name = value;
  }

  final PropertyReflection<AllOfModelArrayAnyOf, UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributes
>> attributesPart;
  static UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributes
> _attributesGetter(AllOfModelArrayAnyOf parent) {
    return parent.attributes;
  }
  static void _attributesSetter(AllOfModelArrayAnyOf parent, UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributes
> value) {
    parent.attributes = value;
  }

  final PropertyReflection<AllOfModelArrayAnyOf, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(AllOfModelArrayAnyOf parent) {
    return parent.id;
  }
  static void _idSetter(AllOfModelArrayAnyOf parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }

  final PropertyReflection<AllOfModelArrayAnyOf, UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfLinkListColumn1
>> linkListColumn1Part;
  static UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfLinkListColumn1
> _linkListColumn1Getter(AllOfModelArrayAnyOf parent) {
    return parent.linkListColumn1;
  }
  static void _linkListColumn1Setter(AllOfModelArrayAnyOf parent, UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfLinkListColumn1
> value) {
    parent.linkListColumn1 = value;
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
  List<PropertyReflection<AllOfModelArrayAnyOf, dynamic>> get properties => [
    namePart,
attributesPart,
idPart,
linkListColumn1Part,
  ];

  @override
  final AdditionalPropertiesPart<AllOfModelArrayAnyOf, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(AllOfModelArrayAnyOf instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(AllOfModelArrayAnyOf instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  final AllOfReflection<AllOfModelArrayAnyOf, CategoryMixin> allOfCategoryPart;

  

  @override
  List<AllOfReflection<AllOfModelArrayAnyOf, Object>> get allOfs => [
    allOfCategoryPart,
  ];

  @override
  List<OneOfReflection<AllOfModelArrayAnyOf, Object>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<AllOfModelArrayAnyOf, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  AllOfModelArrayAnyOf empty() {
    return AllOfModelArrayAnyOf(
      name: namePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is AllOfModelArrayAnyOfReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


