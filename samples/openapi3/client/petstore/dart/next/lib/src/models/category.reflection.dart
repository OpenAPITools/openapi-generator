// Model reflection

part of 'category.dart';


//class reflection

class CategoryReflection extends ModelReflection<Category> {
  static CategoryReflection instanceGetter() => instance;
  static const instance = CategoryReflection._(
    modelName: r'Category',
    className: r'Category',
    xml: XmlReflection(
    xmlName: r'Category',
),
    idPart: PropertyReflection<Category, UndefinedWrapper<
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
    namePart: PropertyReflection<Category, 
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
  const CategoryReflection._({
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

  final PropertyReflection<Category, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(Category parent) {
    return parent.id;
  }
  static void _idSetter(Category parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }

  final PropertyReflection<Category, 
            String
> namePart;
  static 
            String
 _nameGetter(Category parent) {
    return parent.name;
  }
  static void _nameSetter(Category parent, 
            String
 value) {
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
  List<PropertyReflection<Category, dynamic>> get properties => [
    idPart,
namePart,
  ];

  @override
  final AdditionalPropertiesPart<Category, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Category instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Category instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<Category, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Category empty() {
    return Category(
      name: namePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is CategoryReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


