// Model reflection

part of 'tag.dart';


//class reflection

class TagReflection extends ModelReflection<Tag> {
  static TagReflection instanceGetter() => instance;
  static const instance = TagReflection._(
    modelName: r'Tag',
    className: r'Tag',
    xml: XmlReflection(
    xmlName: r'Tag',
),
    idPart: PropertyReflection<Tag, UndefinedWrapper<
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
    namePart: PropertyReflection<Tag, UndefinedWrapper<
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
  const TagReflection._({
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

  final PropertyReflection<Tag, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(Tag parent) {
    return parent.id;
  }
  static void _idSetter(Tag parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }

  final PropertyReflection<Tag, UndefinedWrapper<
            String
>> namePart;
  static UndefinedWrapper<
            String
> _nameGetter(Tag parent) {
    return parent.name;
  }
  static void _nameSetter(Tag parent, UndefinedWrapper<
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
  List<PropertyReflection<Tag, dynamic>> get properties => [
    idPart,
namePart,
  ];

  @override
  final AdditionalPropertiesPart<Tag, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Tag instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Tag instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<Tag, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Tag empty() {
    return Tag(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is TagReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


