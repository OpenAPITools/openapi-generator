// Model reflection

part of 'array_of_inline_all_of.dart';


//class reflection

class ArrayOfInlineAllOfReflection extends ModelReflection<ArrayOfInlineAllOf> {
  static ArrayOfInlineAllOfReflection instanceGetter() => instance;
  static const instance = ArrayOfInlineAllOfReflection._(
    modelName: r'ArrayOfInlineAllOf',
    className: r'ArrayOfInlineAllOf',
    xml: XmlReflection(
),
    idPart: PropertyReflection<ArrayOfInlineAllOf, UndefinedWrapper<
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
    namePart: PropertyReflection<ArrayOfInlineAllOf, 
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
    arrayAllofDogPropertyPart: PropertyReflection<ArrayOfInlineAllOf, UndefinedWrapper<
    List<
        
            ArrayOfInlineAllOfArrayAllofDogPropertyInner
>
>>(
      dartName: r'arrayAllofDogProperty',
      nullable: false,
      required: false,
      oasName: r'array_allof_dog_property',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_arrayAllofDogPropertyGetter),
      setter: FunctionWrapper2(_arrayAllofDogPropertySetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                ArrayOfInlineAllOfArrayAllofDogPropertyInner.$reflection
        
,
)
)
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
  const ArrayOfInlineAllOfReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.idPart,
    required this.namePart,
    required this.arrayAllofDogPropertyPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ArrayOfInlineAllOf, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(ArrayOfInlineAllOf parent) {
    return parent.id;
  }
  static void _idSetter(ArrayOfInlineAllOf parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }

  final PropertyReflection<ArrayOfInlineAllOf, 
            String
> namePart;
  static 
            String
 _nameGetter(ArrayOfInlineAllOf parent) {
    return parent.name;
  }
  static void _nameSetter(ArrayOfInlineAllOf parent, 
            String
 value) {
    parent.name = value;
  }

  final PropertyReflection<ArrayOfInlineAllOf, UndefinedWrapper<
    List<
        
            ArrayOfInlineAllOfArrayAllofDogPropertyInner
>
>> arrayAllofDogPropertyPart;
  static UndefinedWrapper<
    List<
        
            ArrayOfInlineAllOfArrayAllofDogPropertyInner
>
> _arrayAllofDogPropertyGetter(ArrayOfInlineAllOf parent) {
    return parent.arrayAllofDogProperty;
  }
  static void _arrayAllofDogPropertySetter(ArrayOfInlineAllOf parent, UndefinedWrapper<
    List<
        
            ArrayOfInlineAllOfArrayAllofDogPropertyInner
>
> value) {
    parent.arrayAllofDogProperty = value;
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
  List<PropertyReflection<ArrayOfInlineAllOf, dynamic>> get properties => [
    idPart,
namePart,
arrayAllofDogPropertyPart,
  ];

  @override
  final AdditionalPropertiesPart<ArrayOfInlineAllOf, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ArrayOfInlineAllOf instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ArrayOfInlineAllOf instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<ArrayOfInlineAllOf, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ArrayOfInlineAllOf empty() {
    return ArrayOfInlineAllOf(
      name: namePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ArrayOfInlineAllOfReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


