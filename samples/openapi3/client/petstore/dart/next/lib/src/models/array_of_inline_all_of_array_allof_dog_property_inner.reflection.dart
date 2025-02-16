// Model reflection

part of 'array_of_inline_all_of_array_allof_dog_property_inner.dart';


//class reflection

class ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection extends ModelReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner> {
  static ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection instanceGetter() => instance;
  static const instance = ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection._(
    modelName: r'ArrayOfInlineAllOf_array_allof_dog_property_inner',
    className: r'ArrayOfInlineAllOfArrayAllofDogPropertyInner',
    xml: XmlReflection(
),
    breedPart: PropertyReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, UndefinedWrapper<
            String
>>(
      dartName: r'breed',
      nullable: false,
      required: false,
      oasName: r'breed',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_breedGetter),
      setter: FunctionWrapper2(_breedSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    colorPart: PropertyReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, UndefinedWrapper<
            String
>>(
      dartName: r'color',
      nullable: false,
      required: false,
      oasName: r'color',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_colorGetter),
      setter: FunctionWrapper2(_colorSetter),
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
  const ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.breedPart,
    required this.colorPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, UndefinedWrapper<
            String
>> breedPart;
  static UndefinedWrapper<
            String
> _breedGetter(ArrayOfInlineAllOfArrayAllofDogPropertyInner parent) {
    return parent.breed;
  }
  static void _breedSetter(ArrayOfInlineAllOfArrayAllofDogPropertyInner parent, UndefinedWrapper<
            String
> value) {
    parent.breed = value;
  }

  final PropertyReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, UndefinedWrapper<
            String
>> colorPart;
  static UndefinedWrapper<
            String
> _colorGetter(ArrayOfInlineAllOfArrayAllofDogPropertyInner parent) {
    return parent.color;
  }
  static void _colorSetter(ArrayOfInlineAllOfArrayAllofDogPropertyInner parent, UndefinedWrapper<
            String
> value) {
    parent.color = value;
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
  List<PropertyReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, dynamic>> get properties => [
    breedPart,
colorPart,
  ];

  @override
  final AdditionalPropertiesPart<ArrayOfInlineAllOfArrayAllofDogPropertyInner, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ArrayOfInlineAllOfArrayAllofDogPropertyInner instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ArrayOfInlineAllOfArrayAllofDogPropertyInner instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, Object>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, Object>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<ArrayOfInlineAllOfArrayAllofDogPropertyInner, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ArrayOfInlineAllOfArrayAllofDogPropertyInner empty() {
    return ArrayOfInlineAllOfArrayAllofDogPropertyInner(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


