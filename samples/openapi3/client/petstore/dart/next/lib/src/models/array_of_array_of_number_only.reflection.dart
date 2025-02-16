// Model reflection

part of 'array_of_array_of_number_only.dart';


//class reflection

class ArrayOfArrayOfNumberOnlyReflection extends ModelReflection<ArrayOfArrayOfNumberOnly> {
  static ArrayOfArrayOfNumberOnlyReflection instanceGetter() => instance;
  static const instance = ArrayOfArrayOfNumberOnlyReflection._(
    modelName: r'ArrayOfArrayOfNumberOnly',
    className: r'ArrayOfArrayOfNumberOnly',
    xml: XmlReflection(
),
    arrayArrayNumberPart: PropertyReflection<ArrayOfArrayOfNumberOnly, UndefinedWrapper<
    List<
        
    List<
        
            num
>
>
>>(
      dartName: r'arrayArrayNumber',
      nullable: false,
      required: false,
      oasName: r'ArrayArrayNumber',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_arrayArrayNumberGetter),
      setter: FunctionWrapper2(_arrayArrayNumberSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.fornum
        
,
)
)
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
  const ArrayOfArrayOfNumberOnlyReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.arrayArrayNumberPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ArrayOfArrayOfNumberOnly, UndefinedWrapper<
    List<
        
    List<
        
            num
>
>
>> arrayArrayNumberPart;
  static UndefinedWrapper<
    List<
        
    List<
        
            num
>
>
> _arrayArrayNumberGetter(ArrayOfArrayOfNumberOnly parent) {
    return parent.arrayArrayNumber;
  }
  static void _arrayArrayNumberSetter(ArrayOfArrayOfNumberOnly parent, UndefinedWrapper<
    List<
        
    List<
        
            num
>
>
> value) {
    parent.arrayArrayNumber = value;
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
  List<PropertyReflection<ArrayOfArrayOfNumberOnly, dynamic>> get properties => [
    arrayArrayNumberPart,
  ];

  @override
  final AdditionalPropertiesPart<ArrayOfArrayOfNumberOnly, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ArrayOfArrayOfNumberOnly instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ArrayOfArrayOfNumberOnly instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<ArrayOfArrayOfNumberOnly, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ArrayOfArrayOfNumberOnly empty() {
    return ArrayOfArrayOfNumberOnly(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ArrayOfArrayOfNumberOnlyReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


