// Model reflection

part of 'array_of_number_only.dart';


//class reflection

class ArrayOfNumberOnlyReflection extends ModelReflection<ArrayOfNumberOnly> {
  static ArrayOfNumberOnlyReflection instanceGetter() => instance;
  static const instance = ArrayOfNumberOnlyReflection._(
    modelName: r'ArrayOfNumberOnly',
    className: r'ArrayOfNumberOnly',
    xml: const XmlReflection(
),
    arrayNumberPart: PropertyReflection<ArrayOfNumberOnly, UndefinedWrapper<
    List<
        
            num
>
>>(
      dartName: r'arrayNumber',
      nullable: false,
      required: false,
      oasName: r'ArrayNumber',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: const XmlReflection(
),
      getter: FunctionWrapper1(_arrayNumberGetter),
      setter: FunctionWrapper2(_arrayNumberSetter),
      reflection: UndefinedWrapperReflection(
    ListReflection(
            
        
        
            
                PrimitiveReflection.fornum
        
)
),
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesPart(
      parentReflectionGetter: instanceGetter,
      itemReflection: NullableReflection(ObjectReflection()
),
      getter: FunctionWrapper1(_AdditionalPropertiesGetter),
      setter: FunctionWrapper2(_AdditionalPropertiesSetter),
    ),
  );
  const ArrayOfNumberOnlyReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.arrayNumberPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ArrayOfNumberOnly, UndefinedWrapper<
    List<
        
            num
>
>> arrayNumberPart;
  static UndefinedWrapper<
    List<
        
            num
>
> _arrayNumberGetter(ArrayOfNumberOnly parent) {
    return parent.arrayNumber;
  }
  static void _arrayNumberSetter(ArrayOfNumberOnly parent, UndefinedWrapper<
    List<
        
            num
>
> value) {
    parent.arrayNumber = value;
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
  List<PropertyReflection<ArrayOfNumberOnly, dynamic>> get properties => [
    arrayNumberPart,
  ];

  @override
  final AdditionalPropertiesPart<ArrayOfNumberOnly, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ArrayOfNumberOnly instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ArrayOfNumberOnly instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<ArrayOfNumberOnly, dynamic>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ArrayOfNumberOnly empty() {
    return ArrayOfNumberOnly(
    );
  }
}


