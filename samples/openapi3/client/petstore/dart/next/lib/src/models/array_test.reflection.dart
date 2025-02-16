// Model reflection

part of 'array_test.dart';


//class reflection

class ArrayTestReflection extends ModelReflection<ArrayTest> {
  static ArrayTestReflection instanceGetter() => instance;
  static const instance = ArrayTestReflection._(
    modelName: r'ArrayTest',
    className: r'ArrayTest',
    xml: XmlReflection(
),
    arrayOfStringPart: PropertyReflection<ArrayTest, UndefinedWrapper<
    List<
        
            String
>
>>(
      dartName: r'arrayOfString',
      nullable: false,
      required: false,
      oasName: r'array_of_string',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_arrayOfStringGetter),
      setter: FunctionWrapper2(_arrayOfStringSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
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
),
    ),
    arrayArrayOfIntegerPart: PropertyReflection<ArrayTest, UndefinedWrapper<
    List<
        
    List<
        
            int
>
>
>>(
      dartName: r'arrayArrayOfInteger',
      nullable: false,
      required: false,
      oasName: r'array_array_of_integer',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_arrayArrayOfIntegerGetter),
      setter: FunctionWrapper2(_arrayArrayOfIntegerSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
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
    arrayArrayOfModelPart: PropertyReflection<ArrayTest, UndefinedWrapper<
    List<
        
    List<
        
            ReadOnlyFirst
>
>
>>(
      dartName: r'arrayArrayOfModel',
      nullable: false,
      required: false,
      oasName: r'array_array_of_model',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_arrayArrayOfModelGetter),
      setter: FunctionWrapper2(_arrayArrayOfModelSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                ReadOnlyFirst.$reflection
        
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
  const ArrayTestReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.arrayOfStringPart,
    required this.arrayArrayOfIntegerPart,
    required this.arrayArrayOfModelPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ArrayTest, UndefinedWrapper<
    List<
        
            String
>
>> arrayOfStringPart;
  static UndefinedWrapper<
    List<
        
            String
>
> _arrayOfStringGetter(ArrayTest parent) {
    return parent.arrayOfString;
  }
  static void _arrayOfStringSetter(ArrayTest parent, UndefinedWrapper<
    List<
        
            String
>
> value) {
    parent.arrayOfString = value;
  }

  final PropertyReflection<ArrayTest, UndefinedWrapper<
    List<
        
    List<
        
            int
>
>
>> arrayArrayOfIntegerPart;
  static UndefinedWrapper<
    List<
        
    List<
        
            int
>
>
> _arrayArrayOfIntegerGetter(ArrayTest parent) {
    return parent.arrayArrayOfInteger;
  }
  static void _arrayArrayOfIntegerSetter(ArrayTest parent, UndefinedWrapper<
    List<
        
    List<
        
            int
>
>
> value) {
    parent.arrayArrayOfInteger = value;
  }

  final PropertyReflection<ArrayTest, UndefinedWrapper<
    List<
        
    List<
        
            ReadOnlyFirst
>
>
>> arrayArrayOfModelPart;
  static UndefinedWrapper<
    List<
        
    List<
        
            ReadOnlyFirst
>
>
> _arrayArrayOfModelGetter(ArrayTest parent) {
    return parent.arrayArrayOfModel;
  }
  static void _arrayArrayOfModelSetter(ArrayTest parent, UndefinedWrapper<
    List<
        
    List<
        
            ReadOnlyFirst
>
>
> value) {
    parent.arrayArrayOfModel = value;
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
  List<PropertyReflection<ArrayTest, dynamic>> get properties => [
    arrayOfStringPart,
arrayArrayOfIntegerPart,
arrayArrayOfModelPart,
  ];

  @override
  final AdditionalPropertiesPart<ArrayTest, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ArrayTest instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ArrayTest instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<ArrayTest, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ArrayTest empty() {
    return ArrayTest(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ArrayTestReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


