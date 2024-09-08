// Model reflection

part of 'test_endpoint_parameters_request.dart';


//class reflection

class TestEndpointParametersRequestReflection extends ModelReflection<TestEndpointParametersRequest> {
  static TestEndpointParametersRequestReflection instanceGetter() => instance;
  static const instance = TestEndpointParametersRequestReflection._(
    modelName: r'testEndpointParameters_request',
    className: r'TestEndpointParametersRequest',
    xml: XmlReflection(
),
    integerPart: PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            int
>>(
      dartName: r'integer',
      nullable: false,
      required: false,
      oasName: r'integer',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_integerGetter),
      setter: FunctionWrapper2(_integerSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
),
    ),
    int32Part: PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            int
>>(
      dartName: r'int32',
      nullable: false,
      required: false,
      oasName: r'int32',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_int32Getter),
      setter: FunctionWrapper2(_int32Setter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
),
    ),
    int64Part: PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            int
>>(
      dartName: r'int64',
      nullable: false,
      required: false,
      oasName: r'int64',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_int64Getter),
      setter: FunctionWrapper2(_int64Setter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
),
    ),
    numberPart: PropertyReflection<TestEndpointParametersRequest, 
            num
>(
      dartName: r'number',
      nullable: false,
      required: true,
      oasName: r'number',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_numberGetter),
      setter: FunctionWrapper2(_numberSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.fornum
        
,
)
,
    ),
    floatPart: PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            double
>>(
      dartName: r'float',
      nullable: false,
      required: false,
      oasName: r'float',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_floatGetter),
      setter: FunctionWrapper2(_floatSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.fordouble
        
,
)
),
    ),
    $doublePart: PropertyReflection<TestEndpointParametersRequest, 
            double
>(
      dartName: r'$double',
      nullable: false,
      required: true,
      oasName: r'double',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_$doubleGetter),
      setter: FunctionWrapper2(_$doubleSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.fordouble
        
,
)
,
    ),
    stringPart: PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            String
>>(
      dartName: r'string',
      nullable: false,
      required: false,
      oasName: r'string',
      oasType: r'string',
      pattern: r'/[a-z]/i',
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_stringGetter),
      setter: FunctionWrapper2(_stringSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    patternWithoutDelimiterPart: PropertyReflection<TestEndpointParametersRequest, 
            String
>(
      dartName: r'patternWithoutDelimiter',
      nullable: false,
      required: true,
      oasName: r'pattern_without_delimiter',
      oasType: r'string',
      pattern: r'/^[A-Z].*/',
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_patternWithoutDelimiterGetter),
      setter: FunctionWrapper2(_patternWithoutDelimiterSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
,
    ),
    bytePart: PropertyReflection<TestEndpointParametersRequest, 
            Uint8List
>(
      dartName: r'byte',
      nullable: false,
      required: true,
      oasName: r'byte',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_byteGetter),
      setter: FunctionWrapper2(_byteSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forUint8List
        
,
)
,
    ),
    binaryPart: PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            XFile
>>(
      dartName: r'binary',
      nullable: false,
      required: false,
      oasName: r'binary',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_binaryGetter),
      setter: FunctionWrapper2(_binarySetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forXFile
        
,
)
),
    ),
    datePart: PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            DateTime
>>(
      dartName: r'date',
      nullable: false,
      required: false,
      oasName: r'date',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_dateGetter),
      setter: FunctionWrapper2(_dateSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forDateTime
        
,
)
),
    ),
    dateTimePart: PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            DateTime
>>(
      dartName: r'dateTime',
      nullable: false,
      required: false,
      oasName: r'dateTime',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_dateTimeGetter),
      setter: FunctionWrapper2(_dateTimeSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forDateTime
        
,
)
),
    ),
    passwordPart: PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            String
>>(
      dartName: r'password',
      nullable: false,
      required: false,
      oasName: r'password',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_passwordGetter),
      setter: FunctionWrapper2(_passwordSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    callbackPart: PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            String
>>(
      dartName: r'callback',
      nullable: false,
      required: false,
      oasName: r'callback',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_callbackGetter),
      setter: FunctionWrapper2(_callbackSetter),
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
  const TestEndpointParametersRequestReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.integerPart,
    required this.int32Part,
    required this.int64Part,
    required this.numberPart,
    required this.floatPart,
    required this.$doublePart,
    required this.stringPart,
    required this.patternWithoutDelimiterPart,
    required this.bytePart,
    required this.binaryPart,
    required this.datePart,
    required this.dateTimePart,
    required this.passwordPart,
    required this.callbackPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            int
>> integerPart;
  static UndefinedWrapper<
            int
> _integerGetter(TestEndpointParametersRequest parent) {
    return parent.integer;
  }
  static void _integerSetter(TestEndpointParametersRequest parent, UndefinedWrapper<
            int
> value) {
    parent.integer = value;
  }

  final PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            int
>> int32Part;
  static UndefinedWrapper<
            int
> _int32Getter(TestEndpointParametersRequest parent) {
    return parent.int32;
  }
  static void _int32Setter(TestEndpointParametersRequest parent, UndefinedWrapper<
            int
> value) {
    parent.int32 = value;
  }

  final PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            int
>> int64Part;
  static UndefinedWrapper<
            int
> _int64Getter(TestEndpointParametersRequest parent) {
    return parent.int64;
  }
  static void _int64Setter(TestEndpointParametersRequest parent, UndefinedWrapper<
            int
> value) {
    parent.int64 = value;
  }

  final PropertyReflection<TestEndpointParametersRequest, 
            num
> numberPart;
  static 
            num
 _numberGetter(TestEndpointParametersRequest parent) {
    return parent.number;
  }
  static void _numberSetter(TestEndpointParametersRequest parent, 
            num
 value) {
    parent.number = value;
  }

  final PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            double
>> floatPart;
  static UndefinedWrapper<
            double
> _floatGetter(TestEndpointParametersRequest parent) {
    return parent.float;
  }
  static void _floatSetter(TestEndpointParametersRequest parent, UndefinedWrapper<
            double
> value) {
    parent.float = value;
  }

  final PropertyReflection<TestEndpointParametersRequest, 
            double
> $doublePart;
  static 
            double
 _$doubleGetter(TestEndpointParametersRequest parent) {
    return parent.$double;
  }
  static void _$doubleSetter(TestEndpointParametersRequest parent, 
            double
 value) {
    parent.$double = value;
  }

  final PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            String
>> stringPart;
  static UndefinedWrapper<
            String
> _stringGetter(TestEndpointParametersRequest parent) {
    return parent.string;
  }
  static void _stringSetter(TestEndpointParametersRequest parent, UndefinedWrapper<
            String
> value) {
    parent.string = value;
  }

  final PropertyReflection<TestEndpointParametersRequest, 
            String
> patternWithoutDelimiterPart;
  static 
            String
 _patternWithoutDelimiterGetter(TestEndpointParametersRequest parent) {
    return parent.patternWithoutDelimiter;
  }
  static void _patternWithoutDelimiterSetter(TestEndpointParametersRequest parent, 
            String
 value) {
    parent.patternWithoutDelimiter = value;
  }

  final PropertyReflection<TestEndpointParametersRequest, 
            Uint8List
> bytePart;
  static 
            Uint8List
 _byteGetter(TestEndpointParametersRequest parent) {
    return parent.byte;
  }
  static void _byteSetter(TestEndpointParametersRequest parent, 
            Uint8List
 value) {
    parent.byte = value;
  }

  final PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            XFile
>> binaryPart;
  static UndefinedWrapper<
            XFile
> _binaryGetter(TestEndpointParametersRequest parent) {
    return parent.binary;
  }
  static void _binarySetter(TestEndpointParametersRequest parent, UndefinedWrapper<
            XFile
> value) {
    parent.binary = value;
  }

  final PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            DateTime
>> datePart;
  static UndefinedWrapper<
            DateTime
> _dateGetter(TestEndpointParametersRequest parent) {
    return parent.date;
  }
  static void _dateSetter(TestEndpointParametersRequest parent, UndefinedWrapper<
            DateTime
> value) {
    parent.date = value;
  }

  final PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            DateTime
>> dateTimePart;
  static UndefinedWrapper<
            DateTime
> _dateTimeGetter(TestEndpointParametersRequest parent) {
    return parent.dateTime;
  }
  static void _dateTimeSetter(TestEndpointParametersRequest parent, UndefinedWrapper<
            DateTime
> value) {
    parent.dateTime = value;
  }

  final PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            String
>> passwordPart;
  static UndefinedWrapper<
            String
> _passwordGetter(TestEndpointParametersRequest parent) {
    return parent.password;
  }
  static void _passwordSetter(TestEndpointParametersRequest parent, UndefinedWrapper<
            String
> value) {
    parent.password = value;
  }

  final PropertyReflection<TestEndpointParametersRequest, UndefinedWrapper<
            String
>> callbackPart;
  static UndefinedWrapper<
            String
> _callbackGetter(TestEndpointParametersRequest parent) {
    return parent.callback;
  }
  static void _callbackSetter(TestEndpointParametersRequest parent, UndefinedWrapper<
            String
> value) {
    parent.callback = value;
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
  List<PropertyReflection<TestEndpointParametersRequest, dynamic>> get properties => [
    integerPart,
int32Part,
int64Part,
numberPart,
floatPart,
$doublePart,
stringPart,
patternWithoutDelimiterPart,
bytePart,
binaryPart,
datePart,
dateTimePart,
passwordPart,
callbackPart,
  ];

  @override
  final AdditionalPropertiesPart<TestEndpointParametersRequest, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(TestEndpointParametersRequest instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(TestEndpointParametersRequest instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<TestEndpointParametersRequest, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  TestEndpointParametersRequest empty() {
    return TestEndpointParametersRequest(
      number: numberPart.reflection.emptyFunction(),
      $double: $doublePart.reflection.emptyFunction(),
      patternWithoutDelimiter: patternWithoutDelimiterPart.reflection.emptyFunction(),
      byte: bytePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is TestEndpointParametersRequestReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


