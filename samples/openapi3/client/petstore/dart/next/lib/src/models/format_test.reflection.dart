// Model reflection

part of 'format_test.dart';


//class reflection

class FormatTestReflection extends ModelReflection<FormatTest> {
  static FormatTestReflection instanceGetter() => instance;
  static const instance = FormatTestReflection._(
    modelName: r'format_test',
    className: r'FormatTest',
    xml: XmlReflection(
),
    integerPart: PropertyReflection<FormatTest, UndefinedWrapper<
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
    int32Part: PropertyReflection<FormatTest, UndefinedWrapper<
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
    int64Part: PropertyReflection<FormatTest, UndefinedWrapper<
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
    numberPart: PropertyReflection<FormatTest, 
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
    floatPart: PropertyReflection<FormatTest, UndefinedWrapper<
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
    $doublePart: PropertyReflection<FormatTest, UndefinedWrapper<
            double
>>(
      dartName: r'$double',
      nullable: false,
      required: false,
      oasName: r'double',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_$doubleGetter),
      setter: FunctionWrapper2(_$doubleSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.fordouble
        
,
)
),
    ),
    decimalPart: PropertyReflection<FormatTest, UndefinedWrapper<
            double
>>(
      dartName: r'decimal',
      nullable: false,
      required: false,
      oasName: r'decimal',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_decimalGetter),
      setter: FunctionWrapper2(_decimalSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.fordouble
        
,
)
),
    ),
    stringPart: PropertyReflection<FormatTest, UndefinedWrapper<
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
    bytePart: PropertyReflection<FormatTest, 
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
    binaryPart: PropertyReflection<FormatTest, UndefinedWrapper<
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
    datePart: PropertyReflection<FormatTest, 
            DateTime
>(
      dartName: r'date',
      nullable: false,
      required: true,
      oasName: r'date',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_dateGetter),
      setter: FunctionWrapper2(_dateSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forDateTime
        
,
)
,
    ),
    dateTimePart: PropertyReflection<FormatTest, UndefinedWrapper<
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
    uuidPart: PropertyReflection<FormatTest, UndefinedWrapper<
            String
>>(
      dartName: r'uuid',
      nullable: false,
      required: false,
      oasName: r'uuid',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_uuidGetter),
      setter: FunctionWrapper2(_uuidSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    uuidWithDefaultPart: PropertyReflection<FormatTest, UndefinedWrapper<
            String
>>(
      dartName: r'uuidWithDefault',
      nullable: false,
      required: false,
      oasName: r'uuid_with_default',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_uuidWithDefaultGetter),
      setter: FunctionWrapper2(_uuidWithDefaultSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    passwordPart: PropertyReflection<FormatTest, 
            String
>(
      dartName: r'password',
      nullable: false,
      required: true,
      oasName: r'password',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_passwordGetter),
      setter: FunctionWrapper2(_passwordSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
,
    ),
    patternWithDigitsPart: PropertyReflection<FormatTest, UndefinedWrapper<
            String
>>(
      dartName: r'patternWithDigits',
      nullable: false,
      required: false,
      oasName: r'pattern_with_digits',
      oasType: r'string',
      pattern: r'/^\\d{10}$/',
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_patternWithDigitsGetter),
      setter: FunctionWrapper2(_patternWithDigitsSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    patternWithDigitsAndDelimiterPart: PropertyReflection<FormatTest, UndefinedWrapper<
            String
>>(
      dartName: r'patternWithDigitsAndDelimiter',
      nullable: false,
      required: false,
      oasName: r'pattern_with_digits_and_delimiter',
      oasType: r'string',
      pattern: r'/^image_\\d{1,3}$/i',
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_patternWithDigitsAndDelimiterGetter),
      setter: FunctionWrapper2(_patternWithDigitsAndDelimiterSetter),
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
  const FormatTestReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.integerPart,
    required this.int32Part,
    required this.int64Part,
    required this.numberPart,
    required this.floatPart,
    required this.$doublePart,
    required this.decimalPart,
    required this.stringPart,
    required this.bytePart,
    required this.binaryPart,
    required this.datePart,
    required this.dateTimePart,
    required this.uuidPart,
    required this.uuidWithDefaultPart,
    required this.passwordPart,
    required this.patternWithDigitsPart,
    required this.patternWithDigitsAndDelimiterPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<FormatTest, UndefinedWrapper<
            int
>> integerPart;
  static UndefinedWrapper<
            int
> _integerGetter(FormatTest parent) {
    return parent.integer;
  }
  static void _integerSetter(FormatTest parent, UndefinedWrapper<
            int
> value) {
    parent.integer = value;
  }

  final PropertyReflection<FormatTest, UndefinedWrapper<
            int
>> int32Part;
  static UndefinedWrapper<
            int
> _int32Getter(FormatTest parent) {
    return parent.int32;
  }
  static void _int32Setter(FormatTest parent, UndefinedWrapper<
            int
> value) {
    parent.int32 = value;
  }

  final PropertyReflection<FormatTest, UndefinedWrapper<
            int
>> int64Part;
  static UndefinedWrapper<
            int
> _int64Getter(FormatTest parent) {
    return parent.int64;
  }
  static void _int64Setter(FormatTest parent, UndefinedWrapper<
            int
> value) {
    parent.int64 = value;
  }

  final PropertyReflection<FormatTest, 
            num
> numberPart;
  static 
            num
 _numberGetter(FormatTest parent) {
    return parent.number;
  }
  static void _numberSetter(FormatTest parent, 
            num
 value) {
    parent.number = value;
  }

  final PropertyReflection<FormatTest, UndefinedWrapper<
            double
>> floatPart;
  static UndefinedWrapper<
            double
> _floatGetter(FormatTest parent) {
    return parent.float;
  }
  static void _floatSetter(FormatTest parent, UndefinedWrapper<
            double
> value) {
    parent.float = value;
  }

  final PropertyReflection<FormatTest, UndefinedWrapper<
            double
>> $doublePart;
  static UndefinedWrapper<
            double
> _$doubleGetter(FormatTest parent) {
    return parent.$double;
  }
  static void _$doubleSetter(FormatTest parent, UndefinedWrapper<
            double
> value) {
    parent.$double = value;
  }

  final PropertyReflection<FormatTest, UndefinedWrapper<
            double
>> decimalPart;
  static UndefinedWrapper<
            double
> _decimalGetter(FormatTest parent) {
    return parent.decimal;
  }
  static void _decimalSetter(FormatTest parent, UndefinedWrapper<
            double
> value) {
    parent.decimal = value;
  }

  final PropertyReflection<FormatTest, UndefinedWrapper<
            String
>> stringPart;
  static UndefinedWrapper<
            String
> _stringGetter(FormatTest parent) {
    return parent.string;
  }
  static void _stringSetter(FormatTest parent, UndefinedWrapper<
            String
> value) {
    parent.string = value;
  }

  final PropertyReflection<FormatTest, 
            Uint8List
> bytePart;
  static 
            Uint8List
 _byteGetter(FormatTest parent) {
    return parent.byte;
  }
  static void _byteSetter(FormatTest parent, 
            Uint8List
 value) {
    parent.byte = value;
  }

  final PropertyReflection<FormatTest, UndefinedWrapper<
            XFile
>> binaryPart;
  static UndefinedWrapper<
            XFile
> _binaryGetter(FormatTest parent) {
    return parent.binary;
  }
  static void _binarySetter(FormatTest parent, UndefinedWrapper<
            XFile
> value) {
    parent.binary = value;
  }

  final PropertyReflection<FormatTest, 
            DateTime
> datePart;
  static 
            DateTime
 _dateGetter(FormatTest parent) {
    return parent.date;
  }
  static void _dateSetter(FormatTest parent, 
            DateTime
 value) {
    parent.date = value;
  }

  final PropertyReflection<FormatTest, UndefinedWrapper<
            DateTime
>> dateTimePart;
  static UndefinedWrapper<
            DateTime
> _dateTimeGetter(FormatTest parent) {
    return parent.dateTime;
  }
  static void _dateTimeSetter(FormatTest parent, UndefinedWrapper<
            DateTime
> value) {
    parent.dateTime = value;
  }

  final PropertyReflection<FormatTest, UndefinedWrapper<
            String
>> uuidPart;
  static UndefinedWrapper<
            String
> _uuidGetter(FormatTest parent) {
    return parent.uuid;
  }
  static void _uuidSetter(FormatTest parent, UndefinedWrapper<
            String
> value) {
    parent.uuid = value;
  }

  final PropertyReflection<FormatTest, UndefinedWrapper<
            String
>> uuidWithDefaultPart;
  static UndefinedWrapper<
            String
> _uuidWithDefaultGetter(FormatTest parent) {
    return parent.uuidWithDefault;
  }
  static void _uuidWithDefaultSetter(FormatTest parent, UndefinedWrapper<
            String
> value) {
    parent.uuidWithDefault = value;
  }

  final PropertyReflection<FormatTest, 
            String
> passwordPart;
  static 
            String
 _passwordGetter(FormatTest parent) {
    return parent.password;
  }
  static void _passwordSetter(FormatTest parent, 
            String
 value) {
    parent.password = value;
  }

  final PropertyReflection<FormatTest, UndefinedWrapper<
            String
>> patternWithDigitsPart;
  static UndefinedWrapper<
            String
> _patternWithDigitsGetter(FormatTest parent) {
    return parent.patternWithDigits;
  }
  static void _patternWithDigitsSetter(FormatTest parent, UndefinedWrapper<
            String
> value) {
    parent.patternWithDigits = value;
  }

  final PropertyReflection<FormatTest, UndefinedWrapper<
            String
>> patternWithDigitsAndDelimiterPart;
  static UndefinedWrapper<
            String
> _patternWithDigitsAndDelimiterGetter(FormatTest parent) {
    return parent.patternWithDigitsAndDelimiter;
  }
  static void _patternWithDigitsAndDelimiterSetter(FormatTest parent, UndefinedWrapper<
            String
> value) {
    parent.patternWithDigitsAndDelimiter = value;
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
  List<PropertyReflection<FormatTest, dynamic>> get properties => [
    integerPart,
int32Part,
int64Part,
numberPart,
floatPart,
$doublePart,
decimalPart,
stringPart,
bytePart,
binaryPart,
datePart,
dateTimePart,
uuidPart,
uuidWithDefaultPart,
passwordPart,
patternWithDigitsPart,
patternWithDigitsAndDelimiterPart,
  ];

  @override
  final AdditionalPropertiesPart<FormatTest, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(FormatTest instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(FormatTest instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<FormatTest, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  FormatTest empty() {
    return FormatTest(
      number: numberPart.reflection.emptyFunction(),
      byte: bytePart.reflection.emptyFunction(),
      date: datePart.reflection.emptyFunction(),
      password: passwordPart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is FormatTestReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


