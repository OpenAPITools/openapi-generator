// Model reflection

part of 'format_test.dart';


//class reflection

class FormatTestReflection extends ClassReflection<FormatTest> {
  static FormatTestReflection instanceGetter() => instance;
  static const instance = FormatTestReflection._(
    modelName: r'format_test',
    className: r'FormatTest',
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
      getter: _integerGetter,
      setter: _integerSetter,
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
      getter: _int32Getter,
      setter: _int32Setter,
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
      getter: _int64Getter,
      setter: _int64Setter,
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
      getter: _numberGetter,
      setter: _numberSetter,
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
      getter: _floatGetter,
      setter: _floatSetter,
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
      getter: _$doubleGetter,
      setter: _$doubleSetter,
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
      getter: _decimalGetter,
      setter: _decimalSetter,
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
      getter: _stringGetter,
      setter: _stringSetter,
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
      getter: _byteGetter,
      setter: _byteSetter,
    ),
    base64StrPart: PropertyReflection<FormatTest, UndefinedWrapper<
            String

>>(
      dartName: r'base64Str',
      nullable: false,
      required: false,
      oasName: r'base64Str',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _base64StrGetter,
      setter: _base64StrSetter,
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
      getter: _binaryGetter,
      setter: _binarySetter,
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
      getter: _dateGetter,
      setter: _dateSetter,
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
      getter: _dateTimeGetter,
      setter: _dateTimeSetter,
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
      getter: _uuidGetter,
      setter: _uuidSetter,
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
      getter: _uuidWithDefaultGetter,
      setter: _uuidWithDefaultSetter,
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
      getter: _passwordGetter,
      setter: _passwordSetter,
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
      getter: _patternWithDigitsGetter,
      setter: _patternWithDigitsSetter,
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
      getter: _patternWithDigitsAndDelimiterGetter,
      setter: _patternWithDigitsAndDelimiterSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<FormatTest, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const FormatTestReflection._({
    required this.modelName,
    required this.className,
    required this.integerPart,
    required this.int32Part,
    required this.int64Part,
    required this.numberPart,
    required this.floatPart,
    required this.$doublePart,
    required this.decimalPart,
    required this.stringPart,
    required this.bytePart,
    required this.base64StrPart,
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
            String

>> base64StrPart;
  static UndefinedWrapper<
            String

> _base64StrGetter(FormatTest parent) {
    return parent.base64Str;
  }
  static void _base64StrSetter(FormatTest parent, UndefinedWrapper<
            String

> value) {
    parent.base64Str = value;
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
  final Map<String, ClassReflection> discriminatorMappings;
  @override
  final Map<String, ClassReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;


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
base64StrPart,
binaryPart,
datePart,
dateTimePart,
uuidPart,
uuidWithDefaultPart,
passwordPart,
patternWithDigitsPart,
patternWithDigitsAndDelimiterPart,
  ];

  final AdditionalPropertiesReflection<FormatTest, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<FormatTest, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<FormatTest, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => FormatTest.canDeserialize(src);
  @override
  FormatTest Function(Object? src) get deserializeFunction =>
      (src) => FormatTest.deserialize(src);

  @override
  Object? Function(FormatTest src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of FormatTest.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  FormatTest example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
        discriminatorExampleResults = const {},}) {
    final _reflection = this;
    final actualDiscriminators = discriminators ?? _reflection.aggregatedDiscriminators;
    discriminatorExampleResults = Map.from(discriminatorExampleResults);
    for (final MapEntry(key: propName, value: mappings) in actualDiscriminators.entries) {
      if (discriminatorExampleResults.containsKey(propName)) {
        continue;
      }
      final r =  exampleDiscriminator(mappings);
      if (r != null){
        discriminatorExampleResults[propName] = r;
      }
    }

    final exampleResult = FormatTest(
      integer: () {
        var result = 


            
            


    
    exampleint()


;
        return UndefinedWrapper(result);
      } (),
      int32: () {
        var result = 


            
            


    
    exampleint()


;
        return UndefinedWrapper(result);
      } (),
      int64: () {
        var result = 


            
            


    
    exampleint()


;
        return UndefinedWrapper(result);
      } (),
      number: () {
        var result = 


            
            


    
    examplenum()


;
        return result;
      } (),
      float: () {
        var result = 


            
            


    
    exampledouble()


;
        return UndefinedWrapper(result);
      } (),
      $double: () {
        var result = 


            
            


    
    exampledouble()


;
        return UndefinedWrapper(result);
      } (),
      decimal: () {
        var result = 


            
            


    
    exampledouble()


;
        return UndefinedWrapper(result);
      } (),
      string: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[stringPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      byte: () {
        var result = 


            
            


    
    exampleUint8List()


;
        return result;
      } (),
      base64Str: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[base64StrPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      binary: () {
        var result = 


            
            


    
    exampleXFile()


;
        return UndefinedWrapper(result);
      } (),
      date: () {
        var result = 


            
            


    
    exampleDateTime()


;
        return result;
      } (),
      dateTime: () {
        var result = 


            
            


    
    exampleDateTime()


;
        return UndefinedWrapper(result);
      } (),
      uuid: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[uuidPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      uuidWithDefault: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[uuidWithDefaultPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      password: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[passwordPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return result;
      } (),
      patternWithDigits: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[patternWithDigitsPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      patternWithDigitsAndDelimiter: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[patternWithDigitsAndDelimiterPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class FormatTestXmlReflection {
    const FormatTestXmlReflection();
}

