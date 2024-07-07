// Model reflection

part of 'test_endpoint_parameters_request.dart';


//class reflection

class TestEndpointParametersRequestReflection extends ClassReflection<TestEndpointParametersRequest> {
  static TestEndpointParametersRequestReflection instanceGetter() => instance;
  static const instance = TestEndpointParametersRequestReflection._(
    modelName: r'testEndpointParameters_request',
    className: r'TestEndpointParametersRequest',
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
      getter: _integerGetter,
      setter: _integerSetter,
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
      getter: _int32Getter,
      setter: _int32Setter,
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
      getter: _int64Getter,
      setter: _int64Setter,
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
      getter: _numberGetter,
      setter: _numberSetter,
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
      getter: _floatGetter,
      setter: _floatSetter,
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
      getter: _$doubleGetter,
      setter: _$doubleSetter,
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
      getter: _stringGetter,
      setter: _stringSetter,
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
      getter: _patternWithoutDelimiterGetter,
      setter: _patternWithoutDelimiterSetter,
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
      getter: _byteGetter,
      setter: _byteSetter,
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
      getter: _binaryGetter,
      setter: _binarySetter,
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
      getter: _dateGetter,
      setter: _dateSetter,
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
      getter: _dateTimeGetter,
      setter: _dateTimeSetter,
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
      getter: _passwordGetter,
      setter: _passwordSetter,
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
      getter: _callbackGetter,
      setter: _callbackSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<TestEndpointParametersRequest, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const TestEndpointParametersRequestReflection._({
    required this.modelName,
    required this.className,
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

  final AdditionalPropertiesReflection<TestEndpointParametersRequest, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<TestEndpointParametersRequest, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<TestEndpointParametersRequest, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => TestEndpointParametersRequest.canDeserialize(src);
  @override
  TestEndpointParametersRequest Function(Object? src) get deserializeFunction =>
      (src) => TestEndpointParametersRequest.deserialize(src);

  @override
  Object? Function(TestEndpointParametersRequest src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of TestEndpointParametersRequest.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  TestEndpointParametersRequest example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return TestEndpointParametersRequest(
      integer: () {
        PartReflection? _partReflection = _reflection.integerPart;
        
        return UndefinedWrapper(


            
            


    
    exampleint()


);
      }(),
      int32: () {
        PartReflection? _partReflection = _reflection.int32Part;
        
        return UndefinedWrapper(


            
            


    
    exampleint()


);
      }(),
      int64: () {
        PartReflection? _partReflection = _reflection.int64Part;
        
        return UndefinedWrapper(


            
            


    
    exampleint()


);
      }(),
      number: () {
        PartReflection? _partReflection = _reflection.numberPart;
        
        return 


            
            


    
    examplenum()


;
      }(),
      float: () {
        PartReflection? _partReflection = _reflection.floatPart;
        
        return UndefinedWrapper(


            
            


    
    exampledouble()


);
      }(),
      $double: () {
        PartReflection? _partReflection = _reflection.$doublePart;
        
        return 


            
            


    
    exampledouble()


;
      }(),
      string: () {
        PartReflection? _partReflection = _reflection.stringPart;
        
        final disc = discriminators[r'string'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return UndefinedWrapper(result);
          }
        }
        
        return UndefinedWrapper(


            
            


    
    exampleString()


);
      }(),
      patternWithoutDelimiter: () {
        PartReflection? _partReflection = _reflection.patternWithoutDelimiterPart;
        
        final disc = discriminators[r'pattern_without_delimiter'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return result;
          }
        }
        
        return 


            
            


    
    exampleString()


;
      }(),
      byte: () {
        PartReflection? _partReflection = _reflection.bytePart;
        
        return 


            
            


    
    exampleUint8List()


;
      }(),
      binary: () {
        PartReflection? _partReflection = _reflection.binaryPart;
        
        return UndefinedWrapper(


            
            


    
    exampleXFile()


);
      }(),
      date: () {
        PartReflection? _partReflection = _reflection.datePart;
        
        return UndefinedWrapper(


            
            


    
    exampleDateTime()


);
      }(),
      dateTime: () {
        PartReflection? _partReflection = _reflection.dateTimePart;
        
        return UndefinedWrapper(


            
            


    
    exampleDateTime()


);
      }(),
      password: () {
        PartReflection? _partReflection = _reflection.passwordPart;
        
        final disc = discriminators[r'password'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return UndefinedWrapper(result);
          }
        }
        
        return UndefinedWrapper(


            
            


    
    exampleString()


);
      }(),
      callback: () {
        PartReflection? _partReflection = _reflection.callbackPart;
        
        final disc = discriminators[r'callback'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return UndefinedWrapper(result);
          }
        }
        
        return UndefinedWrapper(


            
            


    
    exampleString()


);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class TestEndpointParametersRequestXmlReflection {
    const TestEndpointParametersRequestXmlReflection();
}

