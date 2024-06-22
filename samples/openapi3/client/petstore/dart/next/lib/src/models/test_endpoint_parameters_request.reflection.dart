// Model reflection

part of 'test_endpoint_parameters_request.dart';


//class reflection

class TestEndpointParametersRequestReflection extends ClassReflection<TestEndpointParametersRequest> {
  static const instance = TestEndpointParametersRequestReflection._(
    integer: PropertyReflection(
      dartName: r'integer',
      nullable: false,
      required: false,
      oasName: r'integer',
      oasType: r'integer',
      pattern: null,
    ),
    int32: PropertyReflection(
      dartName: r'int32',
      nullable: false,
      required: false,
      oasName: r'int32',
      oasType: r'integer',
      pattern: null,
    ),
    int64: PropertyReflection(
      dartName: r'int64',
      nullable: false,
      required: false,
      oasName: r'int64',
      oasType: r'integer',
      pattern: null,
    ),
    number: PropertyReflection(
      dartName: r'number',
      nullable: false,
      required: true,
      oasName: r'number',
      oasType: r'number',
      pattern: null,
    ),
    float: PropertyReflection(
      dartName: r'float',
      nullable: false,
      required: false,
      oasName: r'float',
      oasType: r'number',
      pattern: null,
    ),
    $double: PropertyReflection(
      dartName: r'$double',
      nullable: false,
      required: true,
      oasName: r'double',
      oasType: r'number',
      pattern: null,
    ),
    string: PropertyReflection(
      dartName: r'string',
      nullable: false,
      required: false,
      oasName: r'string',
      oasType: r'string',
      pattern: r'/[a-z]/i',
    ),
    patternWithoutDelimiter: PropertyReflection(
      dartName: r'patternWithoutDelimiter',
      nullable: false,
      required: true,
      oasName: r'pattern_without_delimiter',
      oasType: r'string',
      pattern: r'/^[A-Z].*/',
    ),
    byte: PropertyReflection(
      dartName: r'byte',
      nullable: false,
      required: true,
      oasName: r'byte',
      oasType: r'string',
      pattern: null,
    ),
    binary: PropertyReflection(
      dartName: r'binary',
      nullable: false,
      required: false,
      oasName: r'binary',
      oasType: r'string',
      pattern: null,
    ),
    date: PropertyReflection(
      dartName: r'date',
      nullable: false,
      required: false,
      oasName: r'date',
      oasType: r'string',
      pattern: null,
    ),
    dateTime: PropertyReflection(
      dartName: r'dateTime',
      nullable: false,
      required: false,
      oasName: r'dateTime',
      oasType: r'string',
      pattern: null,
    ),
    password: PropertyReflection(
      dartName: r'password',
      nullable: false,
      required: false,
      oasName: r'password',
      oasType: r'string',
      pattern: null,
    ),
    callback: PropertyReflection(
      dartName: r'callback',
      nullable: false,
      required: false,
      oasName: r'callback',
      oasType: r'string',
      pattern: null,
    ),
  );
  const TestEndpointParametersRequestReflection._({
    required this.integer,
  
    required this.int32,
  
    required this.int64,
  
    required this.number,
  
    required this.float,
  
    required this.$double,
  
    required this.string,
  
    required this.patternWithoutDelimiter,
  
    required this.byte,
  
    required this.binary,
  
    required this.date,
  
    required this.dateTime,
  
    required this.password,
  
    required this.callback,
  });

  final PropertyReflection<UndefinedWrapper<
            int
>> integer;
  final PropertyReflection<UndefinedWrapper<
            int
>> int32;
  final PropertyReflection<UndefinedWrapper<
            int
>> int64;
  final PropertyReflection<
            num
> number;
  final PropertyReflection<UndefinedWrapper<
            double
>> float;
  final PropertyReflection<
            double
> $double;
  final PropertyReflection<UndefinedWrapper<
            String
>> string;
  final PropertyReflection<
            String
> patternWithoutDelimiter;
  final PropertyReflection<
            Uint8List
> byte;
  final PropertyReflection<UndefinedWrapper<
            XFile
>> binary;
  final PropertyReflection<UndefinedWrapper<
            DateTime
>> date;
  final PropertyReflection<UndefinedWrapper<
            DateTime
>> dateTime;
  final PropertyReflection<UndefinedWrapper<
            String
>> password;
  final PropertyReflection<UndefinedWrapper<
            String
>> callback;

  @override
  List<PropertyReflection> get members => [
    integer,
int32,
int64,
number,
float,
$double,
string,
patternWithoutDelimiter,
byte,
binary,
date,
dateTime,
password,
callback,
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
}

class TestEndpointParametersRequestXmlReflection {
    const TestEndpointParametersRequestXmlReflection();
}

