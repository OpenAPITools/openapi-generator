// Model reflection

part of 'format_test.dart';


//class reflection

class FormatTestReflection extends ClassReflection<FormatTest> {
  static const instance = FormatTestReflection._(
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
      required: false,
      oasName: r'double',
      oasType: r'number',
      pattern: null,
    ),
    decimal: PropertyReflection(
      dartName: r'decimal',
      nullable: false,
      required: false,
      oasName: r'decimal',
      oasType: r'string',
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
      required: true,
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
    uuid: PropertyReflection(
      dartName: r'uuid',
      nullable: false,
      required: false,
      oasName: r'uuid',
      oasType: r'string',
      pattern: null,
    ),
    uuidWithDefault: PropertyReflection(
      dartName: r'uuidWithDefault',
      nullable: false,
      required: false,
      oasName: r'uuid_with_default',
      oasType: r'string',
      pattern: null,
    ),
    password: PropertyReflection(
      dartName: r'password',
      nullable: false,
      required: true,
      oasName: r'password',
      oasType: r'string',
      pattern: null,
    ),
    patternWithDigits: PropertyReflection(
      dartName: r'patternWithDigits',
      nullable: false,
      required: false,
      oasName: r'pattern_with_digits',
      oasType: r'string',
      pattern: r'/^\\d{10}$/',
    ),
    patternWithDigitsAndDelimiter: PropertyReflection(
      dartName: r'patternWithDigitsAndDelimiter',
      nullable: false,
      required: false,
      oasName: r'pattern_with_digits_and_delimiter',
      oasType: r'string',
      pattern: r'/^image_\\d{1,3}$/i',
    ),
  );
  const FormatTestReflection._({
    required this.integer,
  
    required this.int32,
  
    required this.int64,
  
    required this.number,
  
    required this.float,
  
    required this.$double,
  
    required this.decimal,
  
    required this.string,
  
    required this.byte,
  
    required this.binary,
  
    required this.date,
  
    required this.dateTime,
  
    required this.uuid,
  
    required this.uuidWithDefault,
  
    required this.password,
  
    required this.patternWithDigits,
  
    required this.patternWithDigitsAndDelimiter,
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
  final PropertyReflection<UndefinedWrapper<
            double
>> $double;
  final PropertyReflection<UndefinedWrapper<
            double
>> decimal;
  final PropertyReflection<UndefinedWrapper<
            String
>> string;
  final PropertyReflection<
            Uint8List
> byte;
  final PropertyReflection<UndefinedWrapper<
            XFile
>> binary;
  final PropertyReflection<
            DateTime
> date;
  final PropertyReflection<UndefinedWrapper<
            DateTime
>> dateTime;
  final PropertyReflection<UndefinedWrapper<
            String
>> uuid;
  final PropertyReflection<UndefinedWrapper<
            String
>> uuidWithDefault;
  final PropertyReflection<
            String
> password;
  final PropertyReflection<UndefinedWrapper<
            String
>> patternWithDigits;
  final PropertyReflection<UndefinedWrapper<
            String
>> patternWithDigitsAndDelimiter;

  @override
  List<PropertyReflection> get members => [
    integer,
int32,
int64,
number,
float,
$double,
decimal,
string,
byte,
binary,
date,
dateTime,
uuid,
uuidWithDefault,
password,
patternWithDigits,
patternWithDigitsAndDelimiter,
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
}

class FormatTestXmlReflection {
    const FormatTestXmlReflection();
}

