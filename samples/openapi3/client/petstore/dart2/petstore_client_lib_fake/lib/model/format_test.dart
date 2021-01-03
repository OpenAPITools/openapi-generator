//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

@JsonSerializable(
  checked: true,
  includeIfNull: false,
  disallowUnrecognizedKeys: true,
)
class FormatTest {
  /// Returns a new [FormatTest] instance.
  FormatTest({
    this.integer,
    this.int32,
    this.int64,
    @required this.number,
    this.float,
    this.double_,
    this.decimal,
    this.string,
    @required this.byte,
    this.binary,
    @required this.date,
    this.dateTime,
    this.uuid,
    @required this.password,
    this.patternWithDigits,
    this.patternWithDigitsAndDelimiter,
  });

  // minimum: 10
  // maximum: 100
  @JsonKey(
    name: r'integer',
    
    
    
  )
  int integer;

  // minimum: 20
  // maximum: 200
  @JsonKey(
    name: r'int32',
    
    
    
  )
  int int32;

  @JsonKey(
    name: r'int64',
    
    
    
  )
  int int64;

  // minimum: 32.1
  // maximum: 543.2
  @JsonKey(
    name: r'number',
    required: true,
    
    
  )
  num number;

  // minimum: 54.3
  // maximum: 987.6
  @JsonKey(
    name: r'float',
    
    
    
  )
  double float;

  // minimum: 67.8
  // maximum: 123.4
  @JsonKey(
    name: r'double_',
    
    
    
  )
  double double_;

  @JsonKey(
    name: r'decimal',
    
    
    
  )
  double decimal;

  @JsonKey(
    name: r'string',
    
    
    
  )
  String string;

  @JsonKey(
    name: r'byte',
    required: true,
    
    
  )
  String byte;

  @JsonKey(ignore: true)
  @JsonKey(
    name: r'binary',
    
    
    
  )
  MultipartFile binary;

  @JsonKey(
    name: r'date',
    required: true,
    
    
  )
  DateTime date;

  @JsonKey(
    name: r'dateTime',
    
    
    
  )
  DateTime dateTime;

  @JsonKey(
    name: r'uuid',
    
    
    
  )
  String uuid;

  @JsonKey(
    name: r'password',
    required: true,
    
    
  )
  String password;

  /// A string that is a 10 digit number. Can have leading zeros.
  @JsonKey(
    name: r'patternWithDigits',
    
    
    
  )
  String patternWithDigits;

  /// A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.
  @JsonKey(
    name: r'patternWithDigitsAndDelimiter',
    
    
    
  )
  String patternWithDigitsAndDelimiter;

  @override
  bool operator ==(Object other) => identical(this, other) || other is FormatTest &&
     other.integer == integer &&
     other.int32 == int32 &&
     other.int64 == int64 &&
     other.number == number &&
     other.float == float &&
     other.double_ == double_ &&
     other.decimal == decimal &&
     other.string == string &&
     other.byte == byte &&
     other.binary == binary &&
     other.date == date &&
     other.dateTime == dateTime &&
     other.uuid == uuid &&
     other.password == password &&
     other.patternWithDigits == patternWithDigits &&
     other.patternWithDigitsAndDelimiter == patternWithDigitsAndDelimiter;

  @override
  int get hashCode =>
    (integer == null ? 0 : integer.hashCode) +
    (int32 == null ? 0 : int32.hashCode) +
    (int64 == null ? 0 : int64.hashCode) +
    (number == null ? 0 : number.hashCode) +
    (float == null ? 0 : float.hashCode) +
    (double_ == null ? 0 : double_.hashCode) +
    (decimal == null ? 0 : decimal.hashCode) +
    (string == null ? 0 : string.hashCode) +
    (byte == null ? 0 : byte.hashCode) +
    (binary == null ? 0 : binary.hashCode) +
    (date == null ? 0 : date.hashCode) +
    (dateTime == null ? 0 : dateTime.hashCode) +
    (uuid == null ? 0 : uuid.hashCode) +
    (password == null ? 0 : password.hashCode) +
    (patternWithDigits == null ? 0 : patternWithDigits.hashCode) +
    (patternWithDigitsAndDelimiter == null ? 0 : patternWithDigitsAndDelimiter.hashCode);

  @override
  String toString() => toJson().toString();

  factory FormatTest.fromJson(Map<String, dynamic> json) => _$FormatTestFromJson(json);
  Map<String, dynamic> toJson() => _$FormatTestToJson(this);
}

