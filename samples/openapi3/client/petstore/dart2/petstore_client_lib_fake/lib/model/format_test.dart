//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class FormatTest {
  /// Returns a new [FormatTest] instance.
  FormatTest({
    this.integer,
    this.int32,
    this.int64,
    @required this.number,
    this.float,
    this.double,
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
  int integer;

  
  // minimum: 20
  // maximum: 200
  int int32;

  
  int int64;

  
  // minimum: 32.1
  // maximum: 543.2
  num number;

  
  // minimum: 54.3
  // maximum: 987.6
  double float;

  
  // minimum: 67.8
  // maximum: 123.4
  double double;

  
  Decimal decimal;

  
  String string;

  
  String byte;

  
  MultipartFile binary;

  
  DateTime date;

  
  DateTime dateTime;

  
  String uuid;

  
  String password;

  /// A string that is a 10 digit number. Can have leading zeros.
  String patternWithDigits;

  /// A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.
  String patternWithDigitsAndDelimiter;

  @override
  bool operator ==(Object other) => identical(this, other) || other is FormatTest &&
     other.integer == integer &&
     other.int32 == int32 &&
     other.int64 == int64 &&
     other.number == number &&
     other.float == float &&
     other.double == double &&
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
    (double == null ? 0 : double.hashCode) +
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
  String toString() => 'FormatTest[integer=$integer, int32=$int32, int64=$int64, number=$number, float=$float, double=$double, decimal=$decimal, string=$string, byte=$byte, binary=$binary, date=$date, dateTime=$dateTime, uuid=$uuid, password=$password, patternWithDigits=$patternWithDigits, patternWithDigitsAndDelimiter=$patternWithDigitsAndDelimiter]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (integer != null) {
      json['integer'] = integer;
    }
    if (int32 != null) {
      json['int32'] = int32;
    }
    if (int64 != null) {
      json['int64'] = int64;
    }
    if (number != null) {
      json['number'] = number;
    }
    if (float != null) {
      json['float'] = float;
    }
    if (double != null) {
      json['double'] = double;
    }
    if (decimal != null) {
      json['decimal'] = decimal;
    }
    if (string != null) {
      json['string'] = string;
    }
    if (byte != null) {
      json['byte'] = byte;
    }
    if (binary != null) {
      json['binary'] = binary;
    }
    if (date != null) {
      json['date'] = _dateFormatter.format(date.toUtc());
    }
    if (dateTime != null) {
      json['dateTime'] = dateTime.toUtc().toIso8601String();
    }
    if (uuid != null) {
      json['uuid'] = uuid;
    }
    if (password != null) {
      json['password'] = password;
    }
    if (patternWithDigits != null) {
      json['pattern_with_digits'] = patternWithDigits;
    }
    if (patternWithDigitsAndDelimiter != null) {
      json['pattern_with_digits_and_delimiter'] = patternWithDigitsAndDelimiter;
    }
    return json;
  }

  /// Returns a new [FormatTest] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static FormatTest fromJson(Map<String, dynamic> json) => json == null
    ? null
    : FormatTest(
        integer: json['integer'],
        int32: json['int32'],
        int64: json['int64'],
        number: json['number'] == null ?
          null :
          json['number'].toDouble(),
        float: json['float'],
        double: json['double'],
        decimal: Decimal.fromJson(json['decimal']),
        string: json['string'],
        byte: json['byte'],
        binary: null, // No support for decoding binary content from JSON
        date: json['date'] == null
          ? null
          : DateTime.parse(json['date']),
        dateTime: json['dateTime'] == null
          ? null
          : DateTime.parse(json['dateTime']),
        uuid: json['uuid'],
        password: json['password'],
        patternWithDigits: json['pattern_with_digits'],
        patternWithDigitsAndDelimiter: json['pattern_with_digits_and_delimiter'],
    );

  static List<FormatTest> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <FormatTest>[]
      : json.map((v) => FormatTest.fromJson(v)).toList(growable: true == growable);

  static Map<String, FormatTest> mapFromJson(Map<String, dynamic> json) {
    final map = <String, FormatTest>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = FormatTest.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of FormatTest-objects as value to a dart map
  static Map<String, List<FormatTest>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<FormatTest>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = FormatTest.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

