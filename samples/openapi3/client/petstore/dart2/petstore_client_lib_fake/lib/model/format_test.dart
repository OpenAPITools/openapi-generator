//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
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
  double double_;

  double decimal;

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
  // ignore: unnecessary_parenthesis
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
  String toString() => 'FormatTest[integer=$integer, int32=$int32, int64=$int64, number=$number, float=$float, double_=$double_, decimal=$decimal, string=$string, byte=$byte, binary=$binary, date=$date, dateTime=$dateTime, uuid=$uuid, password=$password, patternWithDigits=$patternWithDigits, patternWithDigitsAndDelimiter=$patternWithDigitsAndDelimiter]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (integer != null) {
      json[r'integer'] = integer;
    }
    if (int32 != null) {
      json[r'int32'] = int32;
    }
    if (int64 != null) {
      json[r'int64'] = int64;
    }
      json[r'number'] = number;
    if (float != null) {
      json[r'float'] = float;
    }
    if (double_ != null) {
      json[r'double'] = double_;
    }
    if (decimal != null) {
      json[r'decimal'] = decimal;
    }
    if (string != null) {
      json[r'string'] = string;
    }
      json[r'byte'] = byte;
    if (binary != null) {
      json[r'binary'] = binary;
    }
      json[r'date'] = _dateFormatter.format(date.toUtc());
    if (dateTime != null) {
      json[r'dateTime'] = dateTime.toUtc().toIso8601String();
    }
    if (uuid != null) {
      json[r'uuid'] = uuid;
    }
      json[r'password'] = password;
    if (patternWithDigits != null) {
      json[r'pattern_with_digits'] = patternWithDigits;
    }
    if (patternWithDigitsAndDelimiter != null) {
      json[r'pattern_with_digits_and_delimiter'] = patternWithDigitsAndDelimiter;
    }
    return json;
  }

  /// Returns a new [FormatTest] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static FormatTest fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return FormatTest(
        integer: mapValueOfType<int>(json, r'integer'),
        int32: mapValueOfType<int>(json, r'int32'),
        int64: mapValueOfType<int>(json, r'int64'),
        number: json[r'number'] == null
          ? null
          : num.parse(json[r'number'].toString()),
        float: mapValueOfType<double>(json, r'float'),
        double_: mapValueOfType<double>(json, r'double'),
        decimal: mapValueOfType<double>(json, r'decimal'),
        string: mapValueOfType<String>(json, r'string'),
        byte: mapValueOfType<String>(json, r'byte'),
        binary: null, // No support for decoding binary content from JSON
        date: mapDateTime(json, r'date', ''),
        dateTime: mapDateTime(json, r'dateTime', ''),
        uuid: mapValueOfType<String>(json, r'uuid'),
        password: mapValueOfType<String>(json, r'password'),
        patternWithDigits: mapValueOfType<String>(json, r'pattern_with_digits'),
        patternWithDigitsAndDelimiter: mapValueOfType<String>(json, r'pattern_with_digits_and_delimiter'),
      );
    }
    return null;
  }

  static List<FormatTest> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(FormatTest.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <FormatTest>[];

  static Map<String, FormatTest> mapFromJson(dynamic json) {
    final map = <String, FormatTest>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = FormatTest.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of FormatTest-objects as value to a dart map
  static Map<String, List<FormatTest>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<FormatTest>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = FormatTest.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

