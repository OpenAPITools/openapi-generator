//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

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
    required this.number,
    this.float,
    this.double_,
    this.decimal,
    this.string,
    required this.byte,
    this.binary,
    required this.date,
    this.dateTime,
    this.uuid,
    required this.password,
    this.patternWithDigits,
    this.patternWithDigitsAndDelimiter,
  });

  /// Minimum value: 10
  /// Maximum value: 100
  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  int? integer;

  /// Minimum value: 20
  /// Maximum value: 200
  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  int? int32;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  int? int64;

  /// Minimum value: 32.1
  /// Maximum value: 543.2
  num number;

  /// Minimum value: 54.3
  /// Maximum value: 987.6
  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  double? float;

  /// Minimum value: 67.8
  /// Maximum value: 123.4
  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  double? double_;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  double? decimal;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? string;

  String byte;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  MultipartFile? binary;

  DateTime date;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  DateTime? dateTime;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? uuid;

  String password;

  /// A string that is a 10 digit number. Can have leading zeros.
  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? patternWithDigits;

  /// A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.
  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? patternWithDigitsAndDelimiter;

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
    (integer == null ? 0 : integer!.hashCode) +
    (int32 == null ? 0 : int32!.hashCode) +
    (int64 == null ? 0 : int64!.hashCode) +
    (number.hashCode) +
    (float == null ? 0 : float!.hashCode) +
    (double_ == null ? 0 : double_!.hashCode) +
    (decimal == null ? 0 : decimal!.hashCode) +
    (string == null ? 0 : string!.hashCode) +
    (byte.hashCode) +
    (binary == null ? 0 : binary!.hashCode) +
    (date.hashCode) +
    (dateTime == null ? 0 : dateTime!.hashCode) +
    (uuid == null ? 0 : uuid!.hashCode) +
    (password.hashCode) +
    (patternWithDigits == null ? 0 : patternWithDigits!.hashCode) +
    (patternWithDigitsAndDelimiter == null ? 0 : patternWithDigitsAndDelimiter!.hashCode);

  @override
  String toString() => 'FormatTest[integer=$integer, int32=$int32, int64=$int64, number=$number, float=$float, double_=$double_, decimal=$decimal, string=$string, byte=$byte, binary=$binary, date=$date, dateTime=$dateTime, uuid=$uuid, password=$password, patternWithDigits=$patternWithDigits, patternWithDigitsAndDelimiter=$patternWithDigitsAndDelimiter]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (this.integer != null) {
      json[r'integer'] = this.integer;
    } else {
      json[r'integer'] = null;
    }
    if (this.int32 != null) {
      json[r'int32'] = this.int32;
    } else {
      json[r'int32'] = null;
    }
    if (this.int64 != null) {
      json[r'int64'] = this.int64;
    } else {
      json[r'int64'] = null;
    }
      json[r'number'] = this.number;
    if (this.float != null) {
      json[r'float'] = this.float;
    } else {
      json[r'float'] = null;
    }
    if (this.double_ != null) {
      json[r'double'] = this.double_;
    } else {
      json[r'double'] = null;
    }
    if (this.decimal != null) {
      json[r'decimal'] = this.decimal;
    } else {
      json[r'decimal'] = null;
    }
    if (this.string != null) {
      json[r'string'] = this.string;
    } else {
      json[r'string'] = null;
    }
      json[r'byte'] = this.byte;
    if (this.binary != null) {
      json[r'binary'] = this.binary;
    } else {
      json[r'binary'] = null;
    }
      json[r'date'] = _dateFormatter.format(this.date.toUtc());
    if (this.dateTime != null) {
      json[r'dateTime'] = this.dateTime!.toUtc().toIso8601String();
    } else {
      json[r'dateTime'] = null;
    }
    if (this.uuid != null) {
      json[r'uuid'] = this.uuid;
    } else {
      json[r'uuid'] = null;
    }
      json[r'password'] = this.password;
    if (this.patternWithDigits != null) {
      json[r'pattern_with_digits'] = this.patternWithDigits;
    } else {
      json[r'pattern_with_digits'] = null;
    }
    if (this.patternWithDigitsAndDelimiter != null) {
      json[r'pattern_with_digits_and_delimiter'] = this.patternWithDigitsAndDelimiter;
    } else {
      json[r'pattern_with_digits_and_delimiter'] = null;
    }
    return json;
  }

  /// Returns a new [FormatTest] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static FormatTest? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "FormatTest[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "FormatTest[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return FormatTest(
        integer: mapValueOfType<int>(json, r'integer'),
        int32: mapValueOfType<int>(json, r'int32'),
        int64: mapValueOfType<int>(json, r'int64'),
        number: num.parse('${json[r'number']}'),
        float: mapValueOfType<double>(json, r'float'),
        double_: mapValueOfType<double>(json, r'double'),
        decimal: mapValueOfType<double>(json, r'decimal'),
        string: mapValueOfType<String>(json, r'string'),
        byte: mapValueOfType<String>(json, r'byte')!,
        binary: null, // No support for decoding binary content from JSON
        date: mapDateTime(json, r'date', r'')!,
        dateTime: mapDateTime(json, r'dateTime', r''),
        uuid: mapValueOfType<String>(json, r'uuid'),
        password: mapValueOfType<String>(json, r'password')!,
        patternWithDigits: mapValueOfType<String>(json, r'pattern_with_digits'),
        patternWithDigitsAndDelimiter: mapValueOfType<String>(json, r'pattern_with_digits_and_delimiter'),
      );
    }
    return null;
  }

  static List<FormatTest> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <FormatTest>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = FormatTest.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, FormatTest> mapFromJson(dynamic json) {
    final map = <String, FormatTest>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = FormatTest.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of FormatTest-objects as value to a dart map
  static Map<String, List<FormatTest>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<FormatTest>>{};
    if (json is Map && json.isNotEmpty) {
      // ignore: parameter_assignments
      json = json.cast<String, dynamic>();
      for (final entry in json.entries) {
        map[entry.key] = FormatTest.listFromJson(entry.value, growable: growable,);
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
    'number',
    'byte',
    'date',
    'password',
  };
}

