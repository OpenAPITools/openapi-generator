//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class InlineObject3 {
  /// Returns a new [InlineObject3] instance.
  InlineObject3({
    this.integer,
    this.int32,
    this.int64,
    @required this.number,
    this.float,
    @required this.double_,
    this.string,
    @required this.patternWithoutDelimiter,
    @required this.byte,
    this.binary,
    this.date,
    this.dateTime,
    this.password,
    this.callback,
  });

  /// None
  // minimum: 10
  // maximum: 100
  int integer;

  /// None
  // minimum: 20
  // maximum: 200
  int int32;

  /// None
  int int64;

  /// None
  // minimum: 32.1
  // maximum: 543.2
  num number;

  /// None
  // maximum: 987.6
  double float;

  /// None
  // minimum: 67.8
  // maximum: 123.4
  double double_;

  /// None
  String string;

  /// None
  String patternWithoutDelimiter;

  /// None
  String byte;

  /// None
  MultipartFile binary;

  /// None
  DateTime date;

  /// None
  DateTime dateTime;

  /// None
  String password;

  /// None
  String callback;

  @override
  bool operator ==(Object other) => identical(this, other) || other is InlineObject3 &&
     other.integer == integer &&
     other.int32 == int32 &&
     other.int64 == int64 &&
     other.number == number &&
     other.float == float &&
     other.double_ == double_ &&
     other.string == string &&
     other.patternWithoutDelimiter == patternWithoutDelimiter &&
     other.byte == byte &&
     other.binary == binary &&
     other.date == date &&
     other.dateTime == dateTime &&
     other.password == password &&
     other.callback == callback;

  @override
  int get hashCode =>
    (integer == null ? 0 : integer.hashCode) +
    (int32 == null ? 0 : int32.hashCode) +
    (int64 == null ? 0 : int64.hashCode) +
    (number == null ? 0 : number.hashCode) +
    (float == null ? 0 : float.hashCode) +
    (double_ == null ? 0 : double_.hashCode) +
    (string == null ? 0 : string.hashCode) +
    (patternWithoutDelimiter == null ? 0 : patternWithoutDelimiter.hashCode) +
    (byte == null ? 0 : byte.hashCode) +
    (binary == null ? 0 : binary.hashCode) +
    (date == null ? 0 : date.hashCode) +
    (dateTime == null ? 0 : dateTime.hashCode) +
    (password == null ? 0 : password.hashCode) +
    (callback == null ? 0 : callback.hashCode);

  @override
  String toString() => 'InlineObject3[integer=$integer, int32=$int32, int64=$int64, number=$number, float=$float, double_=$double_, string=$string, patternWithoutDelimiter=$patternWithoutDelimiter, byte=$byte, binary=$binary, date=$date, dateTime=$dateTime, password=$password, callback=$callback]';

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
    if (number != null) {
      json[r'number'] = number;
    }
    if (float != null) {
      json[r'float'] = float;
    }
    if (double_ != null) {
      json[r'double'] = double_;
    }
    if (string != null) {
      json[r'string'] = string;
    }
    if (patternWithoutDelimiter != null) {
      json[r'pattern_without_delimiter'] = patternWithoutDelimiter;
    }
    if (byte != null) {
      json[r'byte'] = byte;
    }
    if (binary != null) {
      json[r'binary'] = binary;
    }
    if (date != null) {
      json[r'date'] = _dateFormatter.format(date.toUtc());
    }
    if (dateTime != null) {
      json[r'dateTime'] = dateTime.toUtc().toIso8601String();
    }
    if (password != null) {
      json[r'password'] = password;
    }
    if (callback != null) {
      json[r'callback'] = callback;
    }
    return json;
  }

  /// Returns a new [InlineObject3] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static InlineObject3 fromJson(Map<String, dynamic> json) => json == null
    ? null
    : InlineObject3(
        integer: json[r'integer'],
        int32: json[r'int32'],
        int64: json[r'int64'],
        number: json[r'number'] == null ?
          null :
          json[r'number'].toDouble(),
        float: json[r'float'],
        double_: json[r'double'],
        string: json[r'string'],
        patternWithoutDelimiter: json[r'pattern_without_delimiter'],
        byte: json[r'byte'],
        binary: null, // No support for decoding binary content from JSON
        date: json[r'date'] == null
          ? null
          : DateTime.parse(json[r'date']),
        dateTime: json[r'dateTime'] == null
          ? null
          : DateTime.parse(json[r'dateTime']),
        password: json[r'password'],
        callback: json[r'callback'],
    );

  static List<InlineObject3> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <InlineObject3>[]
      : json.map((v) => InlineObject3.fromJson(v)).toList(growable: true == growable);

  static Map<String, InlineObject3> mapFromJson(Map<String, dynamic> json) {
    final map = <String, InlineObject3>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = InlineObject3.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of InlineObject3-objects as value to a dart map
  static Map<String, List<InlineObject3>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<InlineObject3>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = InlineObject3.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

