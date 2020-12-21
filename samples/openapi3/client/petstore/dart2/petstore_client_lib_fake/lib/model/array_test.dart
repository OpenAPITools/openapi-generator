//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ArrayTest {
  /// Returns a new [ArrayTest] instance.
  ArrayTest({
    this.arrayOfString = const [],
    this.arrayArrayOfInteger = const [],
    this.arrayArrayOfModel = const [],
  });

  List<String> arrayOfString;

  List<List<int>> arrayArrayOfInteger;

  List<List<ReadOnlyFirst>> arrayArrayOfModel;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ArrayTest &&
     other.arrayOfString == arrayOfString &&
     other.arrayArrayOfInteger == arrayArrayOfInteger &&
     other.arrayArrayOfModel == arrayArrayOfModel;

  @override
  int get hashCode =>
    (arrayOfString == null ? 0 : arrayOfString.hashCode) +
    (arrayArrayOfInteger == null ? 0 : arrayArrayOfInteger.hashCode) +
    (arrayArrayOfModel == null ? 0 : arrayArrayOfModel.hashCode);

  @override
  String toString() => 'ArrayTest[arrayOfString=$arrayOfString, arrayArrayOfInteger=$arrayArrayOfInteger, arrayArrayOfModel=$arrayArrayOfModel]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (arrayOfString != null) {
      json[r'array_of_string'] = arrayOfString;
    }
    if (arrayArrayOfInteger != null) {
      json[r'array_array_of_integer'] = arrayArrayOfInteger;
    }
    if (arrayArrayOfModel != null) {
      json[r'array_array_of_model'] = arrayArrayOfModel;
    }
    return json;
  }

  /// Returns a new [ArrayTest] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static ArrayTest fromJson(Map<String, dynamic> json) => json == null
    ? null
    : ArrayTest(
        arrayOfString: json[r'array_of_string'] == null
          ? null
          : (json[r'array_of_string'] as List).cast<String>(),
        arrayArrayOfInteger: json[r'array_array_of_integer'] == null
          ? null
          : (json[r'array_array_of_integer'] as List).map(
              (e) => e == null ? null : (e as List).cast<int>()
            ).toList(growable: false),
        arrayArrayOfModel: json[r'array_array_of_model'] == null
          ? null
          : (json[r'array_array_of_model'] as List).map(
              ReadOnlyFirst.listFromJson(json[r'array_array_of_model'])
            ).toList(growable: false),
    );

  static List<ArrayTest> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <ArrayTest>[]
      : json.map((dynamic value) => ArrayTest.fromJson(value)).toList(growable: true == growable);

  static Map<String, ArrayTest> mapFromJson(Map<String, dynamic> json) {
    final map = <String, ArrayTest>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = ArrayTest.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ArrayTest-objects as value to a dart map
  static Map<String, List<ArrayTest>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ArrayTest>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = ArrayTest.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

