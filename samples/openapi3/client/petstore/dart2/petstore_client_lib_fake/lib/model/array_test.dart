//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
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
  // ignore: unnecessary_parenthesis
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
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ArrayTest fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return ArrayTest(
        arrayOfString: json[r'array_of_string'] is List
          ? (json[r'array_of_string'] as List).cast<String>()
          : null,
        arrayArrayOfInteger: json[r'array_array_of_integer'] is List
          ? (json[r'array_array_of_integer'] as List).map(
              (e) => e == null ? null : (e as List).cast<int>()
            ).toList(growable: false)
          : null,
        arrayArrayOfModel: json[r'array_array_of_model'] is List
          ? (json[r'array_array_of_model'] as List).map(
              ReadOnlyFirst.listFromJson(json[r'array_array_of_model'])
            ).toList(growable: false)
          : null,
      );
    }
    return null;
  }

  static List<ArrayTest> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(ArrayTest.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <ArrayTest>[];

  static Map<String, ArrayTest> mapFromJson(dynamic json) {
    final map = <String, ArrayTest>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = ArrayTest.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ArrayTest-objects as value to a dart map
  static Map<String, List<ArrayTest>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ArrayTest>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = ArrayTest.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

