//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

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
    (arrayOfString.hashCode) +
    (arrayArrayOfInteger.hashCode) +
    (arrayArrayOfModel.hashCode);

  @override
  String toString() => 'ArrayTest[arrayOfString=$arrayOfString, arrayArrayOfInteger=$arrayArrayOfInteger, arrayArrayOfModel=$arrayArrayOfModel]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'array_of_string'] = arrayOfString;
      json[r'array_array_of_integer'] = arrayArrayOfInteger;
      json[r'array_array_of_model'] = arrayArrayOfModel;
    return json;
  }

  /// Returns a new [ArrayTest] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ArrayTest? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "ArrayTest[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "ArrayTest[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return ArrayTest(
        arrayOfString: json[r'array_of_string'] is List
            ? (json[r'array_of_string'] as List).cast<String>()
            : const [],
        arrayArrayOfInteger: json[r'array_array_of_integer'] is List
          ? (json[r'array_array_of_integer'] as List).map(
              (e) => e == null ? null : (e as List).cast<int>()
            ).toList()
          : null,
        arrayArrayOfModel: json[r'array_array_of_model'] is List
          ? (json[r'array_array_of_model'] as List).map(
              ReadOnlyFirst.listFromJson(json[r'array_array_of_model'])
            ).toList()
          : null,
      );
    }
    return null;
  }

  static List<ArrayTest>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ArrayTest>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ArrayTest.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, ArrayTest> mapFromJson(dynamic json) {
    final map = <String, ArrayTest>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ArrayTest.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of ArrayTest-objects as value to a dart map
  static Map<String, List<ArrayTest>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<ArrayTest>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ArrayTest.listFromJson(entry.value, growable: growable,);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
}

