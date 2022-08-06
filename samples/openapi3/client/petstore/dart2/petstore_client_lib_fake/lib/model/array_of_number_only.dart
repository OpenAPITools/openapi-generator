//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ArrayOfNumberOnly {
  /// Returns a new [ArrayOfNumberOnly] instance.
  ArrayOfNumberOnly({
    this.arrayNumber = const [],
  });

  List<num> arrayNumber;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ArrayOfNumberOnly &&
     other.arrayNumber == arrayNumber;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (arrayNumber.hashCode);

  @override
  String toString() => 'ArrayOfNumberOnly[arrayNumber=$arrayNumber]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'ArrayNumber'] = arrayNumber;
    return json;
  }

  /// Returns a new [ArrayOfNumberOnly] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ArrayOfNumberOnly? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "ArrayOfNumberOnly[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "ArrayOfNumberOnly[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return ArrayOfNumberOnly(
        arrayNumber: json[r'ArrayNumber'] is List
            ? (json[r'ArrayNumber'] as List).cast<num>()
            : const [],
      );
    }
    return null;
  }

  static List<ArrayOfNumberOnly>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ArrayOfNumberOnly>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ArrayOfNumberOnly.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, ArrayOfNumberOnly> mapFromJson(dynamic json) {
    final map = <String, ArrayOfNumberOnly>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ArrayOfNumberOnly.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of ArrayOfNumberOnly-objects as value to a dart map
  static Map<String, List<ArrayOfNumberOnly>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<ArrayOfNumberOnly>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ArrayOfNumberOnly.listFromJson(entry.value, growable: growable,);
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

