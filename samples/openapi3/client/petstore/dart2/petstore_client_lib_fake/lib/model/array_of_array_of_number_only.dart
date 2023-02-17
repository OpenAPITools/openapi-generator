//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ArrayOfArrayOfNumberOnly {
  /// Returns a new [ArrayOfArrayOfNumberOnly] instance.
  ArrayOfArrayOfNumberOnly({
    this.arrayArrayNumber = const [],
  });

  List<List<num>> arrayArrayNumber;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ArrayOfArrayOfNumberOnly &&
     other.arrayArrayNumber == arrayArrayNumber;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (arrayArrayNumber.hashCode);

  @override
  String toString() => 'ArrayOfArrayOfNumberOnly[arrayArrayNumber=$arrayArrayNumber]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'ArrayArrayNumber'] = this.arrayArrayNumber;
    return json;
  }

  /// Returns a new [ArrayOfArrayOfNumberOnly] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ArrayOfArrayOfNumberOnly? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "ArrayOfArrayOfNumberOnly[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "ArrayOfArrayOfNumberOnly[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return ArrayOfArrayOfNumberOnly(
        arrayArrayNumber: json[r'ArrayArrayNumber'] is List
          ? (json[r'ArrayArrayNumber'] as List).map((e) =>
              e == null ? const  <num>[] : (e as List).cast<num>()
            ).toList()
          :  const [],
      );
    }
    return null;
  }

  static List<ArrayOfArrayOfNumberOnly>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ArrayOfArrayOfNumberOnly>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ArrayOfArrayOfNumberOnly.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, ArrayOfArrayOfNumberOnly> mapFromJson(dynamic json) {
    final map = <String, ArrayOfArrayOfNumberOnly>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ArrayOfArrayOfNumberOnly.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of ArrayOfArrayOfNumberOnly-objects as value to a dart map
  static Map<String, List<ArrayOfArrayOfNumberOnly>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<ArrayOfArrayOfNumberOnly>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ArrayOfArrayOfNumberOnly.listFromJson(entry.value, growable: growable,);
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

