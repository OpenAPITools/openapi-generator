//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ReadOnlyFirst {
  /// Returns a new [ReadOnlyFirst] instance.
  ReadOnlyFirst({
    this.bar,
    this.baz,
  });

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? bar;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? baz;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ReadOnlyFirst &&
     other.bar == bar &&
     other.baz == baz;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (bar == null ? 0 : bar!.hashCode) +
    (baz == null ? 0 : baz!.hashCode);

  @override
  String toString() => 'ReadOnlyFirst[bar=$bar, baz=$baz]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (bar != null) {
      json[r'bar'] = bar;
    }
    if (baz != null) {
      json[r'baz'] = baz;
    }
    return json;
  }

  /// Returns a new [ReadOnlyFirst] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ReadOnlyFirst? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "ReadOnlyFirst[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "ReadOnlyFirst[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return ReadOnlyFirst(
        bar: mapValueOfType<String>(json, r'bar'),
        baz: mapValueOfType<String>(json, r'baz'),
      );
    }
    return null;
  }

  static List<ReadOnlyFirst>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ReadOnlyFirst>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ReadOnlyFirst.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, ReadOnlyFirst> mapFromJson(dynamic json) {
    final map = <String, ReadOnlyFirst>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ReadOnlyFirst.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of ReadOnlyFirst-objects as value to a dart map
  static Map<String, List<ReadOnlyFirst>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<ReadOnlyFirst>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ReadOnlyFirst.listFromJson(entry.value, growable: growable,);
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

