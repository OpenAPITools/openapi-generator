//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.18

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ParentWithNullable {
  /// Returns a new [ParentWithNullable] instance.
  ParentWithNullable({
    this.type,
    this.nullableProperty,
  });

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? type;

  String? nullableProperty;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ParentWithNullable &&
    other.type == type &&
    other.nullableProperty == nullableProperty;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (type == null ? 0 : type!.hashCode) +
    (nullableProperty == null ? 0 : nullableProperty!.hashCode);

  @override
  String toString() => 'ParentWithNullable[type=$type, nullableProperty=$nullableProperty]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (this.type != null) {
      json[r'type'] = this.type;
    } else {
      json[r'type'] = null;
    }
    if (this.nullableProperty != null) {
      json[r'nullableProperty'] = this.nullableProperty;
    } else {
      json[r'nullableProperty'] = null;
    }
    return json;
  }

  /// Returns a new [ParentWithNullable] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ParentWithNullable? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "ParentWithNullable[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "ParentWithNullable[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return ParentWithNullable(
        type: mapValueOfType<String>(json, r'type'),
        nullableProperty: mapValueOfType<String>(json, r'nullableProperty'),
      );
    }
    return null;
  }

  static List<ParentWithNullable> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ParentWithNullable>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ParentWithNullable.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, ParentWithNullable> mapFromJson(dynamic json) {
    final map = <String, ParentWithNullable>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ParentWithNullable.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of ParentWithNullable-objects as value to a dart map
  static Map<String, List<ParentWithNullable>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<ParentWithNullable>>{};
    if (json is Map && json.isNotEmpty) {
      // ignore: parameter_assignments
      json = json.cast<String, dynamic>();
      for (final entry in json.entries) {
        map[entry.key] = ParentWithNullable.listFromJson(entry.value, growable: growable,);
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
}

