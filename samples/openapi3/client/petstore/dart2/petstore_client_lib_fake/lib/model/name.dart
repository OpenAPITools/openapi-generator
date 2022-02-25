//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Name {
  /// Returns a new [Name] instance.
  Name({
    required this.name,
    this.snakeCase,
    this.property,
    this.n123number,
  });

  int name;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  int? snakeCase;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? property;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  int? n123number;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Name &&
     other.name == name &&
     other.snakeCase == snakeCase &&
     other.property == property &&
     other.n123number == n123number;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (name.hashCode) +
    (snakeCase == null ? 0 : snakeCase!.hashCode) +
    (property == null ? 0 : property!.hashCode) +
    (n123number == null ? 0 : n123number!.hashCode);

  @override
  String toString() => 'Name[name=$name, snakeCase=$snakeCase, property=$property, n123number=$n123number]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'name'] = name;
    if (snakeCase != null) {
      json[r'snake_case'] = snakeCase;
    }
    if (property != null) {
      json[r'property'] = property;
    }
    if (n123number != null) {
      json[r'123Number'] = n123number;
    }
    return json;
  }

  /// Returns a new [Name] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Name? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "Name[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "Name[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return Name(
        name: mapValueOfType<int>(json, r'name')!,
        snakeCase: mapValueOfType<int>(json, r'snake_case'),
        property: mapValueOfType<String>(json, r'property'),
        n123number: mapValueOfType<int>(json, r'123Number'),
      );
    }
    return null;
  }

  static List<Name>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <Name>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = Name.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, Name> mapFromJson(dynamic json) {
    final map = <String, Name>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = Name.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of Name-objects as value to a dart map
  static Map<String, List<Name>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<Name>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = Name.listFromJson(entry.value, growable: growable,);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
    'name',
  };
}

