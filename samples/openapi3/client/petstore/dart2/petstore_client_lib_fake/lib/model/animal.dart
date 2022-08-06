//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Animal {
  /// Returns a new [Animal] instance.
  Animal({
    required this.className,
    this.color = 'red',
  });

  String className;

  String color;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Animal &&
     other.className == className &&
     other.color == color;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (className.hashCode) +
    (color.hashCode);

  @override
  String toString() => 'Animal[className=$className, color=$color]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'className'] = className;
      json[r'color'] = color;
    return json;
  }

  /// Returns a new [Animal] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Animal? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "Animal[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "Animal[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return Animal(
        className: mapValueOfType<String>(json, r'className')!,
        color: mapValueOfType<String>(json, r'color') ?? 'red',
      );
    }
    return null;
  }

  static List<Animal>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <Animal>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = Animal.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, Animal> mapFromJson(dynamic json) {
    final map = <String, Animal>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = Animal.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of Animal-objects as value to a dart map
  static Map<String, List<Animal>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<Animal>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = Animal.listFromJson(entry.value, growable: growable,);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
    'className',
  };
}

