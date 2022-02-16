//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Cat {
  /// Returns a new [Cat] instance.
  Cat({
    required this.className,
    this.color = 'red',
    this.declawed,
  });

  String className;

  String color;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  bool? declawed;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Cat &&
     other.className == className &&
     other.color == color &&
     other.declawed == declawed;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (className.hashCode) +
    (color.hashCode) +
    (declawed == null ? 0 : declawed!.hashCode);

  @override
  String toString() => 'Cat[className=$className, color=$color, declawed=$declawed]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'className'] = className;
      json[r'color'] = color;
    if (declawed != null) {
      json[r'declawed'] = declawed;
    }
    return json;
  }

  /// Returns a new [Cat] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Cat? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "Cat[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "Cat[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return Cat(
        className: mapValueOfType<String>(json, r'className')!,
        color: mapValueOfType<String>(json, r'color') ?? 'red',
        declawed: mapValueOfType<bool>(json, r'declawed'),
      );
    }
    return null;
  }

  static List<Cat>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <Cat>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = Cat.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, Cat> mapFromJson(dynamic json) {
    final map = <String, Cat>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = Cat.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of Cat-objects as value to a dart map
  static Map<String, List<Cat>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<Cat>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = Cat.listFromJson(entry.value, growable: growable,);
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

