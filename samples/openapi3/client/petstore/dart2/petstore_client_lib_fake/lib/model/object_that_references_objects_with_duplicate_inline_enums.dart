//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.18

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ObjectThatReferencesObjectsWithDuplicateInlineEnums {
  /// Returns a new [ObjectThatReferencesObjectsWithDuplicateInlineEnums] instance.
  ObjectThatReferencesObjectsWithDuplicateInlineEnums({
    this.objectOne,
    this.objectTwo,
  });

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  ObjectWithInlineEnum? objectOne;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  ObjectWithDuplicateInlineEnum? objectTwo;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ObjectThatReferencesObjectsWithDuplicateInlineEnums &&
    other.objectOne == objectOne &&
    other.objectTwo == objectTwo;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (objectOne == null ? 0 : objectOne!.hashCode) +
    (objectTwo == null ? 0 : objectTwo!.hashCode);

  @override
  String toString() => 'ObjectThatReferencesObjectsWithDuplicateInlineEnums[objectOne=$objectOne, objectTwo=$objectTwo]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (this.objectOne != null) {
      json[r'object_one'] = this.objectOne;
    } else {
      json[r'object_one'] = null;
    }
    if (this.objectTwo != null) {
      json[r'object_two'] = this.objectTwo;
    } else {
      json[r'object_two'] = null;
    }
    return json;
  }

  /// Returns a new [ObjectThatReferencesObjectsWithDuplicateInlineEnums] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ObjectThatReferencesObjectsWithDuplicateInlineEnums? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "ObjectThatReferencesObjectsWithDuplicateInlineEnums[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "ObjectThatReferencesObjectsWithDuplicateInlineEnums[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return ObjectThatReferencesObjectsWithDuplicateInlineEnums(
        objectOne: ObjectWithInlineEnum.fromJson(json[r'object_one']),
        objectTwo: ObjectWithDuplicateInlineEnum.fromJson(json[r'object_two']),
      );
    }
    return null;
  }

  static List<ObjectThatReferencesObjectsWithDuplicateInlineEnums> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ObjectThatReferencesObjectsWithDuplicateInlineEnums>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ObjectThatReferencesObjectsWithDuplicateInlineEnums.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, ObjectThatReferencesObjectsWithDuplicateInlineEnums> mapFromJson(dynamic json) {
    final map = <String, ObjectThatReferencesObjectsWithDuplicateInlineEnums>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ObjectThatReferencesObjectsWithDuplicateInlineEnums.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of ObjectThatReferencesObjectsWithDuplicateInlineEnums-objects as value to a dart map
  static Map<String, List<ObjectThatReferencesObjectsWithDuplicateInlineEnums>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<ObjectThatReferencesObjectsWithDuplicateInlineEnums>>{};
    if (json is Map && json.isNotEmpty) {
      // ignore: parameter_assignments
      json = json.cast<String, dynamic>();
      for (final entry in json.entries) {
        map[entry.key] = ObjectThatReferencesObjectsWithDuplicateInlineEnums.listFromJson(entry.value, growable: growable,);
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
}

