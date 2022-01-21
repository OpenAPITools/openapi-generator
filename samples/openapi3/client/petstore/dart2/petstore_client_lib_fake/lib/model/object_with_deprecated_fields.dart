//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ObjectWithDeprecatedFields {
  /// Returns a new [ObjectWithDeprecatedFields] instance.
  ObjectWithDeprecatedFields({
    this.uuid,
    this.id,
    this.deprecatedRef,
    this.bars = const [],
  });

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? uuid;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  num? id;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  DeprecatedObject? deprecatedRef;

  List<String> bars;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ObjectWithDeprecatedFields &&
     other.uuid == uuid &&
     other.id == id &&
     other.deprecatedRef == deprecatedRef &&
     other.bars == bars;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (uuid == null ? 0 : uuid!.hashCode) +
    (id == null ? 0 : id!.hashCode) +
    (deprecatedRef == null ? 0 : deprecatedRef!.hashCode) +
    (bars.hashCode);

  @override
  String toString() => 'ObjectWithDeprecatedFields[uuid=$uuid, id=$id, deprecatedRef=$deprecatedRef, bars=$bars]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (uuid != null) {
      json[r'uuid'] = uuid;
    }
    if (id != null) {
      json[r'id'] = id;
    }
    if (deprecatedRef != null) {
      json[r'deprecatedRef'] = deprecatedRef;
    }
      json[r'bars'] = bars;
    return json;
  }

  /// Returns a new [ObjectWithDeprecatedFields] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ObjectWithDeprecatedFields? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "ObjectWithDeprecatedFields[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "ObjectWithDeprecatedFields[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return ObjectWithDeprecatedFields(
        uuid: mapValueOfType<String>(json, r'uuid'),
        id: json[r'id'] == null
            ? null
            : num.parse(json[r'id'].toString()),
        deprecatedRef: DeprecatedObject.fromJson(json[r'deprecatedRef']),
        bars: json[r'bars'] is List
            ? (json[r'bars'] as List).cast<String>()
            : const [],
      );
    }
    return null;
  }

  static List<ObjectWithDeprecatedFields>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ObjectWithDeprecatedFields>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ObjectWithDeprecatedFields.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, ObjectWithDeprecatedFields> mapFromJson(dynamic json) {
    final map = <String, ObjectWithDeprecatedFields>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ObjectWithDeprecatedFields.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of ObjectWithDeprecatedFields-objects as value to a dart map
  static Map<String, List<ObjectWithDeprecatedFields>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<ObjectWithDeprecatedFields>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ObjectWithDeprecatedFields.listFromJson(entry.value, growable: growable,);
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

