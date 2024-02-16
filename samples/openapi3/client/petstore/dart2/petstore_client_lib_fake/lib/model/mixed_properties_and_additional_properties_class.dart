//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class MixedPropertiesAndAdditionalPropertiesClass {
  /// Returns a new [MixedPropertiesAndAdditionalPropertiesClass] instance.
  MixedPropertiesAndAdditionalPropertiesClass({
    this.uuid,
    this.dateTime,
    this.map = const {},
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
  DateTime? dateTime;

  Map<String, Animal> map;

  @override
  bool operator ==(Object other) => identical(this, other) || other is MixedPropertiesAndAdditionalPropertiesClass &&
    other.uuid == uuid &&
    other.dateTime == dateTime &&
    _deepEquality.equals(other.map, map);

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (uuid == null ? 0 : uuid!.hashCode) +
    (dateTime == null ? 0 : dateTime!.hashCode) +
    (map.hashCode);

  @override
  String toString() => 'MixedPropertiesAndAdditionalPropertiesClass[uuid=$uuid, dateTime=$dateTime, map=$map]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (this.uuid != null) {
      json[r'uuid'] = this.uuid;
    } else {
      json[r'uuid'] = null;
    }
    if (this.dateTime != null) {
      json[r'dateTime'] = this.dateTime!.toUtc().toIso8601String();
    } else {
      json[r'dateTime'] = null;
    }
      json[r'map'] = this.map;
    return json;
  }

  /// Returns a new [MixedPropertiesAndAdditionalPropertiesClass] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static MixedPropertiesAndAdditionalPropertiesClass? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "MixedPropertiesAndAdditionalPropertiesClass[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "MixedPropertiesAndAdditionalPropertiesClass[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return MixedPropertiesAndAdditionalPropertiesClass(
        uuid: mapValueOfType<String>(json, r'uuid'),
        dateTime: mapDateTime(json, r'dateTime', r''),
        map: Animal.mapFromJson(json[r'map']),
      );
    }
    return null;
  }

  static List<MixedPropertiesAndAdditionalPropertiesClass> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <MixedPropertiesAndAdditionalPropertiesClass>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = MixedPropertiesAndAdditionalPropertiesClass.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, MixedPropertiesAndAdditionalPropertiesClass> mapFromJson(dynamic json) {
    final map = <String, MixedPropertiesAndAdditionalPropertiesClass>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = MixedPropertiesAndAdditionalPropertiesClass.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of MixedPropertiesAndAdditionalPropertiesClass-objects as value to a dart map
  static Map<String, List<MixedPropertiesAndAdditionalPropertiesClass>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<MixedPropertiesAndAdditionalPropertiesClass>>{};
    if (json is Map && json.isNotEmpty) {
      // ignore: parameter_assignments
      json = json.cast<String, dynamic>();
      for (final entry in json.entries) {
        map[entry.key] = MixedPropertiesAndAdditionalPropertiesClass.listFromJson(entry.value, growable: growable,);
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
}

