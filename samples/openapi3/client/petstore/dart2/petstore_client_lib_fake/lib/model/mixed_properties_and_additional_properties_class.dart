//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

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

  String uuid;

  DateTime dateTime;

  Map<String, Animal> map;

  @override
  bool operator ==(Object other) => identical(this, other) || other is MixedPropertiesAndAdditionalPropertiesClass &&
     other.uuid == uuid &&
     other.dateTime == dateTime &&
     other.map == map;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (uuid.hashCode) +
    (dateTime.hashCode) +
    (map.hashCode);

  @override
  String toString() => 'MixedPropertiesAndAdditionalPropertiesClass[uuid=$uuid, dateTime=$dateTime, map=$map]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'uuid'] = uuid;
      json[r'dateTime'] = dateTime.toUtc().toIso8601String();
      json[r'map'] = map;
    return json;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
    
  };

  /// Returns a new [MixedPropertiesAndAdditionalPropertiesClass] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static MixedPropertiesAndAdditionalPropertiesClass? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(
        false,
        () {
          for (final key in requiredKeys) {
            if (!json.containsKey(key)) {
              throw FormatException('Required key "MixedPropertiesAndAdditionalPropertiesClass.$key" is missing from JSON.', json);
            }
            final value = json[key];
            if (null == value) {
              throw FormatException('Required key "MixedPropertiesAndAdditionalPropertiesClass.$key" cannot be null.', json);
            }
          }
        },
      );

      return MixedPropertiesAndAdditionalPropertiesClass(
        uuid: mapValueOfType<String>(json, r'uuid'),
        dateTime: mapDateTime(json, r'dateTime', ''),
        map: mapValueOfType<Map<String, Animal>>(json, r'map') ?? const {},
      );
    }
    return null;
  }

  static List<MixedPropertiesAndAdditionalPropertiesClass>? listFromJson(dynamic json, {bool emptyIsNull = false, bool growable = false,}) {
    final result = <MixedPropertiesAndAdditionalPropertiesClass>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = MixedPropertiesAndAdditionalPropertiesClass.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return emptyIsNull ? null : result.toList(growable: growable);
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
  static Map<String, List<MixedPropertiesAndAdditionalPropertiesClass>> mapListFromJson(dynamic json, {bool emptyIsNull = false, bool growable = false,}) {
    final map = <String, List<MixedPropertiesAndAdditionalPropertiesClass>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = MixedPropertiesAndAdditionalPropertiesClass.listFromJson(entry.value, emptyIsNull: emptyIsNull, growable: growable,);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }
}

