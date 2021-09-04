//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

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
    (uuid == null ? 0 : uuid.hashCode) +
    (dateTime == null ? 0 : dateTime.hashCode) +
    (map == null ? 0 : map.hashCode);

  @override
  String toString() => 'MixedPropertiesAndAdditionalPropertiesClass[uuid=$uuid, dateTime=$dateTime, map=$map]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (uuid != null) {
      json[r'uuid'] = uuid;
    }
    if (dateTime != null) {
      json[r'dateTime'] = dateTime.toUtc().toIso8601String();
    }
    if (map != null) {
      json[r'map'] = map;
    }
    return json;
  }

  /// Returns a new [MixedPropertiesAndAdditionalPropertiesClass] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static MixedPropertiesAndAdditionalPropertiesClass fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return MixedPropertiesAndAdditionalPropertiesClass(
        uuid: mapValueOfType<String>(json, r'uuid'),
        dateTime: mapDateTime(json, r'dateTime', ''),
        map: mapValueOfType<Map<String, Animal>>(json, r'map'),
      );
    }
    return null;
  }

  static List<MixedPropertiesAndAdditionalPropertiesClass> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(MixedPropertiesAndAdditionalPropertiesClass.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <MixedPropertiesAndAdditionalPropertiesClass>[];

  static Map<String, MixedPropertiesAndAdditionalPropertiesClass> mapFromJson(dynamic json) {
    final map = <String, MixedPropertiesAndAdditionalPropertiesClass>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = MixedPropertiesAndAdditionalPropertiesClass.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of MixedPropertiesAndAdditionalPropertiesClass-objects as value to a dart map
  static Map<String, List<MixedPropertiesAndAdditionalPropertiesClass>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<MixedPropertiesAndAdditionalPropertiesClass>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = MixedPropertiesAndAdditionalPropertiesClass.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

