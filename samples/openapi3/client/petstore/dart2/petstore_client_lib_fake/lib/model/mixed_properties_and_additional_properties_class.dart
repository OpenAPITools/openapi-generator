//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

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


  String? uuid;

  DateTime? dateTime;

  Map<String, Animal>? map;

  @override
  bool operator ==(Object other) => identical(this, other) || other is MixedPropertiesAndAdditionalPropertiesClass &&
     other.uuid == uuid &&
     other.dateTime == dateTime &&
     other.map == map;

  @override
  int get hashCode =>
    uuid.hashCode +
    dateTime.hashCode +
    map.hashCode;

  @override
  String toString() => 'MixedPropertiesAndAdditionalPropertiesClass[uuid=$uuid, dateTime=$dateTime, map=$map]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (uuid != null) {
      json[r'uuid'] = uuid;
    }
    if (dateTime != null) {
      json[r'dateTime'] = dateTime!.toUtc().toIso8601String();
    }
    if (map != null) {
      json[r'map'] = map;
    }
    return json;
  }

  /// Returns a new [MixedPropertiesAndAdditionalPropertiesClass] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static MixedPropertiesAndAdditionalPropertiesClass fromJson(Map<String, dynamic> json) => MixedPropertiesAndAdditionalPropertiesClass(
        uuid: json[r'uuid'] as String,
        dateTime: mapDateTime(json, r'dateTime', ''),
        map: json[r'map'] as Map<String, Animal>,
    );

  static List<MixedPropertiesAndAdditionalPropertiesClass> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<MixedPropertiesAndAdditionalPropertiesClass>((i) => MixedPropertiesAndAdditionalPropertiesClass.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <MixedPropertiesAndAdditionalPropertiesClass>[];

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
  static Map<String, List<MixedPropertiesAndAdditionalPropertiesClass>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<MixedPropertiesAndAdditionalPropertiesClass>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = MixedPropertiesAndAdditionalPropertiesClass.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

