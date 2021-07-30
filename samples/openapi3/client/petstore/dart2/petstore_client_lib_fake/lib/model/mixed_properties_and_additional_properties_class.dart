//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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
  /// [json] if it's non-null, null if [json] is null.
  static MixedPropertiesAndAdditionalPropertiesClass fromJson(Map<String, dynamic> json) => json == null
    ? null
    : MixedPropertiesAndAdditionalPropertiesClass(
        uuid: json[r'uuid'],
        dateTime: json[r'dateTime'] == null
          ? null
          : DateTime.parse(json[r'dateTime']),
            map: json[r'map']
    );

  static List<MixedPropertiesAndAdditionalPropertiesClass> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <MixedPropertiesAndAdditionalPropertiesClass>[]
      : json.map((dynamic value) => MixedPropertiesAndAdditionalPropertiesClass.fromJson(value)).toList(growable: true == growable);

  static Map<String, MixedPropertiesAndAdditionalPropertiesClass> mapFromJson(Map<String, dynamic> json) {
    final map = <String, MixedPropertiesAndAdditionalPropertiesClass>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = MixedPropertiesAndAdditionalPropertiesClass.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of MixedPropertiesAndAdditionalPropertiesClass-objects as value to a dart map
  static Map<String, List<MixedPropertiesAndAdditionalPropertiesClass>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<MixedPropertiesAndAdditionalPropertiesClass>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = MixedPropertiesAndAdditionalPropertiesClass.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

