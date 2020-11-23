//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class AdditionalPropertiesClass {
  /// Returns a new [AdditionalPropertiesClass] instance.
  AdditionalPropertiesClass({
    this.mapProperty = const {},
    this.mapOfMapProperty = const {},
  });

  
  Map<String, String> mapProperty;

  
  Map<String, Map<String, String>> mapOfMapProperty;

  @override
  bool operator ==(Object other) => identical(this, other) || other is AdditionalPropertiesClass &&
     other.mapProperty == mapProperty &&
     other.mapOfMapProperty == mapOfMapProperty;

  @override
  int get hashCode =>
    (mapProperty == null ? 0 : mapProperty.hashCode) +
    (mapOfMapProperty == null ? 0 : mapOfMapProperty.hashCode);

  @override
  String toString() => 'AdditionalPropertiesClass[mapProperty=$mapProperty, mapOfMapProperty=$mapOfMapProperty]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (mapProperty != null) {
      json['map_property'] = mapProperty;
    }
    if (mapOfMapProperty != null) {
      json['map_of_map_property'] = mapOfMapProperty;
    }
    return json;
  }

  /// Returns a new [AdditionalPropertiesClass] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static AdditionalPropertiesClass fromJson(Map<String, dynamic> json) => json == null
    ? null
    : AdditionalPropertiesClass(
        mapProperty: json['map_property'] == null ?
          null :
          (json['map_property'] as Map).cast<String, String>(),
        mapOfMapProperty: json['map_of_map_property'] == null
          ? null
          : Map.mapFromJson(json['map_of_map_property']),
    );

  static List<AdditionalPropertiesClass> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <AdditionalPropertiesClass>[]
      : json.map((v) => AdditionalPropertiesClass.fromJson(v)).toList(growable: true == growable);

  static Map<String, AdditionalPropertiesClass> mapFromJson(Map<String, dynamic> json) {
    final map = <String, AdditionalPropertiesClass>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = AdditionalPropertiesClass.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of AdditionalPropertiesClass-objects as value to a dart map
  static Map<String, List<AdditionalPropertiesClass>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<AdditionalPropertiesClass>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = AdditionalPropertiesClass.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

