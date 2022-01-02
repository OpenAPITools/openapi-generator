//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
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
  // ignore: unnecessary_parenthesis
    (mapProperty == null ? 0 : mapProperty.hashCode) +
    (mapOfMapProperty == null ? 0 : mapOfMapProperty.hashCode);

  @override
  String toString() => 'AdditionalPropertiesClass[mapProperty=$mapProperty, mapOfMapProperty=$mapOfMapProperty]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (mapProperty != null) {
      json[r'map_property'] = mapProperty;
    }
    if (mapOfMapProperty != null) {
      json[r'map_of_map_property'] = mapOfMapProperty;
    }
    return json;
  }

  /// Returns a new [AdditionalPropertiesClass] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static AdditionalPropertiesClass fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return AdditionalPropertiesClass(
        mapProperty: mapCastOfType<String, String>(json, r'map_property'),
        mapOfMapProperty: mapCastOfType<String, dynamic>(json, r'map_of_map_property'),
      );
    }
    return null;
  }

  static List<AdditionalPropertiesClass> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(AdditionalPropertiesClass.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <AdditionalPropertiesClass>[];

  static Map<String, AdditionalPropertiesClass> mapFromJson(dynamic json) {
    final map = <String, AdditionalPropertiesClass>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = AdditionalPropertiesClass.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of AdditionalPropertiesClass-objects as value to a dart map
  static Map<String, List<AdditionalPropertiesClass>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<AdditionalPropertiesClass>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = AdditionalPropertiesClass.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

