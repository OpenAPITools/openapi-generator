//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

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
    (mapProperty.hashCode) +
    (mapOfMapProperty.hashCode);

  @override
  String toString() => 'AdditionalPropertiesClass[mapProperty=$mapProperty, mapOfMapProperty=$mapOfMapProperty]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'map_property'] = mapProperty;
      json[r'map_of_map_property'] = mapOfMapProperty;
    return json;
  }

  /// Returns a new [AdditionalPropertiesClass] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static AdditionalPropertiesClass? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "AdditionalPropertiesClass[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "AdditionalPropertiesClass[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return AdditionalPropertiesClass(
        mapProperty: mapCastOfType<String, String>(json, r'map_property') ?? const {},
        mapOfMapProperty: mapCastOfType<String, dynamic>(json, r'map_of_map_property') ?? const {},
      );
    }
    return null;
  }

  static List<AdditionalPropertiesClass>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <AdditionalPropertiesClass>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = AdditionalPropertiesClass.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, AdditionalPropertiesClass> mapFromJson(dynamic json) {
    final map = <String, AdditionalPropertiesClass>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = AdditionalPropertiesClass.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of AdditionalPropertiesClass-objects as value to a dart map
  static Map<String, List<AdditionalPropertiesClass>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<AdditionalPropertiesClass>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = AdditionalPropertiesClass.listFromJson(entry.value, growable: growable,);
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

