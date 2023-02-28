//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class OuterObjectWithEnumProperty {
  /// Returns a new [OuterObjectWithEnumProperty] instance.
  OuterObjectWithEnumProperty({
    required this.value,
  });

  OuterEnumInteger value;

  @override
  bool operator ==(Object other) => identical(this, other) || other is OuterObjectWithEnumProperty &&
     other.value == value;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (value.hashCode);

  @override
  String toString() => 'OuterObjectWithEnumProperty[value=$value]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'value'] = this.value;
    return json;
  }

  /// Returns a new [OuterObjectWithEnumProperty] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static OuterObjectWithEnumProperty? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "OuterObjectWithEnumProperty[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "OuterObjectWithEnumProperty[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return OuterObjectWithEnumProperty(
        value: OuterEnumInteger.fromJson(json[r'value'])!,
      );
    }
    return null;
  }

  static List<OuterObjectWithEnumProperty>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <OuterObjectWithEnumProperty>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = OuterObjectWithEnumProperty.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, OuterObjectWithEnumProperty> mapFromJson(dynamic json) {
    final map = <String, OuterObjectWithEnumProperty>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = OuterObjectWithEnumProperty.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of OuterObjectWithEnumProperty-objects as value to a dart map
  static Map<String, List<OuterObjectWithEnumProperty>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<OuterObjectWithEnumProperty>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = OuterObjectWithEnumProperty.listFromJson(entry.value, growable: growable,);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
    'value',
  };
}

