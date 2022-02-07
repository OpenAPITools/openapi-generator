//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class CatAllOf {
  /// Returns a new [CatAllOf] instance.
  CatAllOf({
    this.declawed,
  });

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  bool? declawed;

  @override
  bool operator ==(Object other) => identical(this, other) || other is CatAllOf &&
     other.declawed == declawed;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (declawed == null ? 0 : declawed!.hashCode);

  @override
  String toString() => 'CatAllOf[declawed=$declawed]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (declawed != null) {
      json[r'declawed'] = declawed;
    }
    return json;
  }

  /// Returns a new [CatAllOf] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static CatAllOf? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "CatAllOf[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "CatAllOf[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return CatAllOf(
        declawed: mapValueOfType<bool>(json, r'declawed'),
      );
    }
    return null;
  }

  static List<CatAllOf>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <CatAllOf>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = CatAllOf.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, CatAllOf> mapFromJson(dynamic json) {
    final map = <String, CatAllOf>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = CatAllOf.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of CatAllOf-objects as value to a dart map
  static Map<String, List<CatAllOf>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<CatAllOf>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = CatAllOf.listFromJson(entry.value, growable: growable,);
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

