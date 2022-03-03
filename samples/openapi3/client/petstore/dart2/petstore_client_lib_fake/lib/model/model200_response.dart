//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Model200Response {
  /// Returns a new [Model200Response] instance.
  Model200Response({
    this.name,
    this.class_,
  });

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  int? name;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? class_;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Model200Response &&
     other.name == name &&
     other.class_ == class_;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (name == null ? 0 : name!.hashCode) +
    (class_ == null ? 0 : class_!.hashCode);

  @override
  String toString() => 'Model200Response[name=$name, class_=$class_]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (name != null) {
      json[r'name'] = name;
    }
    if (class_ != null) {
      json[r'class'] = class_;
    }
    return json;
  }

  /// Returns a new [Model200Response] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Model200Response? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "Model200Response[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "Model200Response[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return Model200Response(
        name: mapValueOfType<int>(json, r'name'),
        class_: mapValueOfType<String>(json, r'class'),
      );
    }
    return null;
  }

  static List<Model200Response>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <Model200Response>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = Model200Response.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, Model200Response> mapFromJson(dynamic json) {
    final map = <String, Model200Response>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = Model200Response.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of Model200Response-objects as value to a dart map
  static Map<String, List<Model200Response>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<Model200Response>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = Model200Response.listFromJson(entry.value, growable: growable,);
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

