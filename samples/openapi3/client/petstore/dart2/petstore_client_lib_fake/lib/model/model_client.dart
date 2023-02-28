//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ModelClient {
  /// Returns a new [ModelClient] instance.
  ModelClient({
    this.client,
  });

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? client;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ModelClient &&
     other.client == client;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (client == null ? 0 : client!.hashCode);

  @override
  String toString() => 'ModelClient[client=$client]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (this.client != null) {
      json[r'client'] = this.client;
    } else {
      json[r'client'] = null;
    }
    return json;
  }

  /// Returns a new [ModelClient] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ModelClient? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "ModelClient[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "ModelClient[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return ModelClient(
        client: mapValueOfType<String>(json, r'client'),
      );
    }
    return null;
  }

  static List<ModelClient>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ModelClient>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ModelClient.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, ModelClient> mapFromJson(dynamic json) {
    final map = <String, ModelClient>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ModelClient.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of ModelClient-objects as value to a dart map
  static Map<String, List<ModelClient>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<ModelClient>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ModelClient.listFromJson(entry.value, growable: growable,);
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

