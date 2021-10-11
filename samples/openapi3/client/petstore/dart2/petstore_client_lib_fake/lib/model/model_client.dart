//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

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

  String client;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ModelClient &&
     other.client == client;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (client == null ? 0 : client.hashCode);

  @override
  String toString() => 'ModelClient[client=$client]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (client != null) {
      json[r'client'] = client;
    }
    return json;
  }

  /// Returns a new [ModelClient] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ModelClient fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return ModelClient(
        client: mapValueOfType<String>(json, r'client'),
      );
    }
    return null;
  }

  static List<ModelClient> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(ModelClient.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <ModelClient>[];

  static Map<String, ModelClient> mapFromJson(dynamic json) {
    final map = <String, ModelClient>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = ModelClient.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ModelClient-objects as value to a dart map
  static Map<String, List<ModelClient>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ModelClient>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = ModelClient.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

