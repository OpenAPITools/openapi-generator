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


  String? client;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ModelClient &&
     other.client == client;

  @override
  int get hashCode =>
    client.hashCode;

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
  static ModelClient fromJson(Map<String, dynamic> json) => ModelClient(
        client: json[r'client'] as String,
    );

  static List<ModelClient> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<ModelClient>((i) => ModelClient.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <ModelClient>[];

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
  static Map<String, List<ModelClient>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<ModelClient>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = ModelClient.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

