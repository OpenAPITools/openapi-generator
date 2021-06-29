//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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
  /// [json] if it's non-null, null if [json] is null.
  static ModelClient fromJson(Map<String, dynamic> json) => json == null
    ? null
    : ModelClient(
        client: json[r'client'],
    );

  static List<ModelClient> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <ModelClient>[]
      : json.map((dynamic value) => ModelClient.fromJson(value)).toList(growable: true == growable);

  static Map<String, ModelClient> mapFromJson(Map<String, dynamic> json) {
    final map = <String, ModelClient>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = ModelClient.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ModelClient-objects as value to a dart map
  static Map<String, List<ModelClient>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ModelClient>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = ModelClient.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

