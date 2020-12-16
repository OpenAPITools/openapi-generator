//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Client {
  /// Returns a new [Client] instance.
  Client({
    this.client,
  });

  String client;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Client &&
     other.client == client;

  @override
  int get hashCode =>
    (client == null ? 0 : client.hashCode);

  @override
  String toString() => 'Client[client=$client]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (client != null) {
      json[r'client'] = client;
    }
    return json;
  }

  /// Returns a new [Client] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static Client fromJson(Map<String, dynamic> json) => json == null
    ? null
    : Client(
        client: json[r'client'],
    );

  static List<Client> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <Client>[]
      : json.map((v) => Client.fromJson(v)).toList(growable: true == growable);

  static Map<String, Client> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Client>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = Client.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of Client-objects as value to a dart map
  static Map<String, List<Client>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Client>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = Client.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

