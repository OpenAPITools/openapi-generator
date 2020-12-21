//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class HealthCheckResult {
  /// Returns a new [HealthCheckResult] instance.
  HealthCheckResult({
    this.nullableMessage,
  });

  String nullableMessage;

  @override
  bool operator ==(Object other) => identical(this, other) || other is HealthCheckResult &&
     other.nullableMessage == nullableMessage;

  @override
  int get hashCode =>
    (nullableMessage == null ? 0 : nullableMessage.hashCode);

  @override
  String toString() => 'HealthCheckResult[nullableMessage=$nullableMessage]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (nullableMessage != null) {
      json[r'NullableMessage'] = nullableMessage;
    }
    return json;
  }

  /// Returns a new [HealthCheckResult] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static HealthCheckResult fromJson(Map<String, dynamic> json) => json == null
    ? null
    : HealthCheckResult(
        nullableMessage: json[r'NullableMessage'],
    );

  static List<HealthCheckResult> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <HealthCheckResult>[]
      : json.map((dynamic value) => HealthCheckResult.fromJson(value)).toList(growable: true == growable);

  static Map<String, HealthCheckResult> mapFromJson(Map<String, dynamic> json) {
    final map = <String, HealthCheckResult>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = HealthCheckResult.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of HealthCheckResult-objects as value to a dart map
  static Map<String, List<HealthCheckResult>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<HealthCheckResult>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = HealthCheckResult.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

