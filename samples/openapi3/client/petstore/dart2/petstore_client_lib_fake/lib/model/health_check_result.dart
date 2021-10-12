//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class HealthCheckResult {
  /// Returns a new [HealthCheckResult] instance.
  HealthCheckResult({
    this.nullableMessage,
  });


  String? nullableMessage;

  @override
  bool operator ==(Object other) => identical(this, other) || other is HealthCheckResult &&
     other.nullableMessage == nullableMessage;

  @override
  int get hashCode =>
    nullableMessage.hashCode;

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
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static HealthCheckResult fromJson(Map<String, dynamic> json) => HealthCheckResult(
        nullableMessage: json[r'NullableMessage'] as String,
    );

  static List<HealthCheckResult> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<HealthCheckResult>((i) => HealthCheckResult.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <HealthCheckResult>[];

  static Map<String, HealthCheckResult> mapFromJson(dynamic json) {
    final map = <String, HealthCheckResult>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = HealthCheckResult.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of HealthCheckResult-objects as value to a dart map
  static Map<String, List<HealthCheckResult>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<HealthCheckResult>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = HealthCheckResult.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

