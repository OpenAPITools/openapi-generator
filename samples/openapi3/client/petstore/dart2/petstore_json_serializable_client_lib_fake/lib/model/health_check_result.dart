//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: true,
  explicitToJson: true,
)
class HealthCheckResult {
  /// Returns a new [HealthCheckResult] instance.
  HealthCheckResult({
    this.nullableMessage,
  });

  @JsonKey(
    nullable: true,
    name: r'NullableMessage',
    required: false,
  )
  String nullableMessage;

  @override
  bool operator ==(Object other) => identical(this, other) || other is HealthCheckResult &&
     other.nullableMessage == nullableMessage;

  @override
  int get hashCode =>
    (nullableMessage == null ? 0 : nullableMessage.hashCode);

  factory HealthCheckResult.fromJson(Map<String, dynamic> json) => _$HealthCheckResultFromJson(json);

  Map<String, dynamic> toJson() => _$HealthCheckResultToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

