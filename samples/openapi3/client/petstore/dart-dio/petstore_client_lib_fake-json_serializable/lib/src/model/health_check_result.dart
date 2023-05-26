//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'health_check_result.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class HealthCheckResult {
  /// Returns a new [HealthCheckResult] instance.
  HealthCheckResult({

     this.nullableMessage,
  });

  @JsonKey(
    
    name: r'NullableMessage',
    required: false,
    includeIfNull: false
  )


  final String? nullableMessage;



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

