//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'health_check_result.freezed.dart';
part 'health_check_result.g.dart';


@freezed
abstract class HealthCheckResult with _$HealthCheckResult {
  const factory HealthCheckResult({
    @JsonKey(
      name: r'NullableMessage',
      required: false,
      includeIfNull: false,
    )
    String? nullableMessage,
  }) = _HealthCheckResult;

  factory HealthCheckResult.fromJson(Map<String, dynamic> json) =>
      _$HealthCheckResultFromJson(json);
}

