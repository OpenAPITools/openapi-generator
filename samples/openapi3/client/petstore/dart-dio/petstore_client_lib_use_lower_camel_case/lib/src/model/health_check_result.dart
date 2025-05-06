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

     this.nullablemessage,
  });

  @JsonKey(
    
    name: r'NullableMessage',
    required: false,
    includeIfNull: false,
  )


  final String? nullablemessage;





    @override
    bool operator ==(Object other) => identical(this, other) || other is HealthCheckResult &&
      other.nullablemessage == nullablemessage;

    @override
    int get hashCode =>
        (nullablemessage == null ? 0 : nullablemessage.hashCode);

  factory HealthCheckResult.fromJson(Map<String, dynamic> json) => _$HealthCheckResultFromJson(json);

  Map<String, dynamic> toJson() => _$HealthCheckResultToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

