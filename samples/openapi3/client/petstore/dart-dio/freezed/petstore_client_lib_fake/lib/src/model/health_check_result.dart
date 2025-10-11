//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.
    ///
    /// Properties:
        /// * [nullableMessage] 

        @freezed
        class HealthCheckResult with _$HealthCheckResult {
        const HealthCheckResult._();
        
        const factory HealthCheckResult({
                    @JsonKey(name: r'NullableMessage') 
    String?
 nullableMessage,
        }) = _HealthCheckResult;


        factory HealthCheckResult.fromJson(Map<String, dynamic> json) => _$HealthCheckResultFromJson(json);






}



