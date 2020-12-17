import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'health_check_result.g.dart';

abstract class HealthCheckResult implements Built<HealthCheckResult, HealthCheckResultBuilder> {

    @nullable
    @BuiltValueField(wireName: r'NullableMessage')
    String get nullableMessage;

    // Boilerplate code needed to wire-up generated code
    HealthCheckResult._();

    factory HealthCheckResult([updates(HealthCheckResultBuilder b)]) = _$HealthCheckResult;
    static Serializer<HealthCheckResult> get serializer => _$healthCheckResultSerializer;
}

