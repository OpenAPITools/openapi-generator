//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'health_check_result.g.dart';

abstract class HealthCheckResult implements Built<HealthCheckResult, HealthCheckResultBuilder> {

    @nullable
    @BuiltValueField(wireName: r'NullableMessage')
    String get nullableMessage;

    // Boilerplate code needed to wire-up generated code
    HealthCheckResult._();

    static void _initializeBuilder(HealthCheckResultBuilder b) => b;

    factory HealthCheckResult([void updates(HealthCheckResultBuilder b)]) = _$HealthCheckResult;
    static Serializer<HealthCheckResult> get serializer => _$healthCheckResultSerializer;
}

