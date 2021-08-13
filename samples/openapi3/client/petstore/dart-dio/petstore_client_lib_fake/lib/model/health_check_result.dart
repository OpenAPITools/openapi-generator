//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'health_check_result.g.dart';

abstract class HealthCheckResult implements Built<HealthCheckResult, HealthCheckResultBuilder> {

    @nullable
    @BuiltValueField(wireName: r'NullableMessage')
    String get nullableMessage;

    HealthCheckResult._();

    static void _initializeBuilder(HealthCheckResultBuilder b) => b;

    factory HealthCheckResult([void updates(HealthCheckResultBuilder b)]) = _$HealthCheckResult;

    @BuiltValueSerializer(custom: true)
    static Serializer<HealthCheckResult> get serializer => _$HealthCheckResultSerializer();
}

class _$HealthCheckResultSerializer implements StructuredSerializer<HealthCheckResult> {

    @override
    final Iterable<Type> types = const [HealthCheckResult, _$HealthCheckResult];
    @override
    final String wireName = r'HealthCheckResult';

    @override
    Iterable<Object> serialize(Serializers serializers, HealthCheckResult object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.nullableMessage != null) {
            result
                ..add(r'NullableMessage')
                ..add(serializers.serialize(object.nullableMessage,
                    specifiedType: const FullType(String)));
        }
        return result;
    }

    @override
    HealthCheckResult deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = HealthCheckResultBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'NullableMessage':
                    result.nullableMessage = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
            }
        }
        return result.build();
    }
}

