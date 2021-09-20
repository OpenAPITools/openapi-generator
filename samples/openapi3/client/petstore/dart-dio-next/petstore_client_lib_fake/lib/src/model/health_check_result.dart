//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'health_check_result.g.dart';

/// Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.
///
/// Properties:
/// * [nullableMessage] 
abstract class HealthCheckResult implements Built<HealthCheckResult, HealthCheckResultBuilder> {
    @BuiltValueField(wireName: r'NullableMessage')
    String? get nullableMessage;

    HealthCheckResult._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(HealthCheckResultBuilder b) => b;

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
    Iterable<Object?> serialize(Serializers serializers, HealthCheckResult object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.nullableMessage != null) {
            result
                ..add(r'NullableMessage')
                ..add(serializers.serialize(object.nullableMessage,
                    specifiedType: const FullType.nullable(String)));
        }
        return result;
    }

    @override
    HealthCheckResult deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = HealthCheckResultBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'NullableMessage':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType.nullable(String)) as String?;
                    if (valueDes == null) continue;
                    result.nullableMessage = valueDes;
                    break;
            }
        }
        return result.build();
    }
}

