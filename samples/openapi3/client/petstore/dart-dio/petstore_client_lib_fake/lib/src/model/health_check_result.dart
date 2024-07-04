//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'health_check_result.g.dart';

/// Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.
///
/// Properties:
/// * [nullableMessage] 
@BuiltValue()
abstract class HealthCheckResult implements Built<HealthCheckResult, HealthCheckResultBuilder> {
  @BuiltValueField(wireName: r'NullableMessage')
  String? get nullableMessage;

  HealthCheckResult._();

  factory HealthCheckResult([void updates(HealthCheckResultBuilder b)]) = _$HealthCheckResult;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(HealthCheckResultBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<HealthCheckResult> get serializer => _$HealthCheckResultSerializer();
}

class _$HealthCheckResultSerializer implements PrimitiveSerializer<HealthCheckResult> {
  @override
  final Iterable<Type> types = const [HealthCheckResult, _$HealthCheckResult];

  @override
  final String wireName = r'HealthCheckResult';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    HealthCheckResult object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.nullableMessage != null) {
      yield r'NullableMessage';
      yield serializers.serialize(
        object.nullableMessage,
        specifiedType: const FullType.nullable(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    HealthCheckResult object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required HealthCheckResultBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'NullableMessage':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType.nullable(String),
          ) as String?;
          if (valueDes == null) continue;
          result.nullableMessage = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  HealthCheckResult deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = HealthCheckResultBuilder();
    final serializedList = (serialized as Iterable<Object?>).toList();
    final unhandled = <Object?>[];
    _deserializeProperties(
      serializers,
      serialized,
      specifiedType: specifiedType,
      serializedList: serializedList,
      unhandled: unhandled,
      result: result,
    );
    return result.build();
  }
}

