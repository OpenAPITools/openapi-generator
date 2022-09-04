//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'model_client.g.dart';

/// ModelClient
///
/// Properties:
/// * [client] 
@BuiltValue()
abstract class ModelClient implements Built<ModelClient, ModelClientBuilder> {
  @BuiltValueField(wireName: r'client')
  String? get client;

  ModelClient._();

  factory ModelClient([void updates(ModelClientBuilder b)]) = _$ModelClient;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ModelClientBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ModelClient> get serializer => _$ModelClientSerializer();
}

class _$ModelClientSerializer implements PrimitiveSerializer<ModelClient> {
  @override
  final Iterable<Type> types = const [ModelClient, _$ModelClient];

  @override
  final String wireName = r'ModelClient';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ModelClient object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.client != null) {
      yield r'client';
      yield serializers.serialize(
        object.client,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ModelClient object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ModelClientBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'client':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.client = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  ModelClient deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ModelClientBuilder();
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

