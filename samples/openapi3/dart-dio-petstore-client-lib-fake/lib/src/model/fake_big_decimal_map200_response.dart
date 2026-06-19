//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'fake_big_decimal_map200_response.g.dart';

/// FakeBigDecimalMap200Response
///
/// Properties:
/// * [someId] 
/// * [someMap] 
@BuiltValue()
abstract class FakeBigDecimalMap200Response implements Built<FakeBigDecimalMap200Response, FakeBigDecimalMap200ResponseBuilder> {
  @BuiltValueField(wireName: r'someId')
  num? get someId;

  @BuiltValueField(wireName: r'someMap')
  BuiltMap<String, num>? get someMap;

  FakeBigDecimalMap200Response._();

  factory FakeBigDecimalMap200Response([void updates(FakeBigDecimalMap200ResponseBuilder b)]) = _$FakeBigDecimalMap200Response;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FakeBigDecimalMap200ResponseBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FakeBigDecimalMap200Response> get serializer => _$FakeBigDecimalMap200ResponseSerializer();
}

class _$FakeBigDecimalMap200ResponseSerializer implements PrimitiveSerializer<FakeBigDecimalMap200Response> {
  @override
  final Iterable<Type> types = const [FakeBigDecimalMap200Response, _$FakeBigDecimalMap200Response];

  @override
  final String wireName = r'FakeBigDecimalMap200Response';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FakeBigDecimalMap200Response object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.someId != null) {
      yield r'someId';
      yield serializers.serialize(
        object.someId,
        specifiedType: const FullType(num),
      );
    }
    if (object.someMap != null) {
      yield r'someMap';
      yield serializers.serialize(
        object.someMap,
        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(num)]),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    FakeBigDecimalMap200Response object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required FakeBigDecimalMap200ResponseBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'someId':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(num),
          ) as num;
          result.someId = valueDes;
          break;
        case r'someMap':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltMap, [FullType(String), FullType(num)]),
          ) as BuiltMap<String, num>;
          result.someMap.replace(valueDes);
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  FakeBigDecimalMap200Response deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FakeBigDecimalMap200ResponseBuilder();
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

