//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/foo.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'foo_get_default_response.g.dart';

/// FooGetDefaultResponse
///
/// Properties:
/// * [string] 
@BuiltValue()
abstract class FooGetDefaultResponse implements Built<FooGetDefaultResponse, FooGetDefaultResponseBuilder> {
  @BuiltValueField(wireName: r'string')
  Foo? get string;

  FooGetDefaultResponse._();

  factory FooGetDefaultResponse([void updates(FooGetDefaultResponseBuilder b)]) = _$FooGetDefaultResponse;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FooGetDefaultResponseBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FooGetDefaultResponse> get serializer => _$FooGetDefaultResponseSerializer();
}

class _$FooGetDefaultResponseSerializer implements PrimitiveSerializer<FooGetDefaultResponse> {
  @override
  final Iterable<Type> types = const [FooGetDefaultResponse, _$FooGetDefaultResponse];

  @override
  final String wireName = r'FooGetDefaultResponse';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FooGetDefaultResponse object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.string != null) {
      yield r'string';
      yield serializers.serialize(
        object.string,
        specifiedType: const FullType(Foo),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    FooGetDefaultResponse object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required FooGetDefaultResponseBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'string':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(Foo),
          ) as Foo;
          result.string.replace(valueDes);
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  FooGetDefaultResponse deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FooGetDefaultResponseBuilder();
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

