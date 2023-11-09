//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/foo.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'foo_basic_get_default_response.g.dart';

/// FooBasicGetDefaultResponse
///
/// Properties:
/// * [string] 
@BuiltValue()
abstract class FooBasicGetDefaultResponse implements Built<FooBasicGetDefaultResponse, FooBasicGetDefaultResponseBuilder> {
  @BuiltValueField(wireName: r'string')
  Foo? get string;

  FooBasicGetDefaultResponse._();

  factory FooBasicGetDefaultResponse([void updates(FooBasicGetDefaultResponseBuilder b)]) = _$FooBasicGetDefaultResponse;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FooBasicGetDefaultResponseBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FooBasicGetDefaultResponse> get serializer => _$FooBasicGetDefaultResponseSerializer();
}

class _$FooBasicGetDefaultResponseSerializer implements PrimitiveSerializer<FooBasicGetDefaultResponse> {
  @override
  final Iterable<Type> types = const [FooBasicGetDefaultResponse, _$FooBasicGetDefaultResponse];

  @override
  final String wireName = r'FooBasicGetDefaultResponse';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FooBasicGetDefaultResponse object, {
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
    FooBasicGetDefaultResponse object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required FooBasicGetDefaultResponseBuilder result,
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
  FooBasicGetDefaultResponse deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FooBasicGetDefaultResponseBuilder();
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
    

