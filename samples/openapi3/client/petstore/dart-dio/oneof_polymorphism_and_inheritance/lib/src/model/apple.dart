//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'apple.g.dart';

/// Apple
///
/// Properties:
/// * [seeds] 
@BuiltValue()
abstract class Apple implements Built<Apple, AppleBuilder> {
  @BuiltValueField(wireName: r'seeds')
  int get seeds;

  Apple._();

  factory Apple([void updates(AppleBuilder b)]) = _$Apple;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(AppleBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<Apple> get serializer => _$AppleSerializer();
}

class _$AppleSerializer implements PrimitiveSerializer<Apple> {
  @override
  final Iterable<Type> types = const [Apple, _$Apple];

  @override
  final String wireName = r'Apple';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Apple object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'seeds';
    yield serializers.serialize(
      object.seeds,
      specifiedType: const FullType(int),
    );
  }

  @override
  Object serialize(
    Serializers serializers,
    Apple object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required AppleBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'seeds':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.seeds = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  Apple deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = AppleBuilder();
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

