//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'banana.g.dart';

/// Banana
///
/// Properties:
/// * [length] 
@BuiltValue()
abstract class Banana implements Built<Banana, BananaBuilder> {
  @BuiltValueField(wireName: r'length')
  int get length;

  Banana._();

  factory Banana([void updates(BananaBuilder b)]) = _$Banana;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(BananaBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<Banana> get serializer => _$BananaSerializer();
}

class _$BananaSerializer implements PrimitiveSerializer<Banana> {
  @override
  final Iterable<Type> types = const [Banana, _$Banana];

  @override
  final String wireName = r'Banana';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Banana object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'length';
    yield serializers.serialize(
      object.length,
      specifiedType: const FullType(int),
    );
  }

  @override
  Object serialize(
    Serializers serializers,
    Banana object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required BananaBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'length':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.length = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  Banana deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = BananaBuilder();
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

