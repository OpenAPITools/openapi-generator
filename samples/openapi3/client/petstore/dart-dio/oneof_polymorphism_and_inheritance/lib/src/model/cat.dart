//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'cat.g.dart';

/// Cat
///
/// Properties:
/// * [declawed] 
@BuiltValue()
abstract class Cat implements Built<Cat, CatBuilder> {
  @BuiltValueField(wireName: r'declawed')
  bool? get declawed;

  Cat._();

  factory Cat([void updates(CatBuilder b)]) = _$Cat;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(CatBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<Cat> get serializer => _$CatSerializer();
}

class _$CatSerializer implements PrimitiveSerializer<Cat> {
  @override
  final Iterable<Type> types = const [Cat, _$Cat];

  @override
  final String wireName = r'Cat';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Cat object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.declawed != null) {
      yield r'declawed';
      yield serializers.serialize(
        object.declawed,
        specifiedType: const FullType(bool),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    Cat object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required CatBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'declawed':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(bool),
          ) as bool;
          result.declawed = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  Cat deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = CatBuilder();
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

