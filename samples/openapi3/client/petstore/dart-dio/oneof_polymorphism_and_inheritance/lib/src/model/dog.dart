//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'dog.g.dart';

/// Dog
///
/// Properties:
/// * [bark] 
@BuiltValue()
abstract class Dog implements Built<Dog, DogBuilder> {
  @BuiltValueField(wireName: r'bark')
  bool? get bark;

  Dog._();

  factory Dog([void updates(DogBuilder b)]) = _$Dog;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(DogBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<Dog> get serializer => _$DogSerializer();
}

class _$DogSerializer implements PrimitiveSerializer<Dog> {
  @override
  final Iterable<Type> types = const [Dog, _$Dog];

  @override
  final String wireName = r'Dog';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Dog object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.bark != null) {
      yield r'bark';
      yield serializers.serialize(
        object.bark,
        specifiedType: const FullType(bool),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    Dog object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required DogBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'bark':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(bool),
          ) as bool;
          result.bark = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  Dog deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = DogBuilder();
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

