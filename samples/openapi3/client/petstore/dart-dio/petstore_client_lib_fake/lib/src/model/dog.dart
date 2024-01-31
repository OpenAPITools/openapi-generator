//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/animal.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'dog.g.dart';

/// Dog
///
/// Properties:
/// * [className] 
/// * [color] 
/// * [breed] 
@BuiltValue()
abstract class Dog implements Animal, Built<Dog, DogBuilder> {
  @BuiltValueField(wireName: r'breed')
  String? get breed;

  Dog._();

  factory Dog([void updates(DogBuilder b)]) = _$Dog;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(DogBuilder b) => b..className=b.discriminatorValue
      ..color = 'red';

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
    if (object.color != null) {
      yield r'color';
      yield serializers.serialize(
        object.color,
        specifiedType: const FullType(String),
      );
    }
    if (object.breed != null) {
      yield r'breed';
      yield serializers.serialize(
        object.breed,
        specifiedType: const FullType(String),
      );
    }
    yield r'className';
    yield serializers.serialize(
      object.className,
      specifiedType: const FullType(String),
    );
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
        case r'color':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.color = valueDes;
          break;
        case r'breed':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.breed = valueDes;
          break;
        case r'className':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.className = valueDes;
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

