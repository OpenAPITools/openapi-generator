//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/dog.dart';
import 'package:openapi/src/model/cat.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'animal.g.dart';

/// Animal
///
/// Properties:
/// * [bark] 
/// * [declawed] 
@BuiltValue()
abstract class Animal implements Built<Animal, AnimalBuilder> {
  /// One Of [Cat], [Dog]
  OneOf get oneOf;

  Animal._();

  factory Animal([void updates(AnimalBuilder b)]) = _$Animal;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(AnimalBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<Animal> get serializer => _$AnimalSerializer();
}

class _$AnimalSerializer implements PrimitiveSerializer<Animal> {
  @override
  final Iterable<Type> types = const [Animal, _$Animal];

  @override
  final String wireName = r'Animal';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Animal object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
  }

  @override
  Object serialize(
    Serializers serializers,
    Animal object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value, specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  Animal deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = AnimalBuilder();
    Object? oneOfDataSrc;
    final targetType = const FullType(OneOf, [FullType(Dog), FullType(Cat), ]);
    oneOfDataSrc = serialized;
    result.oneOf = serializers.deserialize(oneOfDataSrc, specifiedType: targetType) as OneOf;
    return result.build();
  }
}

