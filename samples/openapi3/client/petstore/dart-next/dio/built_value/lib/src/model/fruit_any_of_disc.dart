//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_type.dart';
import 'package:openapi/src/model/apple_any_of_disc.dart';
import 'package:openapi/src/model/banana_any_of_disc.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/any_of.dart';

part 'fruit_any_of_disc.g.dart';

/// FruitAnyOfDisc
///
/// Properties:
/// * [fruitType]
@BuiltValue()
abstract class FruitAnyOfDisc
    implements Built<FruitAnyOfDisc, FruitAnyOfDiscBuilder> {
  /// Any Of [FruitType]
  AnyOf get anyOf;

  FruitAnyOfDisc._();

  factory FruitAnyOfDisc([void updates(FruitAnyOfDiscBuilder b)]) =
      _$FruitAnyOfDisc;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FruitAnyOfDiscBuilder b) =>
      b..fruitType = b.discriminatorValue;

  @BuiltValueSerializer(custom: true)
  static Serializer<FruitAnyOfDisc> get serializer =>
      _$FruitAnyOfDiscSerializer();
}

class _$FruitAnyOfDiscSerializer
    implements PrimitiveSerializer<FruitAnyOfDisc> {
  @override
  final Iterable<Type> types = const [FruitAnyOfDisc, _$FruitAnyOfDisc];

  @override
  final String wireName = r'FruitAnyOfDisc';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FruitAnyOfDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {}

  @override
  Object serialize(
    Serializers serializers,
    FruitAnyOfDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final anyOf = object.anyOf;
    return serializers.serialize(anyOf,
        specifiedType: FullType(
            AnyOf, anyOf.valueTypes.map((type) => FullType(type)).toList()))!;
  }

  @override
  FruitAnyOfDisc deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FruitAnyOfDiscBuilder();
    Object? anyOfDataSrc;
    return result.build();
  }
}
