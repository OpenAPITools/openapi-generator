//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/one_of_primitive_child.dart';
import 'dart:core';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'giga_one_of.g.dart';

/// GigaOneOf
///
/// Properties:
/// * [name]
@BuiltValue()
abstract class GigaOneOf implements Built<GigaOneOf, GigaOneOfBuilder> {
  /// One Of [DateTime], [OneOfPrimitiveChild], [String], [int], [num]
  OneOf get oneOf;

  GigaOneOf._();

  factory GigaOneOf([void updates(GigaOneOfBuilder b)]) = _$GigaOneOf;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(GigaOneOfBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<GigaOneOf> get serializer => _$GigaOneOfSerializer();
}

class _$GigaOneOfSerializer implements PrimitiveSerializer<GigaOneOf> {
  @override
  final Iterable<Type> types = const [GigaOneOf, _$GigaOneOf];

  @override
  final String wireName = r'GigaOneOf';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    GigaOneOf object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {}

  @override
  Object serialize(
    Serializers serializers,
    GigaOneOf object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value,
        specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  GigaOneOf deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = GigaOneOfBuilder();
    Object? oneOfDataSrc;
    final targetType = const FullType(OneOf, [
      FullType(String),
      FullType(DateTime),
      FullType(int),
      FullType(num),
      FullType(OneOfPrimitiveChild),
      FullType(int),
    ]);
    oneOfDataSrc = serialized;
    result.oneOf = serializers.deserialize(oneOfDataSrc,
        specifiedType: targetType) as OneOf;
    return result.build();
  }
}
