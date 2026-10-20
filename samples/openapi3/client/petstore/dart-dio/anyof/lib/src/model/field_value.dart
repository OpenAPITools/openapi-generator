//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'dart:core';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/any_of.dart';

part 'field_value.g.dart';

/// FieldValue
@BuiltValue()
abstract class FieldValue implements Built<FieldValue, FieldValueBuilder> {
  /// Any Of [String], [int]
  AnyOf get anyOf;

  FieldValue._();

  factory FieldValue([void updates(FieldValueBuilder b)]) = _$FieldValue;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FieldValueBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FieldValue> get serializer => _$FieldValueSerializer();
}

class _$FieldValueSerializer implements PrimitiveSerializer<FieldValue> {
  @override
  final Iterable<Type> types = const [FieldValue, _$FieldValue];

  @override
  final String wireName = r'FieldValue';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FieldValue object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
  }

  @override
  Object serialize(
    Serializers serializers,
    FieldValue object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final anyOf = object.anyOf;
    return serializers.serialize(anyOf, specifiedType: FullType(AnyOf, anyOf.types.map((type) => FullType(type)).toList()))!;
  }

  @override
  FieldValue deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FieldValueBuilder();
    Object? anyOfDataSrc;
    final targetType = const FullType(AnyOf, [FullType(int), FullType(String), ]);
    anyOfDataSrc = serialized;
    result.anyOf = serializers.deserialize(anyOfDataSrc, specifiedType: targetType) as AnyOf;
    return result.build();
  }
}


