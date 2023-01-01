//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'dart:core';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'example_non_primitive.g.dart';

/// ExampleNonPrimitive
@BuiltValue()
abstract class ExampleNonPrimitive implements Built<ExampleNonPrimitive, ExampleNonPrimitiveBuilder> {
  /// One Of [DateTime], [String], [int], [num]
  OneOf get oneOf;

  ExampleNonPrimitive._();

  factory ExampleNonPrimitive([void updates(ExampleNonPrimitiveBuilder b)]) = _$ExampleNonPrimitive;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ExampleNonPrimitiveBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ExampleNonPrimitive> get serializer => _$ExampleNonPrimitiveSerializer();
}

class _$ExampleNonPrimitiveSerializer implements PrimitiveSerializer<ExampleNonPrimitive> {
  @override
  final Iterable<Type> types = const [ExampleNonPrimitive, _$ExampleNonPrimitive];

  @override
  final String wireName = r'ExampleNonPrimitive';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ExampleNonPrimitive object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
  }

  @override
  Object serialize(
    Serializers serializers,
    ExampleNonPrimitive object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value, specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  ExampleNonPrimitive deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ExampleNonPrimitiveBuilder();
    Object? oneOfDataSrc;
    final targetType = const FullType(OneOf, [FullType(String), FullType(DateTime), FullType(int), FullType(num), ]);
    oneOfDataSrc = serialized;
    result.oneOf = serializers.deserialize(oneOfDataSrc, specifiedType: targetType) as OneOf;
    return result.build();
  }
}

