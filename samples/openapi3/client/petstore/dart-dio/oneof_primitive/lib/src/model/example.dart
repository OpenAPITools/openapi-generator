//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/child.dart';
import 'dart:core';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'example.g.dart';

/// Example
///
/// Properties:
/// * [name] 
@BuiltValue()
abstract class Example implements Built<Example, ExampleBuilder> {
  /// One Of [Child], [int]
  OneOf get oneOf;

  Example._();

  factory Example([void updates(ExampleBuilder b)]) = _$Example;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ExampleBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<Example> get serializer => _$ExampleSerializer();
}

class _$ExampleSerializer implements PrimitiveSerializer<Example> {
  @override
  final Iterable<Type> types = const [Example, _$Example];

  @override
  final String wireName = r'Example';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Example object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
  }

  @override
  Object serialize(
    Serializers serializers,
    Example object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value, specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  Example deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ExampleBuilder();
    Object? oneOfDataSrc;
    final targetType = const FullType(OneOf, [FullType(Child), FullType(int), ]);
    oneOfDataSrc = serialized;
    result.oneOf = serializers.deserialize(oneOfDataSrc, specifiedType: targetType) as OneOf;
    return result.build();
  }
}

