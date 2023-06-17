//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_inline_inline_disc_one_of_one_of.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'fruit_inline_inline_disc_one_of.g.dart';

/// FruitInlineInlineDiscOneOf
///
/// Properties:
/// * [seeds]
/// * [fruitType]
@BuiltValue()
abstract class FruitInlineInlineDiscOneOf
    implements
        Built<FruitInlineInlineDiscOneOf, FruitInlineInlineDiscOneOfBuilder> {
  @BuiltValueField(wireName: r'seeds')
  int get seeds;

  /// One Of [FruitInlineInlineDiscOneOfOneOf]
  OneOf get oneOf;

  FruitInlineInlineDiscOneOf._();

  factory FruitInlineInlineDiscOneOf(
          [void updates(FruitInlineInlineDiscOneOfBuilder b)]) =
      _$FruitInlineInlineDiscOneOf;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FruitInlineInlineDiscOneOfBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FruitInlineInlineDiscOneOf> get serializer =>
      _$FruitInlineInlineDiscOneOfSerializer();
}

class _$FruitInlineInlineDiscOneOfSerializer
    implements PrimitiveSerializer<FruitInlineInlineDiscOneOf> {
  @override
  final Iterable<Type> types = const [
    FruitInlineInlineDiscOneOf,
    _$FruitInlineInlineDiscOneOf
  ];

  @override
  final String wireName = r'FruitInlineInlineDiscOneOf';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FruitInlineInlineDiscOneOf object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'seeds';
    yield serializers.serialize(
      object.seeds,
      specifiedType: const FullType(int),
    );
  }

  @override
  Object serialize(
    Serializers serializers,
    FruitInlineInlineDiscOneOf object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    final result =
        _serializeProperties(serializers, object, specifiedType: specifiedType)
            .toList();
    result.addAll(serializers.serialize(oneOf.value,
        specifiedType: FullType(oneOf.valueType)) as Iterable<Object?>);
    return result;
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required FruitInlineInlineDiscOneOfBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'seeds':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.seeds = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  FruitInlineInlineDiscOneOf deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FruitInlineInlineDiscOneOfBuilder();
    Object? oneOfDataSrc;
    final targetType = const FullType(OneOf, [
      FullType(FruitInlineInlineDiscOneOfOneOf),
    ]);
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
    oneOfDataSrc = unhandled;
    result.oneOf = serializers.deserialize(oneOfDataSrc,
        specifiedType: targetType) as OneOf;
    return result.build();
  }
}
