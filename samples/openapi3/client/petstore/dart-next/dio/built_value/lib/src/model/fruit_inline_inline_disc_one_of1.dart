//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_inline_inline_disc_one_of_one_of.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'fruit_inline_inline_disc_one_of1.g.dart';

/// FruitInlineInlineDiscOneOf1
///
/// Properties:
/// * [length]
/// * [fruitType]
@BuiltValue()
abstract class FruitInlineInlineDiscOneOf1
    implements
        Built<FruitInlineInlineDiscOneOf1, FruitInlineInlineDiscOneOf1Builder> {
  @BuiltValueField(wireName: r'length')
  int get length;

  /// One Of [FruitInlineInlineDiscOneOfOneOf]
  OneOf get oneOf;

  FruitInlineInlineDiscOneOf1._();

  factory FruitInlineInlineDiscOneOf1(
          [void updates(FruitInlineInlineDiscOneOf1Builder b)]) =
      _$FruitInlineInlineDiscOneOf1;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FruitInlineInlineDiscOneOf1Builder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FruitInlineInlineDiscOneOf1> get serializer =>
      _$FruitInlineInlineDiscOneOf1Serializer();
}

class _$FruitInlineInlineDiscOneOf1Serializer
    implements PrimitiveSerializer<FruitInlineInlineDiscOneOf1> {
  @override
  final Iterable<Type> types = const [
    FruitInlineInlineDiscOneOf1,
    _$FruitInlineInlineDiscOneOf1
  ];

  @override
  final String wireName = r'FruitInlineInlineDiscOneOf1';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FruitInlineInlineDiscOneOf1 object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'length';
    yield serializers.serialize(
      object.length,
      specifiedType: const FullType(int),
    );
  }

  @override
  Object serialize(
    Serializers serializers,
    FruitInlineInlineDiscOneOf1 object, {
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
    required FruitInlineInlineDiscOneOf1Builder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'length':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.length = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  FruitInlineInlineDiscOneOf1 deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FruitInlineInlineDiscOneOf1Builder();
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
