//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'fruit_inline_inline_disc_one_of_one_of.g.dart';

/// FruitInlineInlineDiscOneOfOneOf
///
/// Properties:
/// * [fruitType]
@BuiltValue()
abstract class FruitInlineInlineDiscOneOfOneOf
    implements
        Built<FruitInlineInlineDiscOneOfOneOf,
            FruitInlineInlineDiscOneOfOneOfBuilder> {
  @BuiltValueField(wireName: r'fruitType')
  String get fruitType;

  FruitInlineInlineDiscOneOfOneOf._();

  factory FruitInlineInlineDiscOneOfOneOf(
          [void updates(FruitInlineInlineDiscOneOfOneOfBuilder b)]) =
      _$FruitInlineInlineDiscOneOfOneOf;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FruitInlineInlineDiscOneOfOneOfBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FruitInlineInlineDiscOneOfOneOf> get serializer =>
      _$FruitInlineInlineDiscOneOfOneOfSerializer();
}

class _$FruitInlineInlineDiscOneOfOneOfSerializer
    implements PrimitiveSerializer<FruitInlineInlineDiscOneOfOneOf> {
  @override
  final Iterable<Type> types = const [
    FruitInlineInlineDiscOneOfOneOf,
    _$FruitInlineInlineDiscOneOfOneOf
  ];

  @override
  final String wireName = r'FruitInlineInlineDiscOneOfOneOf';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FruitInlineInlineDiscOneOfOneOf object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'fruitType';
    yield serializers.serialize(
      object.fruitType,
      specifiedType: const FullType(String),
    );
  }

  @override
  Object serialize(
    Serializers serializers,
    FruitInlineInlineDiscOneOfOneOf object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object,
            specifiedType: specifiedType)
        .toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required FruitInlineInlineDiscOneOfOneOfBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'fruitType':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.fruitType = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  FruitInlineInlineDiscOneOfOneOf deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FruitInlineInlineDiscOneOfOneOfBuilder();
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
