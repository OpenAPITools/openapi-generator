//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'fruit_inline_disc_one_of.g.dart';

/// FruitInlineDiscOneOf
///
/// Properties:
/// * [seeds] 
/// * [fruitType] 
@BuiltValue()
abstract class FruitInlineDiscOneOf implements Built<FruitInlineDiscOneOf, FruitInlineDiscOneOfBuilder> {
  @BuiltValueField(wireName: r'seeds')
  int get seeds;

  @BuiltValueField(wireName: r'fruitType')
  String get fruitType;

  FruitInlineDiscOneOf._();

  factory FruitInlineDiscOneOf([void updates(FruitInlineDiscOneOfBuilder b)]) = _$FruitInlineDiscOneOf;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FruitInlineDiscOneOfBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FruitInlineDiscOneOf> get serializer => _$FruitInlineDiscOneOfSerializer();
}

class _$FruitInlineDiscOneOfSerializer implements PrimitiveSerializer<FruitInlineDiscOneOf> {
  @override
  final Iterable<Type> types = const [FruitInlineDiscOneOf, _$FruitInlineDiscOneOf];

  @override
  final String wireName = r'FruitInlineDiscOneOf';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FruitInlineDiscOneOf object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'seeds';
    yield serializers.serialize(
      object.seeds,
      specifiedType: const FullType(int),
    );
    yield r'fruitType';
    yield serializers.serialize(
      object.fruitType,
      specifiedType: const FullType(String),
    );
  }

  @override
  Object serialize(
    Serializers serializers,
    FruitInlineDiscOneOf object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required FruitInlineDiscOneOfBuilder result,
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
  FruitInlineDiscOneOf deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FruitInlineDiscOneOfBuilder();
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
    
    

