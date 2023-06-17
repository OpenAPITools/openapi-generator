//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'fruit_inline_disc_one_of1.g.dart';

/// FruitInlineDiscOneOf1
///
/// Properties:
/// * [length] 
/// * [fruitType] 
@BuiltValue()
abstract class FruitInlineDiscOneOf1 implements Built<FruitInlineDiscOneOf1, FruitInlineDiscOneOf1Builder> {
  @BuiltValueField(wireName: r'length')
  int get length;

  @BuiltValueField(wireName: r'fruitType')
  String get fruitType;

  FruitInlineDiscOneOf1._();

  factory FruitInlineDiscOneOf1([void updates(FruitInlineDiscOneOf1Builder b)]) = _$FruitInlineDiscOneOf1;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FruitInlineDiscOneOf1Builder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FruitInlineDiscOneOf1> get serializer => _$FruitInlineDiscOneOf1Serializer();
}

class _$FruitInlineDiscOneOf1Serializer implements PrimitiveSerializer<FruitInlineDiscOneOf1> {
  @override
  final Iterable<Type> types = const [FruitInlineDiscOneOf1, _$FruitInlineDiscOneOf1];

  @override
  final String wireName = r'FruitInlineDiscOneOf1';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FruitInlineDiscOneOf1 object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'length';
    yield serializers.serialize(
      object.length,
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
    FruitInlineDiscOneOf1 object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required FruitInlineDiscOneOf1Builder result,
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
  FruitInlineDiscOneOf1 deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FruitInlineDiscOneOf1Builder();
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
    
    

