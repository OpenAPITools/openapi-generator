//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/apple.dart';
import 'package:openapi/src/model/banana.dart';
import 'package:openapi/src/serializers_util.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/any_of.dart';

part 'fruit.g.dart';

/// Fruit
///
/// Properties:
/// * [color] 
/// * [kind] 
/// * [count] 
@BuiltValue()
abstract class Fruit implements Built<Fruit, FruitBuilder> {
  @BuiltValueField(wireName: r'color')
  String? get color;

  /// Any Of [Apple], [Banana]
  AnyOf2<Apple, Banana> get anyOf;

  Fruit._();

  factory Fruit([void updates(FruitBuilder b)]) = _$Fruit;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FruitBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<Fruit> get serializer => _$FruitSerializer();
}

class _$FruitSerializer implements PrimitiveSerializer<Fruit> {
  @override
  final Iterable<Type> types = const [Fruit, _$Fruit];

  @override
  final String wireName = r'Fruit';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Fruit object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.color != null) {
      yield r'color';
      yield serializers.serialize(
        object.color,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    Fruit object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final anyOf = object.anyOf;
    final result = _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
    final serialized = serializers.serialize(anyOf, specifiedType: FullType(AnyOf, anyOf.valueTypes.map((type) => FullType(type)).toList()));
    result.addAll((serialized is Map ? serialized.entries.map((e) => <dynamic>[e.key, e.value]).expand<dynamic>((e) => e) : serialized) as Iterable<Object?>);
    return result;
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required FruitBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'color':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.color = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  Fruit deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FruitBuilder();
    Object? anyOfDataSrc;
    final targetType = const FullType(AnyOf, [FullType(Apple), FullType(Banana), ]);
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
    anyOfDataSrc = unhandled;
    result.anyOf = anyOfFactory(serializers.deserialize(anyOfDataSrc, specifiedType: targetType) as AnyOfDynamic)<Apple, Banana>();
    return result.build();
  }
}

