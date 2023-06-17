//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/apple_variant1.dart';
import 'package:built_collection/built_collection.dart';
import 'package:openapi/src/model/grape_variant1.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'fruit_variant1.g.dart';

/// FruitVariant1
///
/// Properties:
/// * [color] 
/// * [kind] 
@BuiltValue()
abstract class FruitVariant1 implements Built<FruitVariant1, FruitVariant1Builder> {
  @BuiltValueField(wireName: r'color')
  String? get color;

  /// One Of [AppleVariant1], [BuiltList<GrapeVariant1>]
  OneOf get oneOf;

  FruitVariant1._();

  factory FruitVariant1([void updates(FruitVariant1Builder b)]) = _$FruitVariant1;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FruitVariant1Builder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FruitVariant1> get serializer => _$FruitVariant1Serializer();
}

class _$FruitVariant1Serializer implements PrimitiveSerializer<FruitVariant1> {
  @override
  final Iterable<Type> types = const [FruitVariant1, _$FruitVariant1];

  @override
  final String wireName = r'FruitVariant1';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FruitVariant1 object, {
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
    FruitVariant1 object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    final result = _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
    result.addAll(serializers.serialize(oneOf.value, specifiedType: FullType(oneOf.valueType)) as Iterable<Object?>);
    return result;
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required FruitVariant1Builder result,
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
  FruitVariant1 deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FruitVariant1Builder();
    Object? oneOfDataSrc;
    final targetType = const FullType(OneOf, [FullType(AppleVariant1), FullType(BuiltList, [FullType(GrapeVariant1)]), ]);
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
    result.oneOf = serializers.deserialize(oneOfDataSrc, specifiedType: targetType) as OneOf;
    return result.build();
  }
}
    
    

