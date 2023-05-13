//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_type.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/any_of.dart';

part 'banana_any_of_disc.g.dart';

/// BananaAnyOfDisc
///
/// Properties:
/// * [length]
/// * [fruitType]
@BuiltValue()
abstract class BananaAnyOfDisc
    implements Built<BananaAnyOfDisc, BananaAnyOfDiscBuilder> {
  @BuiltValueField(wireName: r'length')
  int get length;

  /// Any Of [FruitType]
  AnyOf get anyOf;

  BananaAnyOfDisc._();

  factory BananaAnyOfDisc([void updates(BananaAnyOfDiscBuilder b)]) =
      _$BananaAnyOfDisc;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(BananaAnyOfDiscBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<BananaAnyOfDisc> get serializer =>
      _$BananaAnyOfDiscSerializer();
}

class _$BananaAnyOfDiscSerializer
    implements PrimitiveSerializer<BananaAnyOfDisc> {
  @override
  final Iterable<Type> types = const [BananaAnyOfDisc, _$BananaAnyOfDisc];

  @override
  final String wireName = r'BananaAnyOfDisc';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    BananaAnyOfDisc object, {
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
    BananaAnyOfDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final anyOf = object.anyOf;
    final result =
        _serializeProperties(serializers, object, specifiedType: specifiedType)
            .toList();
    final serialized = serializers.serialize(anyOf,
        specifiedType: FullType(
            AnyOf, anyOf.valueTypes.map((type) => FullType(type)).toList()));
    result.addAll((serialized is Map
        ? serialized.entries
            .map((e) => <dynamic>[e.key, e.value])
            .expand<dynamic>((e) => e)
        : serialized) as Iterable<Object?>);
    return result;
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required BananaAnyOfDiscBuilder result,
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
  BananaAnyOfDisc deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = BananaAnyOfDiscBuilder();
    Object? anyOfDataSrc;
    final targetType = const FullType(AnyOf, [
      FullType(FruitType),
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
    anyOfDataSrc = unhandled;
    result.anyOf = serializers.deserialize(anyOfDataSrc,
        specifiedType: targetType) as AnyOf;
    return result.build();
  }
}
