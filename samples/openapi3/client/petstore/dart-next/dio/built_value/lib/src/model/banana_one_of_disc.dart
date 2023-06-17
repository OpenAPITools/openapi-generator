//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_type.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'banana_one_of_disc.g.dart';

/// BananaOneOfDisc
///
/// Properties:
/// * [length]
/// * [fruitType]
@BuiltValue()
abstract class BananaOneOfDisc
    implements Built<BananaOneOfDisc, BananaOneOfDiscBuilder> {
  @BuiltValueField(wireName: r'length')
  int get length;

  /// One Of [FruitType]
  OneOf get oneOf;

  BananaOneOfDisc._();

  factory BananaOneOfDisc([void updates(BananaOneOfDiscBuilder b)]) =
      _$BananaOneOfDisc;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(BananaOneOfDiscBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<BananaOneOfDisc> get serializer =>
      _$BananaOneOfDiscSerializer();
}

class _$BananaOneOfDiscSerializer
    implements PrimitiveSerializer<BananaOneOfDisc> {
  @override
  final Iterable<Type> types = const [BananaOneOfDisc, _$BananaOneOfDisc];

  @override
  final String wireName = r'BananaOneOfDisc';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    BananaOneOfDisc object, {
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
    BananaOneOfDisc object, {
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
    required BananaOneOfDiscBuilder result,
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
  BananaOneOfDisc deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = BananaOneOfDiscBuilder();
    Object? oneOfDataSrc;
    final targetType = const FullType(OneOf, [
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
    oneOfDataSrc = unhandled;
    result.oneOf = serializers.deserialize(oneOfDataSrc,
        specifiedType: targetType) as OneOf;
    return result.build();
  }
}
