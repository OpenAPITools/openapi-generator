//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_type.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/any_of.dart';

part 'apple_any_of_disc.g.dart';

/// AppleAnyOfDisc
///
/// Properties:
/// * [seeds]
/// * [fruitType]
@BuiltValue()
abstract class AppleAnyOfDisc
    implements Built<AppleAnyOfDisc, AppleAnyOfDiscBuilder> {
  @BuiltValueField(wireName: r'seeds')
  int get seeds;

  /// Any Of [FruitType]
  AnyOf get anyOf;

  AppleAnyOfDisc._();

  factory AppleAnyOfDisc([void updates(AppleAnyOfDiscBuilder b)]) =
      _$AppleAnyOfDisc;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(AppleAnyOfDiscBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<AppleAnyOfDisc> get serializer =>
      _$AppleAnyOfDiscSerializer();
}

class _$AppleAnyOfDiscSerializer
    implements PrimitiveSerializer<AppleAnyOfDisc> {
  @override
  final Iterable<Type> types = const [AppleAnyOfDisc, _$AppleAnyOfDisc];

  @override
  final String wireName = r'AppleAnyOfDisc';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    AppleAnyOfDisc object, {
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
    AppleAnyOfDisc object, {
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
    required AppleAnyOfDiscBuilder result,
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
  AppleAnyOfDisc deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = AppleAnyOfDiscBuilder();
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
