//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_type.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'apple_one_of_disc.g.dart';

/// AppleOneOfDisc
///
/// Properties:
/// * [seeds] 
/// * [fruitType] 
@BuiltValue()
abstract class AppleOneOfDisc implements Built<AppleOneOfDisc, AppleOneOfDiscBuilder> {
  @BuiltValueField(wireName: r'seeds')
  int get seeds;

  /// One Of [FruitType]
  OneOf get oneOf;

  AppleOneOfDisc._();

  factory AppleOneOfDisc([void updates(AppleOneOfDiscBuilder b)]) = _$AppleOneOfDisc;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(AppleOneOfDiscBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<AppleOneOfDisc> get serializer => _$AppleOneOfDiscSerializer();
}

class _$AppleOneOfDiscSerializer implements PrimitiveSerializer<AppleOneOfDisc> {
  @override
  final Iterable<Type> types = const [AppleOneOfDisc, _$AppleOneOfDisc];

  @override
  final String wireName = r'AppleOneOfDisc';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    AppleOneOfDisc object, {
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
    AppleOneOfDisc object, {
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
    required AppleOneOfDiscBuilder result,
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
  AppleOneOfDisc deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = AppleOneOfDiscBuilder();
    Object? oneOfDataSrc;
    final targetType = const FullType(OneOf, [FullType(FruitType), ]);
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
    
    

