//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_inline_inline_disc_one_of.dart';
import 'package:openapi/src/model/fruit_inline_inline_disc_one_of1.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'fruit_inline_inline_disc.g.dart';

/// FruitInlineInlineDisc
///
/// Properties:
/// * [fruitType]
@BuiltValue()
abstract class FruitInlineInlineDisc
    implements Built<FruitInlineInlineDisc, FruitInlineInlineDiscBuilder> {
  /// One Of [FruitInlineInlineDiscOneOf], [FruitInlineInlineDiscOneOf1]
  OneOf get oneOf;

  static const String discriminatorFieldName = r'fruitType';

  static const Map<String, Type> discriminatorMapping = {
    r'FruitInlineInlineDisc_oneOf': FruitInlineInlineDiscOneOf,
    r'FruitInlineInlineDisc_oneOf_1': FruitInlineInlineDiscOneOf1,
  };

  FruitInlineInlineDisc._();

  factory FruitInlineInlineDisc(
      [void updates(FruitInlineInlineDiscBuilder b)]) = _$FruitInlineInlineDisc;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FruitInlineInlineDiscBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FruitInlineInlineDisc> get serializer =>
      _$FruitInlineInlineDiscSerializer();
}

extension FruitInlineInlineDiscDiscriminatorExt on FruitInlineInlineDisc {
  String? get discriminatorValue {
    if (this is FruitInlineInlineDiscOneOf) {
      return r'FruitInlineInlineDisc_oneOf';
    }
    if (this is FruitInlineInlineDiscOneOf1) {
      return r'FruitInlineInlineDisc_oneOf_1';
    }
    return null;
  }
}

extension FruitInlineInlineDiscBuilderDiscriminatorExt
    on FruitInlineInlineDiscBuilder {
  String? get discriminatorValue {
    if (this is FruitInlineInlineDiscOneOfBuilder) {
      return r'FruitInlineInlineDisc_oneOf';
    }
    if (this is FruitInlineInlineDiscOneOf1Builder) {
      return r'FruitInlineInlineDisc_oneOf_1';
    }
    return null;
  }
}

class _$FruitInlineInlineDiscSerializer
    implements PrimitiveSerializer<FruitInlineInlineDisc> {
  @override
  final Iterable<Type> types = const [
    FruitInlineInlineDisc,
    _$FruitInlineInlineDisc
  ];

  @override
  final String wireName = r'FruitInlineInlineDisc';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FruitInlineInlineDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {}

  @override
  Object serialize(
    Serializers serializers,
    FruitInlineInlineDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value,
        specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  FruitInlineInlineDisc deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FruitInlineInlineDiscBuilder();
    Object? oneOfDataSrc;
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex =
        serializedList.indexOf(FruitInlineInlineDisc.discriminatorFieldName) +
            1;
    final discValue = serializers.deserialize(serializedList[discIndex],
        specifiedType: FullType(String)) as String;
    oneOfDataSrc = serialized;
    final oneOfTypes = [
      FruitInlineInlineDiscOneOf,
      FruitInlineInlineDiscOneOf1,
    ];
    Object oneOfResult;
    Type oneOfType;
    switch (discValue) {
      case r'FruitInlineInlineDisc_oneOf':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(FruitInlineInlineDiscOneOf),
        ) as FruitInlineInlineDiscOneOf;
        oneOfType = FruitInlineInlineDiscOneOf;
        break;
      case r'FruitInlineInlineDisc_oneOf_1':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(FruitInlineInlineDiscOneOf1),
        ) as FruitInlineInlineDiscOneOf1;
        oneOfType = FruitInlineInlineDiscOneOf1;
        break;
      default:
        throw UnsupportedError(
            "Couldn't deserialize oneOf for the discriminator value: ${discValue}");
    }
    result.oneOf = OneOfDynamic(
        typeIndex: oneOfTypes.indexOf(oneOfType),
        types: oneOfTypes,
        value: oneOfResult);
    return result.build();
  }
}
