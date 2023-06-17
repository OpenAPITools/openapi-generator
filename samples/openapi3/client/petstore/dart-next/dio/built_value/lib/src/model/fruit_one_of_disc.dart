//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/banana_one_of_disc.dart';
import 'package:openapi/src/model/apple_one_of_disc.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'fruit_one_of_disc.g.dart';

/// FruitOneOfDisc
///
/// Properties:
/// * [fruitType]
@BuiltValue()
abstract class FruitOneOfDisc
    implements Built<FruitOneOfDisc, FruitOneOfDiscBuilder> {
  /// One Of [AppleOneOfDisc], [BananaOneOfDisc]
  OneOf get oneOf;

  static const String discriminatorFieldName = r'fruitType';

  static const Map<String, Type> discriminatorMapping = {
    r'AppleOneOfDisc': AppleOneOfDisc,
    r'BananaOneOfDisc': BananaOneOfDisc,
  };

  FruitOneOfDisc._();

  factory FruitOneOfDisc([void updates(FruitOneOfDiscBuilder b)]) =
      _$FruitOneOfDisc;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FruitOneOfDiscBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FruitOneOfDisc> get serializer =>
      _$FruitOneOfDiscSerializer();
}

extension FruitOneOfDiscDiscriminatorExt on FruitOneOfDisc {
  String? get discriminatorValue {
    if (this is AppleOneOfDisc) {
      return r'AppleOneOfDisc';
    }
    if (this is BananaOneOfDisc) {
      return r'BananaOneOfDisc';
    }
    return null;
  }
}

extension FruitOneOfDiscBuilderDiscriminatorExt on FruitOneOfDiscBuilder {
  String? get discriminatorValue {
    if (this is AppleOneOfDiscBuilder) {
      return r'AppleOneOfDisc';
    }
    if (this is BananaOneOfDiscBuilder) {
      return r'BananaOneOfDisc';
    }
    return null;
  }
}

class _$FruitOneOfDiscSerializer
    implements PrimitiveSerializer<FruitOneOfDisc> {
  @override
  final Iterable<Type> types = const [FruitOneOfDisc, _$FruitOneOfDisc];

  @override
  final String wireName = r'FruitOneOfDisc';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FruitOneOfDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {}

  @override
  Object serialize(
    Serializers serializers,
    FruitOneOfDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value,
        specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  FruitOneOfDisc deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FruitOneOfDiscBuilder();
    Object? oneOfDataSrc;
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex =
        serializedList.indexOf(FruitOneOfDisc.discriminatorFieldName) + 1;
    final discValue = serializers.deserialize(serializedList[discIndex],
        specifiedType: FullType(String)) as String;
    oneOfDataSrc = serialized;
    final oneOfTypes = [
      AppleOneOfDisc,
      BananaOneOfDisc,
    ];
    Object oneOfResult;
    Type oneOfType;
    switch (discValue) {
      case r'AppleOneOfDisc':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(AppleOneOfDisc),
        ) as AppleOneOfDisc;
        oneOfType = AppleOneOfDisc;
        break;
      case r'BananaOneOfDisc':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(BananaOneOfDisc),
        ) as BananaOneOfDisc;
        oneOfType = BananaOneOfDisc;
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
