//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/apple_any_of_disc.dart';
import 'package:openapi/src/model/banana_any_of_disc.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'fruit_any_of_disc.g.dart';

/// FruitAnyOfDisc
///
/// Properties:
/// * [fruitType]
@BuiltValue()
abstract class FruitAnyOfDisc
    implements Built<FruitAnyOfDisc, FruitAnyOfDiscBuilder> {
  /// One Of [AppleAnyOfDisc], [BananaAnyOfDisc]
  OneOf get oneOf;

  static const String discriminatorFieldName = r'fruitType';

  static const Map<String, Type> discriminatorMapping = {
    r'AppleAnyOfDisc': AppleAnyOfDisc,
    r'BananaAnyOfDisc': BananaAnyOfDisc,
  };

  FruitAnyOfDisc._();

  factory FruitAnyOfDisc([void updates(FruitAnyOfDiscBuilder b)]) =
      _$FruitAnyOfDisc;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FruitAnyOfDiscBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FruitAnyOfDisc> get serializer =>
      _$FruitAnyOfDiscSerializer();
}

extension FruitAnyOfDiscDiscriminatorExt on FruitAnyOfDisc {
  String? get discriminatorValue {
    if (this is AppleAnyOfDisc) {
      return r'AppleAnyOfDisc';
    }
    if (this is BananaAnyOfDisc) {
      return r'BananaAnyOfDisc';
    }
    return null;
  }
}

extension FruitAnyOfDiscBuilderDiscriminatorExt on FruitAnyOfDiscBuilder {
  String? get discriminatorValue {
    if (this is AppleAnyOfDiscBuilder) {
      return r'AppleAnyOfDisc';
    }
    if (this is BananaAnyOfDiscBuilder) {
      return r'BananaAnyOfDisc';
    }
    return null;
  }
}

class _$FruitAnyOfDiscSerializer
    implements PrimitiveSerializer<FruitAnyOfDisc> {
  @override
  final Iterable<Type> types = const [FruitAnyOfDisc, _$FruitAnyOfDisc];

  @override
  final String wireName = r'FruitAnyOfDisc';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FruitAnyOfDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {}

  @override
  Object serialize(
    Serializers serializers,
    FruitAnyOfDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value,
        specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  FruitAnyOfDisc deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FruitAnyOfDiscBuilder();
    Object? oneOfDataSrc;
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex =
        serializedList.indexOf(FruitAnyOfDisc.discriminatorFieldName) + 1;
    final discValue = serializers.deserialize(serializedList[discIndex],
        specifiedType: FullType(String)) as String;
    oneOfDataSrc = serialized;
    final oneOfTypes = [
      AppleAnyOfDisc,
      BananaAnyOfDisc,
    ];
    Object oneOfResult;
    Type oneOfType;
    switch (discValue) {
      case r'AppleAnyOfDisc':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(AppleAnyOfDisc),
        ) as AppleAnyOfDisc;
        oneOfType = AppleAnyOfDisc;
        break;
      case r'BananaAnyOfDisc':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(BananaAnyOfDisc),
        ) as BananaAnyOfDisc;
        oneOfType = BananaAnyOfDisc;
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
