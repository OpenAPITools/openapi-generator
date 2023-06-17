//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/banana_grandparent_disc.dart';
import 'package:openapi/src/model/apple_grandparent_disc.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'fruit_grandparent_disc.g.dart';

/// FruitGrandparentDisc
///
/// Properties:
/// * [fruitType]
@BuiltValue()
abstract class FruitGrandparentDisc
    implements Built<FruitGrandparentDisc, FruitGrandparentDiscBuilder> {
  /// One Of [AppleGrandparentDisc], [BananaGrandparentDisc]
  OneOf get oneOf;

  static const String discriminatorFieldName = r'fruitType';

  static const Map<String, Type> discriminatorMapping = {
    r'AppleGrandparentDisc': AppleGrandparentDisc,
    r'BananaGrandparentDisc': BananaGrandparentDisc,
  };

  FruitGrandparentDisc._();

  factory FruitGrandparentDisc([void updates(FruitGrandparentDiscBuilder b)]) =
      _$FruitGrandparentDisc;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FruitGrandparentDiscBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FruitGrandparentDisc> get serializer =>
      _$FruitGrandparentDiscSerializer();
}

extension FruitGrandparentDiscDiscriminatorExt on FruitGrandparentDisc {
  String? get discriminatorValue {
    if (this is AppleGrandparentDisc) {
      return r'AppleGrandparentDisc';
    }
    if (this is BananaGrandparentDisc) {
      return r'BananaGrandparentDisc';
    }
    return null;
  }
}

extension FruitGrandparentDiscBuilderDiscriminatorExt
    on FruitGrandparentDiscBuilder {
  String? get discriminatorValue {
    if (this is AppleGrandparentDiscBuilder) {
      return r'AppleGrandparentDisc';
    }
    if (this is BananaGrandparentDiscBuilder) {
      return r'BananaGrandparentDisc';
    }
    return null;
  }
}

class _$FruitGrandparentDiscSerializer
    implements PrimitiveSerializer<FruitGrandparentDisc> {
  @override
  final Iterable<Type> types = const [
    FruitGrandparentDisc,
    _$FruitGrandparentDisc
  ];

  @override
  final String wireName = r'FruitGrandparentDisc';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FruitGrandparentDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {}

  @override
  Object serialize(
    Serializers serializers,
    FruitGrandparentDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value,
        specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  FruitGrandparentDisc deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FruitGrandparentDiscBuilder();
    Object? oneOfDataSrc;
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex =
        serializedList.indexOf(FruitGrandparentDisc.discriminatorFieldName) + 1;
    final discValue = serializers.deserialize(serializedList[discIndex],
        specifiedType: FullType(String)) as String;
    oneOfDataSrc = serialized;
    final oneOfTypes = [
      AppleGrandparentDisc,
      BananaGrandparentDisc,
    ];
    Object oneOfResult;
    Type oneOfType;
    switch (discValue) {
      case r'AppleGrandparentDisc':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(AppleGrandparentDisc),
        ) as AppleGrandparentDisc;
        oneOfType = AppleGrandparentDisc;
        break;
      case r'BananaGrandparentDisc':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(BananaGrandparentDisc),
        ) as BananaGrandparentDisc;
        oneOfType = BananaGrandparentDisc;
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
