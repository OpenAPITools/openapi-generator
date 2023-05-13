//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/banana_req_disc.dart';
import 'package:openapi/src/model/apple_req_disc.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'fruit_req_disc.g.dart';

/// FruitReqDisc
///
/// Properties:
/// * [seeds]
/// * [fruitType]
/// * [length]
@BuiltValue()
abstract class FruitReqDisc
    implements Built<FruitReqDisc, FruitReqDiscBuilder> {
  /// One Of [AppleReqDisc], [BananaReqDisc]
  OneOf get oneOf;

  static const String discriminatorFieldName = r'fruitType';

  static const Map<String, Type> discriminatorMapping = {
    r'AppleReqDisc': AppleReqDisc,
    r'BananaReqDisc': BananaReqDisc,
  };

  FruitReqDisc._();

  factory FruitReqDisc([void updates(FruitReqDiscBuilder b)]) = _$FruitReqDisc;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FruitReqDiscBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FruitReqDisc> get serializer => _$FruitReqDiscSerializer();
}

extension FruitReqDiscDiscriminatorExt on FruitReqDisc {
  String? get discriminatorValue {
    if (this is AppleReqDisc) {
      return r'AppleReqDisc';
    }
    if (this is BananaReqDisc) {
      return r'BananaReqDisc';
    }
    return null;
  }
}

extension FruitReqDiscBuilderDiscriminatorExt on FruitReqDiscBuilder {
  String? get discriminatorValue {
    if (this is AppleReqDiscBuilder) {
      return r'AppleReqDisc';
    }
    if (this is BananaReqDiscBuilder) {
      return r'BananaReqDisc';
    }
    return null;
  }
}

class _$FruitReqDiscSerializer implements PrimitiveSerializer<FruitReqDisc> {
  @override
  final Iterable<Type> types = const [FruitReqDisc, _$FruitReqDisc];

  @override
  final String wireName = r'FruitReqDisc';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FruitReqDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {}

  @override
  Object serialize(
    Serializers serializers,
    FruitReqDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value,
        specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  FruitReqDisc deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FruitReqDiscBuilder();
    Object? oneOfDataSrc;
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex =
        serializedList.indexOf(FruitReqDisc.discriminatorFieldName) + 1;
    final discValue = serializers.deserialize(serializedList[discIndex],
        specifiedType: FullType(String)) as String;
    oneOfDataSrc = serialized;
    final oneOfTypes = [
      AppleReqDisc,
      BananaReqDisc,
    ];
    Object oneOfResult;
    Type oneOfType;
    switch (discValue) {
      case r'AppleReqDisc':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(AppleReqDisc),
        ) as AppleReqDisc;
        oneOfType = AppleReqDisc;
        break;
      case r'BananaReqDisc':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(BananaReqDisc),
        ) as BananaReqDisc;
        oneOfType = BananaReqDisc;
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
