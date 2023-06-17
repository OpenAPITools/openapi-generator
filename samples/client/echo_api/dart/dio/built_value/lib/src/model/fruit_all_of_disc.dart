//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/banana_all_of_disc.dart';
import 'package:openapi/src/model/apple_all_of_disc.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'fruit_all_of_disc.g.dart';

/// FruitAllOfDisc
///
/// Properties:
/// * [fruitType] 
@BuiltValue()
abstract class FruitAllOfDisc implements Built<FruitAllOfDisc, FruitAllOfDiscBuilder> {
  /// One Of [AppleAllOfDisc], [BananaAllOfDisc]
  OneOf get oneOf;

  static const String discriminatorFieldName = r'fruitType';

  static const Map<String, Type> discriminatorMapping = {
    r'AppleAllOfDisc': AppleAllOfDisc,
    r'BananaAllOfDisc': BananaAllOfDisc,
  };

  FruitAllOfDisc._();

  factory FruitAllOfDisc([void updates(FruitAllOfDiscBuilder b)]) = _$FruitAllOfDisc;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FruitAllOfDiscBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FruitAllOfDisc> get serializer => _$FruitAllOfDiscSerializer();
}

extension FruitAllOfDiscDiscriminatorExt on FruitAllOfDisc {
    String? get discriminatorValue {
        if (this is AppleAllOfDisc) {
            return r'AppleAllOfDisc';
        }
        if (this is BananaAllOfDisc) {
            return r'BananaAllOfDisc';
        }
        return null;
    }
}
extension FruitAllOfDiscBuilderDiscriminatorExt on FruitAllOfDiscBuilder {
    String? get discriminatorValue {
        if (this is AppleAllOfDiscBuilder) {
            return r'AppleAllOfDisc';
        }
        if (this is BananaAllOfDiscBuilder) {
            return r'BananaAllOfDisc';
        }
        return null;
    }
}

class _$FruitAllOfDiscSerializer implements PrimitiveSerializer<FruitAllOfDisc> {
  @override
  final Iterable<Type> types = const [FruitAllOfDisc, _$FruitAllOfDisc];

  @override
  final String wireName = r'FruitAllOfDisc';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FruitAllOfDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
  }

  @override
  Object serialize(
    Serializers serializers,
    FruitAllOfDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value, specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  FruitAllOfDisc deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FruitAllOfDiscBuilder();
    Object? oneOfDataSrc;
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex = serializedList.indexOf(FruitAllOfDisc.discriminatorFieldName) + 1;
    final discValue = serializers.deserialize(serializedList[discIndex], specifiedType: FullType(String)) as String;
    oneOfDataSrc = serialized;
    final oneOfTypes = [AppleAllOfDisc, BananaAllOfDisc, ];
    Object oneOfResult;
    Type oneOfType;
    switch (discValue) {
      case r'AppleAllOfDisc':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(AppleAllOfDisc),
        ) as AppleAllOfDisc;
        oneOfType = AppleAllOfDisc;
        break;
      case r'BananaAllOfDisc':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(BananaAllOfDisc),
        ) as BananaAllOfDisc;
        oneOfType = BananaAllOfDisc;
        break;
      default:
        throw UnsupportedError("Couldn't deserialize oneOf for the discriminator value: ${discValue}");
    }
    result.oneOf = OneOfDynamic(typeIndex: oneOfTypes.indexOf(oneOfType), types: oneOfTypes, value: oneOfResult);
    return result.build();
  }
}
    

