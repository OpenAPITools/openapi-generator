//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_inline_disc_one_of.dart';
import 'package:openapi/src/model/fruit_inline_disc_one_of1.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'fruit_inline_disc.g.dart';

/// FruitInlineDisc
///
/// Properties:
/// * [seeds] 
/// * [fruitType] 
/// * [length] 
@BuiltValue()
abstract class FruitInlineDisc implements Built<FruitInlineDisc, FruitInlineDiscBuilder> {
  /// One Of [FruitInlineDiscOneOf], [FruitInlineDiscOneOf1]
  OneOf get oneOf;

  static const String discriminatorFieldName = r'fruitType';

  static const Map<String, Type> discriminatorMapping = {
    r'FruitInlineDisc_oneOf': FruitInlineDiscOneOf,
    r'FruitInlineDisc_oneOf_1': FruitInlineDiscOneOf1,
  };

  FruitInlineDisc._();

  factory FruitInlineDisc([void updates(FruitInlineDiscBuilder b)]) = _$FruitInlineDisc;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FruitInlineDiscBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FruitInlineDisc> get serializer => _$FruitInlineDiscSerializer();
}

extension FruitInlineDiscDiscriminatorExt on FruitInlineDisc {
    String? get discriminatorValue {
        if (this is FruitInlineDiscOneOf) {
            return r'FruitInlineDisc_oneOf';
        }
        if (this is FruitInlineDiscOneOf1) {
            return r'FruitInlineDisc_oneOf_1';
        }
        return null;
    }
}
extension FruitInlineDiscBuilderDiscriminatorExt on FruitInlineDiscBuilder {
    String? get discriminatorValue {
        if (this is FruitInlineDiscOneOfBuilder) {
            return r'FruitInlineDisc_oneOf';
        }
        if (this is FruitInlineDiscOneOf1Builder) {
            return r'FruitInlineDisc_oneOf_1';
        }
        return null;
    }
}

class _$FruitInlineDiscSerializer implements PrimitiveSerializer<FruitInlineDisc> {
  @override
  final Iterable<Type> types = const [FruitInlineDisc, _$FruitInlineDisc];

  @override
  final String wireName = r'FruitInlineDisc';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FruitInlineDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
  }

  @override
  Object serialize(
    Serializers serializers,
    FruitInlineDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value, specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  FruitInlineDisc deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FruitInlineDiscBuilder();
    Object? oneOfDataSrc;
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex = serializedList.indexOf(FruitInlineDisc.discriminatorFieldName) + 1;
    final discValue = serializers.deserialize(serializedList[discIndex], specifiedType: FullType(String)) as String;
    oneOfDataSrc = serialized;
    final oneOfTypes = [FruitInlineDiscOneOf, FruitInlineDiscOneOf1, ];
    Object oneOfResult;
    Type oneOfType;
    switch (discValue) {
      case r'FruitInlineDisc_oneOf':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(FruitInlineDiscOneOf),
        ) as FruitInlineDiscOneOf;
        oneOfType = FruitInlineDiscOneOf;
        break;
      case r'FruitInlineDisc_oneOf_1':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(FruitInlineDiscOneOf1),
        ) as FruitInlineDiscOneOf1;
        oneOfType = FruitInlineDiscOneOf1;
        break;
      default:
        throw UnsupportedError("Couldn't deserialize oneOf for the discriminator value: ${discValue}");
    }
    result.oneOf = OneOfDynamic(typeIndex: oneOfTypes.indexOf(oneOfType), types: oneOfTypes, value: oneOfResult);
    return result.build();
  }
}
    
    
    

