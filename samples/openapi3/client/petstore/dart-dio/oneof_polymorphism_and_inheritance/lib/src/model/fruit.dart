//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/apple.dart';
import 'package:openapi/src/model/banana.dart';
import 'package:openapi/src/model/fruit_type.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'fruit.g.dart';

/// Fruit
///
/// Properties:
/// * [fruitType] 
/// * [seeds] 
/// * [length] 
@BuiltValue()
abstract class Fruit implements Built<Fruit, FruitBuilder> {
  @BuiltValueField(wireName: r'fruitType')
  FruitType get fruitType;
  // enum fruitTypeEnum {  APPLE,  BANANA,  };

  /// One Of [Apple], [Banana]
  OneOf get oneOf;

  static const String discriminatorFieldName = r'fruitType';

  static const Map<String, Type> discriminatorMapping = {
    r'APPLE': Apple,
    r'BANANA': Banana,
  };

  Fruit._();

  factory Fruit([void updates(FruitBuilder b)]) = _$Fruit;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FruitBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<Fruit> get serializer => _$FruitSerializer();
}

extension FruitDiscriminatorExt on Fruit {
    String? get discriminatorValue {
        if (this is Apple) {
            return r'APPLE';
        }
        if (this is Banana) {
            return r'BANANA';
        }
        return null;
    }
}
extension FruitBuilderDiscriminatorExt on FruitBuilder {
    String? get discriminatorValue {
        if (this is AppleBuilder) {
            return r'APPLE';
        }
        if (this is BananaBuilder) {
            return r'BANANA';
        }
        return null;
    }
}

class _$FruitSerializer implements PrimitiveSerializer<Fruit> {
  @override
  final Iterable<Type> types = const [Fruit, _$Fruit];

  @override
  final String wireName = r'Fruit';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Fruit object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'fruitType';
    yield serializers.serialize(
      object.fruitType,
      specifiedType: const FullType(FruitType),
    );
  }

  @override
  Object serialize(
    Serializers serializers,
    Fruit object, {
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
    required FruitBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'fruitType':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(FruitType),
          ) as FruitType;
          result.fruitType = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  Fruit deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FruitBuilder();
    Object? oneOfDataSrc;
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex = serializedList.indexOf(Fruit.discriminatorFieldName) + 1;
    final discValue = serializers.deserialize(serializedList[discIndex], specifiedType: FullType(String)) as String;
    oneOfDataSrc = serialized;
    final oneOfTypes = [Apple, Banana, ];
    Object oneOfResult;
    Type oneOfType;
    switch (discValue) {
      case r'APPLE':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(Apple),
        ) as Apple;
        oneOfType = Apple;
        break;
      case r'BANANA':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(Banana),
        ) as Banana;
        oneOfType = Banana;
        break;
      default:
        throw UnsupportedError("Couldn't deserialize oneOf for the discriminator value: ${discValue}");
    }
    result.oneOf = OneOfDynamic(typeIndex: oneOfTypes.indexOf(oneOfType), types: oneOfTypes, value: oneOfResult);
    return result.build();
  }
}

