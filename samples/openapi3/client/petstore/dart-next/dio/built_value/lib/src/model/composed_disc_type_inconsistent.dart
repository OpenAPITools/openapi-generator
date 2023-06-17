//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/disc_type_incorrect.dart';
import 'package:openapi/src/model/fruit_type.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'composed_disc_type_inconsistent.g.dart';

/// ComposedDiscTypeInconsistent
///
/// Properties:
/// * [fruitType]
@BuiltValue()
abstract class ComposedDiscTypeInconsistent
    implements
        Built<ComposedDiscTypeInconsistent,
            ComposedDiscTypeInconsistentBuilder> {
  /// One Of [DiscTypeIncorrect], [FruitType]
  OneOf get oneOf;

  static const String discriminatorFieldName = r'fruitType';

  static const Map<String, Type> discriminatorMapping = {
    r'DiscTypeIncorrect': DiscTypeIncorrect,
    r'FruitType': FruitType,
  };

  ComposedDiscTypeInconsistent._();

  factory ComposedDiscTypeInconsistent(
          [void updates(ComposedDiscTypeInconsistentBuilder b)]) =
      _$ComposedDiscTypeInconsistent;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ComposedDiscTypeInconsistentBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ComposedDiscTypeInconsistent> get serializer =>
      _$ComposedDiscTypeInconsistentSerializer();
}

extension ComposedDiscTypeInconsistentDiscriminatorExt
    on ComposedDiscTypeInconsistent {
  String? get discriminatorValue {
    if (this is DiscTypeIncorrect) {
      return r'DiscTypeIncorrect';
    }
    if (this is FruitType) {
      return r'FruitType';
    }
    return null;
  }
}

extension ComposedDiscTypeInconsistentBuilderDiscriminatorExt
    on ComposedDiscTypeInconsistentBuilder {
  String? get discriminatorValue {
    if (this is DiscTypeIncorrectBuilder) {
      return r'DiscTypeIncorrect';
    }
    if (this is FruitTypeBuilder) {
      return r'FruitType';
    }
    return null;
  }
}

class _$ComposedDiscTypeInconsistentSerializer
    implements PrimitiveSerializer<ComposedDiscTypeInconsistent> {
  @override
  final Iterable<Type> types = const [
    ComposedDiscTypeInconsistent,
    _$ComposedDiscTypeInconsistent
  ];

  @override
  final String wireName = r'ComposedDiscTypeInconsistent';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ComposedDiscTypeInconsistent object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {}

  @override
  Object serialize(
    Serializers serializers,
    ComposedDiscTypeInconsistent object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value,
        specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  ComposedDiscTypeInconsistent deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ComposedDiscTypeInconsistentBuilder();
    Object? oneOfDataSrc;
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex = serializedList
            .indexOf(ComposedDiscTypeInconsistent.discriminatorFieldName) +
        1;
    final discValue = serializers.deserialize(serializedList[discIndex],
        specifiedType: FullType(String)) as String;
    oneOfDataSrc = serialized;
    final oneOfTypes = [
      DiscTypeIncorrect,
      FruitType,
    ];
    Object oneOfResult;
    Type oneOfType;
    switch (discValue) {
      case r'DiscTypeIncorrect':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(DiscTypeIncorrect),
        ) as DiscTypeIncorrect;
        oneOfType = DiscTypeIncorrect;
        break;
      case r'FruitType':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(FruitType),
        ) as FruitType;
        oneOfType = FruitType;
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
