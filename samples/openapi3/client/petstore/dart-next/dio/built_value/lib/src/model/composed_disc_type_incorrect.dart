//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/disc_type_incorrect.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'composed_disc_type_incorrect.g.dart';

/// ComposedDiscTypeIncorrect
///
/// Properties:
/// * [fruitType]
@BuiltValue()
abstract class ComposedDiscTypeIncorrect
    implements
        Built<ComposedDiscTypeIncorrect, ComposedDiscTypeIncorrectBuilder> {
  /// One Of [DiscTypeIncorrect]
  OneOf get oneOf;

  static const String discriminatorFieldName = r'fruitType';

  static const Map<String, Type> discriminatorMapping = {
    r'DiscTypeIncorrect': DiscTypeIncorrect,
  };

  ComposedDiscTypeIncorrect._();

  factory ComposedDiscTypeIncorrect(
          [void updates(ComposedDiscTypeIncorrectBuilder b)]) =
      _$ComposedDiscTypeIncorrect;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ComposedDiscTypeIncorrectBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ComposedDiscTypeIncorrect> get serializer =>
      _$ComposedDiscTypeIncorrectSerializer();
}

extension ComposedDiscTypeIncorrectDiscriminatorExt
    on ComposedDiscTypeIncorrect {
  String? get discriminatorValue {
    if (this is DiscTypeIncorrect) {
      return r'DiscTypeIncorrect';
    }
    return null;
  }
}

extension ComposedDiscTypeIncorrectBuilderDiscriminatorExt
    on ComposedDiscTypeIncorrectBuilder {
  String? get discriminatorValue {
    if (this is DiscTypeIncorrectBuilder) {
      return r'DiscTypeIncorrect';
    }
    return null;
  }
}

class _$ComposedDiscTypeIncorrectSerializer
    implements PrimitiveSerializer<ComposedDiscTypeIncorrect> {
  @override
  final Iterable<Type> types = const [
    ComposedDiscTypeIncorrect,
    _$ComposedDiscTypeIncorrect
  ];

  @override
  final String wireName = r'ComposedDiscTypeIncorrect';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ComposedDiscTypeIncorrect object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {}

  @override
  Object serialize(
    Serializers serializers,
    ComposedDiscTypeIncorrect object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value,
        specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  ComposedDiscTypeIncorrect deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ComposedDiscTypeIncorrectBuilder();
    Object? oneOfDataSrc;
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex = serializedList
            .indexOf(ComposedDiscTypeIncorrect.discriminatorFieldName) +
        1;
    final discValue = serializers.deserialize(serializedList[discIndex],
        specifiedType: FullType(String)) as String;
    oneOfDataSrc = serialized;
    final oneOfTypes = [
      DiscTypeIncorrect,
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
