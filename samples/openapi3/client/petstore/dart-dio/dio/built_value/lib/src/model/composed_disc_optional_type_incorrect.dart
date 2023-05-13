//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/disc_optional_type_incorrect.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'composed_disc_optional_type_incorrect.g.dart';

/// ComposedDiscOptionalTypeIncorrect
///
/// Properties:
/// * [fruitType]
@BuiltValue()
abstract class ComposedDiscOptionalTypeIncorrect
    implements
        Built<ComposedDiscOptionalTypeIncorrect,
            ComposedDiscOptionalTypeIncorrectBuilder> {
  /// One Of [DiscOptionalTypeIncorrect]
  OneOf get oneOf;

  static const String discriminatorFieldName = r'fruitType';

  static const Map<String, Type> discriminatorMapping = {
    r'DiscOptionalTypeIncorrect': DiscOptionalTypeIncorrect,
  };

  ComposedDiscOptionalTypeIncorrect._();

  factory ComposedDiscOptionalTypeIncorrect(
          [void updates(ComposedDiscOptionalTypeIncorrectBuilder b)]) =
      _$ComposedDiscOptionalTypeIncorrect;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ComposedDiscOptionalTypeIncorrectBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ComposedDiscOptionalTypeIncorrect> get serializer =>
      _$ComposedDiscOptionalTypeIncorrectSerializer();
}

extension ComposedDiscOptionalTypeIncorrectDiscriminatorExt
    on ComposedDiscOptionalTypeIncorrect {
  String? get discriminatorValue {
    if (this is DiscOptionalTypeIncorrect) {
      return r'DiscOptionalTypeIncorrect';
    }
    return null;
  }
}

extension ComposedDiscOptionalTypeIncorrectBuilderDiscriminatorExt
    on ComposedDiscOptionalTypeIncorrectBuilder {
  String? get discriminatorValue {
    if (this is DiscOptionalTypeIncorrectBuilder) {
      return r'DiscOptionalTypeIncorrect';
    }
    return null;
  }
}

class _$ComposedDiscOptionalTypeIncorrectSerializer
    implements PrimitiveSerializer<ComposedDiscOptionalTypeIncorrect> {
  @override
  final Iterable<Type> types = const [
    ComposedDiscOptionalTypeIncorrect,
    _$ComposedDiscOptionalTypeIncorrect
  ];

  @override
  final String wireName = r'ComposedDiscOptionalTypeIncorrect';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ComposedDiscOptionalTypeIncorrect object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {}

  @override
  Object serialize(
    Serializers serializers,
    ComposedDiscOptionalTypeIncorrect object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value,
        specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  ComposedDiscOptionalTypeIncorrect deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ComposedDiscOptionalTypeIncorrectBuilder();
    Object? oneOfDataSrc;
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex = serializedList
            .indexOf(ComposedDiscOptionalTypeIncorrect.discriminatorFieldName) +
        1;
    final discValue = serializers.deserialize(serializedList[discIndex],
        specifiedType: FullType(String)) as String;
    oneOfDataSrc = serialized;
    final oneOfTypes = [
      DiscOptionalTypeIncorrect,
    ];
    Object oneOfResult;
    Type oneOfType;
    switch (discValue) {
      case r'DiscOptionalTypeIncorrect':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(DiscOptionalTypeIncorrect),
        ) as DiscOptionalTypeIncorrect;
        oneOfType = DiscOptionalTypeIncorrect;
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
