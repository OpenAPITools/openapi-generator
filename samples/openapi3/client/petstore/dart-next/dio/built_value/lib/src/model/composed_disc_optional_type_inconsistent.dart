//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/disc_optional_type_incorrect.dart';
import 'package:openapi/src/model/disc_optional_type_correct.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'composed_disc_optional_type_inconsistent.g.dart';

/// ComposedDiscOptionalTypeInconsistent
///
/// Properties:
/// * [fruitType]
@BuiltValue()
abstract class ComposedDiscOptionalTypeInconsistent
    implements
        Built<ComposedDiscOptionalTypeInconsistent,
            ComposedDiscOptionalTypeInconsistentBuilder> {
  /// One Of [DiscOptionalTypeCorrect], [DiscOptionalTypeIncorrect]
  OneOf get oneOf;

  static const String discriminatorFieldName = r'fruitType';

  static const Map<String, Type> discriminatorMapping = {
    r'DiscOptionalTypeCorrect': DiscOptionalTypeCorrect,
    r'DiscOptionalTypeIncorrect': DiscOptionalTypeIncorrect,
  };

  ComposedDiscOptionalTypeInconsistent._();

  factory ComposedDiscOptionalTypeInconsistent(
          [void updates(ComposedDiscOptionalTypeInconsistentBuilder b)]) =
      _$ComposedDiscOptionalTypeInconsistent;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ComposedDiscOptionalTypeInconsistentBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ComposedDiscOptionalTypeInconsistent> get serializer =>
      _$ComposedDiscOptionalTypeInconsistentSerializer();
}

extension ComposedDiscOptionalTypeInconsistentDiscriminatorExt
    on ComposedDiscOptionalTypeInconsistent {
  String? get discriminatorValue {
    if (this is DiscOptionalTypeCorrect) {
      return r'DiscOptionalTypeCorrect';
    }
    if (this is DiscOptionalTypeIncorrect) {
      return r'DiscOptionalTypeIncorrect';
    }
    return null;
  }
}

extension ComposedDiscOptionalTypeInconsistentBuilderDiscriminatorExt
    on ComposedDiscOptionalTypeInconsistentBuilder {
  String? get discriminatorValue {
    if (this is DiscOptionalTypeCorrectBuilder) {
      return r'DiscOptionalTypeCorrect';
    }
    if (this is DiscOptionalTypeIncorrectBuilder) {
      return r'DiscOptionalTypeIncorrect';
    }
    return null;
  }
}

class _$ComposedDiscOptionalTypeInconsistentSerializer
    implements PrimitiveSerializer<ComposedDiscOptionalTypeInconsistent> {
  @override
  final Iterable<Type> types = const [
    ComposedDiscOptionalTypeInconsistent,
    _$ComposedDiscOptionalTypeInconsistent
  ];

  @override
  final String wireName = r'ComposedDiscOptionalTypeInconsistent';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ComposedDiscOptionalTypeInconsistent object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {}

  @override
  Object serialize(
    Serializers serializers,
    ComposedDiscOptionalTypeInconsistent object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value,
        specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  ComposedDiscOptionalTypeInconsistent deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ComposedDiscOptionalTypeInconsistentBuilder();
    Object? oneOfDataSrc;
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex = serializedList.indexOf(
            ComposedDiscOptionalTypeInconsistent.discriminatorFieldName) +
        1;
    final discValue = serializers.deserialize(serializedList[discIndex],
        specifiedType: FullType(String)) as String;
    oneOfDataSrc = serialized;
    final oneOfTypes = [
      DiscOptionalTypeCorrect,
      DiscOptionalTypeIncorrect,
    ];
    Object oneOfResult;
    Type oneOfType;
    switch (discValue) {
      case r'DiscOptionalTypeCorrect':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(DiscOptionalTypeCorrect),
        ) as DiscOptionalTypeCorrect;
        oneOfType = DiscOptionalTypeCorrect;
        break;
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
