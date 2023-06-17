//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_type.dart';
import 'package:openapi/src/model/disc_optional_type_correct.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'composed_disc_required_inconsistent.g.dart';

/// ComposedDiscRequiredInconsistent
///
/// Properties:
/// * [fruitType] 
@BuiltValue()
abstract class ComposedDiscRequiredInconsistent implements Built<ComposedDiscRequiredInconsistent, ComposedDiscRequiredInconsistentBuilder> {
  /// One Of [DiscOptionalTypeCorrect], [FruitType]
  OneOf get oneOf;

  static const String discriminatorFieldName = r'fruitType';

  static const Map<String, Type> discriminatorMapping = {
    r'DiscOptionalTypeCorrect': DiscOptionalTypeCorrect,
    r'FruitType': FruitType,
  };

  ComposedDiscRequiredInconsistent._();

  factory ComposedDiscRequiredInconsistent([void updates(ComposedDiscRequiredInconsistentBuilder b)]) = _$ComposedDiscRequiredInconsistent;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ComposedDiscRequiredInconsistentBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ComposedDiscRequiredInconsistent> get serializer => _$ComposedDiscRequiredInconsistentSerializer();
}

extension ComposedDiscRequiredInconsistentDiscriminatorExt on ComposedDiscRequiredInconsistent {
    String? get discriminatorValue {
        if (this is DiscOptionalTypeCorrect) {
            return r'DiscOptionalTypeCorrect';
        }
        if (this is FruitType) {
            return r'FruitType';
        }
        return null;
    }
}
extension ComposedDiscRequiredInconsistentBuilderDiscriminatorExt on ComposedDiscRequiredInconsistentBuilder {
    String? get discriminatorValue {
        if (this is DiscOptionalTypeCorrectBuilder) {
            return r'DiscOptionalTypeCorrect';
        }
        if (this is FruitTypeBuilder) {
            return r'FruitType';
        }
        return null;
    }
}

class _$ComposedDiscRequiredInconsistentSerializer implements PrimitiveSerializer<ComposedDiscRequiredInconsistent> {
  @override
  final Iterable<Type> types = const [ComposedDiscRequiredInconsistent, _$ComposedDiscRequiredInconsistent];

  @override
  final String wireName = r'ComposedDiscRequiredInconsistent';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ComposedDiscRequiredInconsistent object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
  }

  @override
  Object serialize(
    Serializers serializers,
    ComposedDiscRequiredInconsistent object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value, specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  ComposedDiscRequiredInconsistent deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ComposedDiscRequiredInconsistentBuilder();
    Object? oneOfDataSrc;
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex = serializedList.indexOf(ComposedDiscRequiredInconsistent.discriminatorFieldName) + 1;
    final discValue = serializers.deserialize(serializedList[discIndex], specifiedType: FullType(String)) as String;
    oneOfDataSrc = serialized;
    final oneOfTypes = [DiscOptionalTypeCorrect, FruitType, ];
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
      case r'FruitType':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(FruitType),
        ) as FruitType;
        oneOfType = FruitType;
        break;
      default:
        throw UnsupportedError("Couldn't deserialize oneOf for the discriminator value: ${discValue}");
    }
    result.oneOf = OneOfDynamic(typeIndex: oneOfTypes.indexOf(oneOfType), types: oneOfTypes, value: oneOfResult);
    return result.build();
  }
}
    

