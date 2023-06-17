//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/disc_optional_type_correct.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'composed_disc_optional_type_correct.g.dart';

/// ComposedDiscOptionalTypeCorrect
///
/// Properties:
/// * [fruitType] 
@BuiltValue()
abstract class ComposedDiscOptionalTypeCorrect implements Built<ComposedDiscOptionalTypeCorrect, ComposedDiscOptionalTypeCorrectBuilder> {
  /// One Of [DiscOptionalTypeCorrect]
  OneOf get oneOf;

  static const String discriminatorFieldName = r'fruitType';

  static const Map<String, Type> discriminatorMapping = {
    r'DiscOptionalTypeCorrect': DiscOptionalTypeCorrect,
  };

  ComposedDiscOptionalTypeCorrect._();

  factory ComposedDiscOptionalTypeCorrect([void updates(ComposedDiscOptionalTypeCorrectBuilder b)]) = _$ComposedDiscOptionalTypeCorrect;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ComposedDiscOptionalTypeCorrectBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ComposedDiscOptionalTypeCorrect> get serializer => _$ComposedDiscOptionalTypeCorrectSerializer();
}

extension ComposedDiscOptionalTypeCorrectDiscriminatorExt on ComposedDiscOptionalTypeCorrect {
    String? get discriminatorValue {
        if (this is DiscOptionalTypeCorrect) {
            return r'DiscOptionalTypeCorrect';
        }
        return null;
    }
}
extension ComposedDiscOptionalTypeCorrectBuilderDiscriminatorExt on ComposedDiscOptionalTypeCorrectBuilder {
    String? get discriminatorValue {
        if (this is DiscOptionalTypeCorrectBuilder) {
            return r'DiscOptionalTypeCorrect';
        }
        return null;
    }
}

class _$ComposedDiscOptionalTypeCorrectSerializer implements PrimitiveSerializer<ComposedDiscOptionalTypeCorrect> {
  @override
  final Iterable<Type> types = const [ComposedDiscOptionalTypeCorrect, _$ComposedDiscOptionalTypeCorrect];

  @override
  final String wireName = r'ComposedDiscOptionalTypeCorrect';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ComposedDiscOptionalTypeCorrect object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
  }

  @override
  Object serialize(
    Serializers serializers,
    ComposedDiscOptionalTypeCorrect object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value, specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  ComposedDiscOptionalTypeCorrect deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ComposedDiscOptionalTypeCorrectBuilder();
    Object? oneOfDataSrc;
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex = serializedList.indexOf(ComposedDiscOptionalTypeCorrect.discriminatorFieldName) + 1;
    final discValue = serializers.deserialize(serializedList[discIndex], specifiedType: FullType(String)) as String;
    oneOfDataSrc = serialized;
    final oneOfTypes = [DiscOptionalTypeCorrect, ];
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
      default:
        throw UnsupportedError("Couldn't deserialize oneOf for the discriminator value: ${discValue}");
    }
    result.oneOf = OneOfDynamic(typeIndex: oneOfTypes.indexOf(oneOfType), types: oneOfTypes, value: oneOfResult);
    return result.build();
  }
}
    

