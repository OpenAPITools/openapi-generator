//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/disc_missing_from_properties.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'composed_disc_missing_from_properties.g.dart';

/// ComposedDiscMissingFromProperties
///
/// Properties:
/// * [length] 
@BuiltValue()
abstract class ComposedDiscMissingFromProperties implements Built<ComposedDiscMissingFromProperties, ComposedDiscMissingFromPropertiesBuilder> {
  /// One Of [DiscMissingFromProperties]
  OneOf get oneOf;

  static const String discriminatorFieldName = r'fruitType';

  static const Map<String, Type> discriminatorMapping = {
    r'DiscMissingFromProperties': DiscMissingFromProperties,
  };

  ComposedDiscMissingFromProperties._();

  factory ComposedDiscMissingFromProperties([void updates(ComposedDiscMissingFromPropertiesBuilder b)]) = _$ComposedDiscMissingFromProperties;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ComposedDiscMissingFromPropertiesBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ComposedDiscMissingFromProperties> get serializer => _$ComposedDiscMissingFromPropertiesSerializer();
}

extension ComposedDiscMissingFromPropertiesDiscriminatorExt on ComposedDiscMissingFromProperties {
    String? get discriminatorValue {
        if (this is DiscMissingFromProperties) {
            return r'DiscMissingFromProperties';
        }
        return null;
    }
}
extension ComposedDiscMissingFromPropertiesBuilderDiscriminatorExt on ComposedDiscMissingFromPropertiesBuilder {
    String? get discriminatorValue {
        if (this is DiscMissingFromPropertiesBuilder) {
            return r'DiscMissingFromProperties';
        }
        return null;
    }
}

class _$ComposedDiscMissingFromPropertiesSerializer implements PrimitiveSerializer<ComposedDiscMissingFromProperties> {
  @override
  final Iterable<Type> types = const [ComposedDiscMissingFromProperties, _$ComposedDiscMissingFromProperties];

  @override
  final String wireName = r'ComposedDiscMissingFromProperties';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ComposedDiscMissingFromProperties object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
  }

  @override
  Object serialize(
    Serializers serializers,
    ComposedDiscMissingFromProperties object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value, specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  ComposedDiscMissingFromProperties deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ComposedDiscMissingFromPropertiesBuilder();
    Object? oneOfDataSrc;
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex = serializedList.indexOf(ComposedDiscMissingFromProperties.discriminatorFieldName) + 1;
    final discValue = serializers.deserialize(serializedList[discIndex], specifiedType: FullType(String)) as String;
    oneOfDataSrc = serialized;
    final oneOfTypes = [DiscMissingFromProperties, ];
    Object oneOfResult;
    Type oneOfType;
    switch (discValue) {
      case r'DiscMissingFromProperties':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(DiscMissingFromProperties),
        ) as DiscMissingFromProperties;
        oneOfType = DiscMissingFromProperties;
        break;
      default:
        throw UnsupportedError("Couldn't deserialize oneOf for the discriminator value: ${discValue}");
    }
    result.oneOf = OneOfDynamic(typeIndex: oneOfTypes.indexOf(oneOfType), types: oneOfTypes, value: oneOfResult);
    return result.build();
  }
}
    

