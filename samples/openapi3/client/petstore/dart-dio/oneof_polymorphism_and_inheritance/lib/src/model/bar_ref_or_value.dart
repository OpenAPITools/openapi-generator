//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/bar_ref.dart';
import 'package:openapi/src/model/bar.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'bar_ref_or_value.g.dart';

/// BarRefOrValue
///
/// Properties:
/// * [href] - Hyperlink reference
/// * [id] - unique identifier
/// * [atSchemaLocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
/// * [atBaseType] - When sub-classing, this defines the super-class
/// * [atType] - When sub-classing, this defines the sub-class Extensible name
@BuiltValue()
abstract class BarRefOrValue implements Built<BarRefOrValue, BarRefOrValueBuilder> {
  /// One Of [Bar], [BarRef]
  OneOf get oneOf;

  static const String discriminatorFieldName = r'@type';

  static const Map<String, Type> discriminatorMapping = {
    r'Bar': Bar,
    r'BarRef': BarRef,
  };

  BarRefOrValue._();

  factory BarRefOrValue([void updates(BarRefOrValueBuilder b)]) = _$BarRefOrValue;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(BarRefOrValueBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<BarRefOrValue> get serializer => _$BarRefOrValueSerializer();
}

extension BarRefOrValueDiscriminatorExt on BarRefOrValue {
    String? get discriminatorValue {
        if (this is Bar) {
            return r'Bar';
        }
        if (this is BarRef) {
            return r'BarRef';
        }
        return null;
    }
}
extension BarRefOrValueBuilderDiscriminatorExt on BarRefOrValueBuilder {
    String? get discriminatorValue {
        if (this is BarBuilder) {
            return r'Bar';
        }
        if (this is BarRefBuilder) {
            return r'BarRef';
        }
        return null;
    }
}

class _$BarRefOrValueSerializer implements PrimitiveSerializer<BarRefOrValue> {
  @override
  final Iterable<Type> types = const [BarRefOrValue, _$BarRefOrValue];

  @override
  final String wireName = r'BarRefOrValue';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    BarRefOrValue object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
  }

  @override
  Object serialize(
    Serializers serializers,
    BarRefOrValue object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value, specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  BarRefOrValue deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = BarRefOrValueBuilder();
    Object? oneOfDataSrc;
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex = serializedList.indexOf(BarRefOrValue.discriminatorFieldName) + 1;
    final discValue = serializers.deserialize(serializedList[discIndex], specifiedType: FullType(String)) as String;
    oneOfDataSrc = serialized;
    final oneOfTypes = [Bar, BarRef, ];
    Object oneOfResult;
    Type oneOfType;
    switch (discValue) {
      case r'Bar':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(Bar),
        ) as Bar;
        oneOfType = Bar;
        break;
      case r'BarRef':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(BarRef),
        ) as BarRef;
        oneOfType = BarRef;
        break;
      default:
        throw UnsupportedError("Couldn't deserialize oneOf for the discriminator value: ${discValue}");
    }
    result.oneOf = OneOfDynamic(typeIndex: oneOfTypes.indexOf(oneOfType), types: oneOfTypes, value: oneOfResult);
    return result.build();
  }
}

