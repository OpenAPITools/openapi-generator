//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/foo.dart';
import 'package:openapi/src/model/foo_ref.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'foo_ref_or_value.g.dart';

/// FooRefOrValue
///
/// Properties:
/// * [href] - Hyperlink reference
/// * [id] - unique identifier
/// * [atSchemaLocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
/// * [atBaseType] - When sub-classing, this defines the super-class
/// * [atType] - When sub-classing, this defines the sub-class Extensible name
@BuiltValue()
abstract class FooRefOrValue implements Built<FooRefOrValue, FooRefOrValueBuilder> {
  /// One Of [Foo], [FooRef]
  OneOf get oneOf;

  static const String discriminatorFieldName = r'@type';

  static const Map<String, Type> discriminatorMapping = {
    r'Foo': Foo,
    r'FooRef': FooRef,
  };

  FooRefOrValue._();

  factory FooRefOrValue([void updates(FooRefOrValueBuilder b)]) = _$FooRefOrValue;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FooRefOrValueBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FooRefOrValue> get serializer => _$FooRefOrValueSerializer();
}

extension FooRefOrValueDiscriminatorExt on FooRefOrValue {
    String? get discriminatorValue {
        if (this is Foo) {
            return r'Foo';
        }
        if (this is FooRef) {
            return r'FooRef';
        }
        return null;
    }
}
extension FooRefOrValueBuilderDiscriminatorExt on FooRefOrValueBuilder {
    String? get discriminatorValue {
        if (this is FooBuilder) {
            return r'Foo';
        }
        if (this is FooRefBuilder) {
            return r'FooRef';
        }
        return null;
    }
}

class _$FooRefOrValueSerializer implements PrimitiveSerializer<FooRefOrValue> {
  @override
  final Iterable<Type> types = const [FooRefOrValue, _$FooRefOrValue];

  @override
  final String wireName = r'FooRefOrValue';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FooRefOrValue object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
  }

  @override
  Object serialize(
    Serializers serializers,
    FooRefOrValue object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value, specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  FooRefOrValue deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FooRefOrValueBuilder();
    Object? oneOfDataSrc;
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex = serializedList.indexOf(FooRefOrValue.discriminatorFieldName) + 1;
    final discValue = serializers.deserialize(serializedList[discIndex], specifiedType: FullType(String)) as String;
    oneOfDataSrc = serialized;
    final oneOfTypes = [Foo, FooRef, ];
    Object oneOfResult;
    Type oneOfType;
    switch (discValue) {
      case r'Foo':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(Foo),
        ) as Foo;
        oneOfType = Foo;
        break;
      case r'FooRef':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(FooRef),
        ) as FooRef;
        oneOfType = FooRef;
        break;
      default:
        throw UnsupportedError("Couldn't deserialize oneOf for the discriminator value: ${discValue}");
    }
    result.oneOf = OneOfDynamic(typeIndex: oneOfTypes.indexOf(oneOfType), types: oneOfTypes, value: oneOfResult);
    return result.build();
  }
}

