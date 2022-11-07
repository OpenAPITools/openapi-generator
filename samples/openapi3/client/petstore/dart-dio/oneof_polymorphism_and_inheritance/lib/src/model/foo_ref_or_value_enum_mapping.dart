//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/foo.dart';
import 'package:openapi/src/model/ref_or_value_enum.dart';
import 'package:openapi/src/model/foo_ref.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'foo_ref_or_value_enum_mapping.g.dart';

/// FooRefOrValueEnumMapping
///
/// Properties:
/// * [objectType] 
/// * [href] - Hyperlink reference
/// * [id] - unique identifier
/// * [atSchemaLocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
/// * [atBaseType] - When sub-classing, this defines the super-class
/// * [atType] - When sub-classing, this defines the sub-class Extensible name
@BuiltValue()
abstract class FooRefOrValueEnumMapping implements Built<FooRefOrValueEnumMapping, FooRefOrValueEnumMappingBuilder> {
  @BuiltValueField(wireName: r'objectType')
  RefOrValueEnum get objectType;
  // enum objectTypeEnum {  REF,  VALUE,  };

  /// One Of [Foo], [FooRef]
  OneOf get oneOf;

  static const String discriminatorFieldName = r'objectType';

  static const Map<String, Type> discriminatorMapping = {
    r'Foo': Foo,
    r'FooRef': FooRef,
    r'REF': FooRef,
    r'VALUE': Foo,
  };

  FooRefOrValueEnumMapping._();

  factory FooRefOrValueEnumMapping([void updates(FooRefOrValueEnumMappingBuilder b)]) = _$FooRefOrValueEnumMapping;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FooRefOrValueEnumMappingBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FooRefOrValueEnumMapping> get serializer => _$FooRefOrValueEnumMappingSerializer();
}

class _$FooRefOrValueEnumMappingSerializer implements PrimitiveSerializer<FooRefOrValueEnumMapping> {
  @override
  final Iterable<Type> types = const [FooRefOrValueEnumMapping, _$FooRefOrValueEnumMapping];

  @override
  final String wireName = r'FooRefOrValueEnumMapping';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FooRefOrValueEnumMapping object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'objectType';
    yield serializers.serialize(
      object.objectType,
      specifiedType: const FullType(RefOrValueEnum),
    );
  }

  @override
  Object serialize(
    Serializers serializers,
    FooRefOrValueEnumMapping object, {
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
    required FooRefOrValueEnumMappingBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'objectType':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(RefOrValueEnum),
          ) as RefOrValueEnum;
          result.objectType = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  FooRefOrValueEnumMapping deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FooRefOrValueEnumMappingBuilder();
    Object? oneOfDataSrc;
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex = serializedList.indexOf(FooRefOrValueEnumMapping.discriminatorFieldName) + 1;
    final discValue = serializers.deserialize(serializedList[discIndex], specifiedType: FullType(String)) as String;
    final unhandled = <Object?>[];
    _deserializeProperties(
      serializers,
      serialized,
      specifiedType: specifiedType,
      serializedList: serializedList,
      unhandled: unhandled,
      result: result
    );
    oneOfDataSrc = unhandled;
    final oneOfTypes = [Foo, FooRef, FooRef, Foo, ];
    Object oneOfResult;
    Type oneOfType;
    switch (discValue) {
      case 'Foo':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(Foo),
        ) as Foo;
        oneOfType = Foo;
        break;
      case 'FooRef':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(FooRef),
        ) as FooRef;
        oneOfType = FooRef;
        break;
      case 'REF':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(FooRef),
        ) as FooRef;
        oneOfType = FooRef;
        break;
      case 'VALUE':
        oneOfResult = serializers.deserialize(
          oneOfDataSrc,
          specifiedType: FullType(Foo),
        ) as Foo;
        oneOfType = Foo;
        break;
      default:
        throw UnsupportedError("Couldn't deserialize oneOf for the discriminator value: ${discValue}");
    }
    result.oneOf = OneOfDynamic(typeIndex: oneOfTypes.indexOf(oneOfType), types: oneOfTypes, value: oneOfResult);
    return result.build();
  }
}

