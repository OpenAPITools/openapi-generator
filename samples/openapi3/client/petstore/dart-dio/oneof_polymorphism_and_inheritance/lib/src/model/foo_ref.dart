//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/entity_ref.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'foo_ref.g.dart';

/// FooRef
///
/// Properties:
/// * [foorefPropA] 
/// * [href] - Hyperlink reference
/// * [id] - unique identifier
/// * [atSchemaLocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
/// * [atBaseType] - When sub-classing, this defines the super-class
/// * [atType] - When sub-classing, this defines the sub-class Extensible name
@BuiltValue()
abstract class FooRef implements EntityRef, Built<FooRef, FooRefBuilder> {
  @BuiltValueField(wireName: r'foorefPropA')
  String? get foorefPropA;

  FooRef._();

  factory FooRef([void updates(FooRefBuilder b)]) = _$FooRef;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FooRefBuilder b) => b..atType=b.discriminatorValue;

  @BuiltValueSerializer(custom: true)
  static Serializer<FooRef> get serializer => _$FooRefSerializer();
}

class _$FooRefSerializer implements PrimitiveSerializer<FooRef> {
  @override
  final Iterable<Type> types = const [FooRef, _$FooRef];

  @override
  final String wireName = r'FooRef';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FooRef object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.atSchemaLocation != null) {
      yield r'@schemaLocation';
      yield serializers.serialize(
        object.atSchemaLocation,
        specifiedType: const FullType(String),
      );
    }
    if (object.atReferredType != null) {
      yield r'@referredType';
      yield serializers.serialize(
        object.atReferredType,
        specifiedType: const FullType(String),
      );
    }
    if (object.foorefPropA != null) {
      yield r'foorefPropA';
      yield serializers.serialize(
        object.foorefPropA,
        specifiedType: const FullType(String),
      );
    }
    if (object.name != null) {
      yield r'name';
      yield serializers.serialize(
        object.name,
        specifiedType: const FullType(String),
      );
    }
    if (object.atBaseType != null) {
      yield r'@baseType';
      yield serializers.serialize(
        object.atBaseType,
        specifiedType: const FullType(String),
      );
    }
    if (object.href != null) {
      yield r'href';
      yield serializers.serialize(
        object.href,
        specifiedType: const FullType(String),
      );
    }
    if (object.id != null) {
      yield r'id';
      yield serializers.serialize(
        object.id,
        specifiedType: const FullType(String),
      );
    }
    yield r'@type';
    yield serializers.serialize(
      object.atType,
      specifiedType: const FullType(String),
    );
  }

  @override
  Object serialize(
    Serializers serializers,
    FooRef object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required FooRefBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'@schemaLocation':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.atSchemaLocation = valueDes;
          break;
        case r'@referredType':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.atReferredType = valueDes;
          break;
        case r'foorefPropA':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.foorefPropA = valueDes;
          break;
        case r'name':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.name = valueDes;
          break;
        case r'@baseType':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.atBaseType = valueDes;
          break;
        case r'href':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.href = valueDes;
          break;
        case r'id':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.id = valueDes;
          break;
        case r'@type':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.atType = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  FooRef deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FooRefBuilder();
    final serializedList = (serialized as Iterable<Object?>).toList();
    final unhandled = <Object?>[];
    _deserializeProperties(
      serializers,
      serialized,
      specifiedType: specifiedType,
      serializedList: serializedList,
      unhandled: unhandled,
      result: result,
    );
    return result.build();
  }
}

