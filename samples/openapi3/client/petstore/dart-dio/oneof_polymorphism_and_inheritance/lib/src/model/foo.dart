//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/entity.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'foo.g.dart';

/// Foo
///
/// Properties:
/// * [fooPropA] 
/// * [fooPropB] 
/// * [href] - Hyperlink reference
/// * [id] - unique identifier
/// * [atSchemaLocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
/// * [atBaseType] - When sub-classing, this defines the super-class
/// * [atType] - When sub-classing, this defines the sub-class Extensible name
@BuiltValue()
abstract class Foo implements Entity, Built<Foo, FooBuilder> {
  @BuiltValueField(wireName: r'fooPropA')
  String? get fooPropA;

  @BuiltValueField(wireName: r'fooPropB')
  String? get fooPropB;

  Foo._();

  factory Foo([void updates(FooBuilder b)]) = _$Foo;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FooBuilder b) => b..atType=b.discriminatorValue;

  @BuiltValueSerializer(custom: true)
  static Serializer<Foo> get serializer => _$FooSerializer();
}

class _$FooSerializer implements PrimitiveSerializer<Foo> {
  @override
  final Iterable<Type> types = const [Foo, _$Foo];

  @override
  final String wireName = r'Foo';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Foo object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.atSchemaLocation != null) {
      yield r'@schemaLocation';
      yield serializers.serialize(
        object.atSchemaLocation,
        specifiedType: const FullType(String),
      );
    }
    if (object.fooPropA != null) {
      yield r'fooPropA';
      yield serializers.serialize(
        object.fooPropA,
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
    if (object.fooPropB != null) {
      yield r'fooPropB';
      yield serializers.serialize(
        object.fooPropB,
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
    Foo object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required FooBuilder result,
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
        case r'fooPropA':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.fooPropA = valueDes;
          break;
        case r'@baseType':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.atBaseType = valueDes;
          break;
        case r'fooPropB':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.fooPropB = valueDes;
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
  Foo deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FooBuilder();
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

