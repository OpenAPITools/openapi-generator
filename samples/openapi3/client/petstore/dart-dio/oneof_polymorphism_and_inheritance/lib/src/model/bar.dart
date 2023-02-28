//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/foo_ref_or_value.dart';
import 'package:openapi/src/model/entity.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'bar.g.dart';

/// Bar
///
/// Properties:
/// * [id] 
/// * [barPropA] 
/// * [fooPropB] 
/// * [foo] 
/// * [href] - Hyperlink reference
/// * [atSchemaLocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
/// * [atBaseType] - When sub-classing, this defines the super-class
/// * [atType] - When sub-classing, this defines the sub-class Extensible name
@BuiltValue()
abstract class Bar implements Entity, Built<Bar, BarBuilder> {
  @BuiltValueField(wireName: r'foo')
  FooRefOrValue? get foo;

  @BuiltValueField(wireName: r'fooPropB')
  String? get fooPropB;

  @BuiltValueField(wireName: r'barPropA')
  String? get barPropA;

  Bar._();

  factory Bar([void updates(BarBuilder b)]) = _$Bar;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(BarBuilder b) => b..atType=b.discriminatorValue;

  @BuiltValueSerializer(custom: true)
  static Serializer<Bar> get serializer => _$BarSerializer();
}

class _$BarSerializer implements PrimitiveSerializer<Bar> {
  @override
  final Iterable<Type> types = const [Bar, _$Bar];

  @override
  final String wireName = r'Bar';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Bar object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.atSchemaLocation != null) {
      yield r'@schemaLocation';
      yield serializers.serialize(
        object.atSchemaLocation,
        specifiedType: const FullType(String),
      );
    }
    if (object.foo != null) {
      yield r'foo';
      yield serializers.serialize(
        object.foo,
        specifiedType: const FullType(FooRefOrValue),
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
    if (object.barPropA != null) {
      yield r'barPropA';
      yield serializers.serialize(
        object.barPropA,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    Bar object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required BarBuilder result,
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
        case r'foo':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(FooRefOrValue),
          ) as FooRefOrValue;
          result.foo.replace(valueDes);
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
        case r'barPropA':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.barPropA = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  Bar deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = BarBuilder();
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

