//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/foo_ref_or_value.dart';
import 'package:openapi/src/model/entity.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'bar_create.g.dart';

/// BarCreate
///
/// Properties:
/// * [barpropa] 
/// * [foopropb] 
/// * [foo] 
/// * [href] - Hyperlink reference
/// * [id] - unique identifier
/// * [atSchemalocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
/// * [atBasetype] - When sub-classing, this defines the super-class
/// * [atType] - When sub-classing, this defines the sub-class Extensible name
@BuiltValue()
abstract class BarCreate implements Entity, Built<BarCreate, BarCreateBuilder> {
  @BuiltValueField(wireName: r'foo')
  FooRefOrValue? get foo;

  @BuiltValueField(wireName: r'barPropA')
  String? get barpropa;

  @BuiltValueField(wireName: r'fooPropB')
  String? get foopropb;

  BarCreate._();

  factory BarCreate([void updates(BarCreateBuilder b)]) = _$BarCreate;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(BarCreateBuilder b) => b..atType=b.discriminatorValue;

  @BuiltValueSerializer(custom: true)
  static Serializer<BarCreate> get serializer => _$BarCreateSerializer();
}

class _$BarCreateSerializer implements PrimitiveSerializer<BarCreate> {
  @override
  final Iterable<Type> types = const [BarCreate, _$BarCreate];

  @override
  final String wireName = r'BarCreate';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    BarCreate object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.atSchemalocation != null) {
      yield r'@schemaLocation';
      yield serializers.serialize(
        object.atSchemalocation,
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
    if (object.barpropa != null) {
      yield r'barPropA';
      yield serializers.serialize(
        object.barpropa,
        specifiedType: const FullType(String),
      );
    }
    if (object.foopropb != null) {
      yield r'fooPropB';
      yield serializers.serialize(
        object.foopropb,
        specifiedType: const FullType(String),
      );
    }
    if (object.atBasetype != null) {
      yield r'@baseType';
      yield serializers.serialize(
        object.atBasetype,
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
    BarCreate object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required BarCreateBuilder result,
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
          result.atSchemalocation = valueDes;
          break;
        case r'foo':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(FooRefOrValue),
          ) as FooRefOrValue;
          result.foo.replace(valueDes);
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
        case r'barPropA':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.barpropa = valueDes;
          break;
        case r'fooPropB':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.foopropb = valueDes;
          break;
        case r'@baseType':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.atBasetype = valueDes;
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
  BarCreate deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = BarCreateBuilder();
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

