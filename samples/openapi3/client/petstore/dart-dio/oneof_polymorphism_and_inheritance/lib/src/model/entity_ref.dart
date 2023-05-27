//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/extensible.dart';
import 'package:openapi/src/model/bar_ref.dart';
import 'package:openapi/src/model/addressable.dart';
import 'package:openapi/src/model/foo_ref.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'entity_ref.g.dart';

/// Entity reference schema to be use for all entityRef class.
///
/// Properties:
/// * [name] - Name of the related entity.
/// * [atReferredType] - The actual type of the target instance when needed for disambiguation.
/// * [href] - Hyperlink reference
/// * [id] - unique identifier
/// * [atSchemaLocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
/// * [atBaseType] - When sub-classing, this defines the super-class
/// * [atType] - When sub-classing, this defines the sub-class Extensible name
@BuiltValue(instantiable: false)
abstract class EntityRef implements Addressable, Extensible {
  /// The actual type of the target instance when needed for disambiguation.
  @BuiltValueField(wireName: r'@referredType')
  String? get atReferredType;

  /// Name of the related entity.
  @BuiltValueField(wireName: r'name')
  String? get name;

  static const String discriminatorFieldName = r'@type';

  static const Map<String, Type> discriminatorMapping = {
    r'BarRef': BarRef,
    r'FooRef': FooRef,
  };

  @BuiltValueSerializer(custom: true)
  static Serializer<EntityRef> get serializer => _$EntityRefSerializer();
}

extension EntityRefDiscriminatorExt on EntityRef {
    String? get discriminatorValue {
        if (this is BarRef) {
            return r'BarRef';
        }
        if (this is FooRef) {
            return r'FooRef';
        }
        return null;
    }
}
extension EntityRefBuilderDiscriminatorExt on EntityRefBuilder {
    String? get discriminatorValue {
        if (this is BarRefBuilder) {
            return r'BarRef';
        }
        if (this is FooRefBuilder) {
            return r'FooRef';
        }
        return null;
    }
}

class _$EntityRefSerializer implements PrimitiveSerializer<EntityRef> {
  @override
  final Iterable<Type> types = const [EntityRef];

  @override
  final String wireName = r'EntityRef';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    EntityRef object, {
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
    EntityRef object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    if (object is BarRef) {
      return serializers.serialize(object, specifiedType: FullType(BarRef))!;
    }
    if (object is FooRef) {
      return serializers.serialize(object, specifiedType: FullType(FooRef))!;
    }
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  @override
  EntityRef deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex = serializedList.indexOf(EntityRef.discriminatorFieldName) + 1;
    final discValue = serializers.deserialize(serializedList[discIndex], specifiedType: FullType(String)) as String;
    switch (discValue) {
      case r'BarRef':
        return serializers.deserialize(serialized, specifiedType: FullType(BarRef)) as BarRef;
      case r'FooRef':
        return serializers.deserialize(serialized, specifiedType: FullType(FooRef)) as FooRef;
      default:
        return serializers.deserialize(serialized, specifiedType: FullType($EntityRef)) as $EntityRef;
    }
  }
}

/// a concrete implementation of [EntityRef], since [EntityRef] is not instantiable
@BuiltValue(instantiable: true)
abstract class $EntityRef implements EntityRef, Built<$EntityRef, $EntityRefBuilder> {
  $EntityRef._();

  factory $EntityRef([void Function($EntityRefBuilder)? updates]) = _$$EntityRef;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults($EntityRefBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<$EntityRef> get serializer => _$$EntityRefSerializer();
}

class _$$EntityRefSerializer implements PrimitiveSerializer<$EntityRef> {
  @override
  final Iterable<Type> types = const [$EntityRef, _$$EntityRef];

  @override
  final String wireName = r'$EntityRef';

  @override
  Object serialize(
    Serializers serializers,
    $EntityRef object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return serializers.serialize(object, specifiedType: FullType(EntityRef))!;
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required EntityRefBuilder result,
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
  $EntityRef deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = $EntityRefBuilder();
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

