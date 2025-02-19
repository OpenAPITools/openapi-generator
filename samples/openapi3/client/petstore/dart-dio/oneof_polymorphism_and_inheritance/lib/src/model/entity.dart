//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/extensible.dart';
import 'package:openapi/src/model/pizza_speziale.dart';
import 'package:openapi/src/model/foo.dart';
import 'package:openapi/src/model/pizza.dart';
import 'package:openapi/src/model/addressable.dart';
import 'package:openapi/src/model/pasta.dart';
import 'package:openapi/src/model/bar_create.dart';
import 'package:openapi/src/model/bar.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'entity.g.dart';

/// Entity
///
/// Properties:
/// * [href] - Hyperlink reference
/// * [id] - unique identifier
/// * [atSchemaLocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
/// * [atBaseType] - When sub-classing, this defines the super-class
/// * [atType] - When sub-classing, this defines the sub-class Extensible name
@BuiltValue(instantiable: false)
abstract class Entity implements Addressable, Extensible {
  static const String discriminatorFieldName = r'@type';

  static const Map<String, Type> discriminatorMapping = {
    r'Bar': Bar,
    r'Bar_Create': BarCreate,
    r'Foo': Foo,
    r'Pasta': Pasta,
    r'Pizza': Pizza,
    r'PizzaSpeziale': PizzaSpeziale,
  };

  @BuiltValueSerializer(custom: true)
  static Serializer<Entity> get serializer => _$EntitySerializer();
}

extension EntityDiscriminatorExt on Entity {
    String? get discriminatorValue {
        if (this is Bar) {
            return r'Bar';
        }
        if (this is BarCreate) {
            return r'Bar_Create';
        }
        if (this is Foo) {
            return r'Foo';
        }
        if (this is Pasta) {
            return r'Pasta';
        }
        if (this is Pizza) {
            return r'Pizza';
        }
        if (this is PizzaSpeziale) {
            return r'PizzaSpeziale';
        }
        return null;
    }
}
extension EntityBuilderDiscriminatorExt on EntityBuilder {
    String? get discriminatorValue {
        if (this is BarBuilder) {
            return r'Bar';
        }
        if (this is BarCreateBuilder) {
            return r'Bar_Create';
        }
        if (this is FooBuilder) {
            return r'Foo';
        }
        if (this is PastaBuilder) {
            return r'Pasta';
        }
        if (this is PizzaBuilder) {
            return r'Pizza';
        }
        if (this is PizzaSpezialeBuilder) {
            return r'PizzaSpeziale';
        }
        return null;
    }
}

class _$EntitySerializer implements PrimitiveSerializer<Entity> {
  @override
  final Iterable<Type> types = const [Entity];

  @override
  final String wireName = r'Entity';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Entity object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.atSchemaLocation != null) {
      yield r'@schemaLocation';
      yield serializers.serialize(
        object.atSchemaLocation,
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
    Entity object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    if (object is Bar) {
      return serializers.serialize(object, specifiedType: FullType(Bar))!;
    }
    if (object is BarCreate) {
      return serializers.serialize(object, specifiedType: FullType(BarCreate))!;
    }
    if (object is Foo) {
      return serializers.serialize(object, specifiedType: FullType(Foo))!;
    }
    if (object is Pasta) {
      return serializers.serialize(object, specifiedType: FullType(Pasta))!;
    }
    if (object is Pizza) {
      return serializers.serialize(object, specifiedType: FullType(Pizza))!;
    }
    if (object is PizzaSpeziale) {
      return serializers.serialize(object, specifiedType: FullType(PizzaSpeziale))!;
    }
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  @override
  Entity deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex = serializedList.indexOf(Entity.discriminatorFieldName) + 1;
    final discValue = serializers.deserialize(serializedList[discIndex], specifiedType: FullType(String)) as String;
    switch (discValue) {
      case r'Bar':
        return serializers.deserialize(serialized, specifiedType: FullType(Bar)) as Bar;
      case r'Bar_Create':
        return serializers.deserialize(serialized, specifiedType: FullType(BarCreate)) as BarCreate;
      case r'Foo':
        return serializers.deserialize(serialized, specifiedType: FullType(Foo)) as Foo;
      case r'Pasta':
        return serializers.deserialize(serialized, specifiedType: FullType(Pasta)) as Pasta;
      case r'Pizza':
        return serializers.deserialize(serialized, specifiedType: FullType(Pizza)) as Pizza;
      case r'PizzaSpeziale':
        return serializers.deserialize(serialized, specifiedType: FullType(PizzaSpeziale)) as PizzaSpeziale;
      default:
        return serializers.deserialize(serialized, specifiedType: FullType($Entity)) as $Entity;
    }
  }
}

/// a concrete implementation of [Entity], since [Entity] is not instantiable
@BuiltValue(instantiable: true)
abstract class $Entity implements Entity, Built<$Entity, $EntityBuilder> {
  $Entity._();

  factory $Entity([void Function($EntityBuilder)? updates]) = _$$Entity;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults($EntityBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<$Entity> get serializer => _$$EntitySerializer();
}

class _$$EntitySerializer implements PrimitiveSerializer<$Entity> {
  @override
  final Iterable<Type> types = const [$Entity, _$$Entity];

  @override
  final String wireName = r'$Entity';

  @override
  Object serialize(
    Serializers serializers,
    $Entity object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return serializers.serialize(object, specifiedType: FullType(Entity))!;
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required EntityBuilder result,
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
  $Entity deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = $EntityBuilder();
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

