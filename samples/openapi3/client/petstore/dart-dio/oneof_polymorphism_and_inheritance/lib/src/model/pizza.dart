//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/pizza_speziale.dart';
import 'package:openapi/src/model/entity.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'pizza.g.dart';

/// Pizza
///
/// Properties:
/// * [pizzaSize] 
/// * [href] - Hyperlink reference
/// * [id] - unique identifier
/// * [atSchemaLocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
/// * [atBaseType] - When sub-classing, this defines the super-class
/// * [atType] - When sub-classing, this defines the sub-class Extensible name
@BuiltValue(instantiable: false)
abstract class Pizza implements Entity {
  @BuiltValueField(wireName: r'pizzaSize')
  num? get pizzaSize;

  static const String discriminatorFieldName = r'@type';

  static const Map<String, Type> discriminatorMapping = {
    r'PizzaSpeziale': PizzaSpeziale,
  };

  @BuiltValueSerializer(custom: true)
  static Serializer<Pizza> get serializer => _$PizzaSerializer();
}

extension PizzaDiscriminatorExt on Pizza {
    String? get discriminatorValue {
        if (this is PizzaSpeziale) {
            return r'PizzaSpeziale';
        }
        return null;
    }
}
extension PizzaBuilderDiscriminatorExt on PizzaBuilder {
    String? get discriminatorValue {
        if (this is PizzaSpezialeBuilder) {
            return r'PizzaSpeziale';
        }
        return null;
    }
}

class _$PizzaSerializer implements PrimitiveSerializer<Pizza> {
  @override
  final Iterable<Type> types = const [Pizza];

  @override
  final String wireName = r'Pizza';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Pizza object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.pizzaSize != null) {
      yield r'pizzaSize';
      yield serializers.serialize(
        object.pizzaSize,
        specifiedType: const FullType(num),
      );
    }
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
    Pizza object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    if (object is PizzaSpeziale) {
      return serializers.serialize(object, specifiedType: FullType(PizzaSpeziale))!;
    }
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  @override
  Pizza deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex = serializedList.indexOf(Pizza.discriminatorFieldName) + 1;
    final discValue = serializers.deserialize(serializedList[discIndex], specifiedType: FullType(String)) as String;
    switch (discValue) {
      case r'PizzaSpeziale':
        return serializers.deserialize(serialized, specifiedType: FullType(PizzaSpeziale)) as PizzaSpeziale;
      default:
        return serializers.deserialize(serialized, specifiedType: FullType($Pizza)) as $Pizza;
    }
  }
}

/// a concrete implementation of [Pizza], since [Pizza] is not instantiable
@BuiltValue(instantiable: true)
abstract class $Pizza implements Pizza, Built<$Pizza, $PizzaBuilder> {
  $Pizza._();

  factory $Pizza([void Function($PizzaBuilder)? updates]) = _$$Pizza;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults($PizzaBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<$Pizza> get serializer => _$$PizzaSerializer();
}

class _$$PizzaSerializer implements PrimitiveSerializer<$Pizza> {
  @override
  final Iterable<Type> types = const [$Pizza, _$$Pizza];

  @override
  final String wireName = r'$Pizza';

  @override
  Object serialize(
    Serializers serializers,
    $Pizza object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return serializers.serialize(object, specifiedType: FullType(Pizza))!;
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required PizzaBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'pizzaSize':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(num),
          ) as num;
          result.pizzaSize = valueDes;
          break;
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
  $Pizza deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = $PizzaBuilder();
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

