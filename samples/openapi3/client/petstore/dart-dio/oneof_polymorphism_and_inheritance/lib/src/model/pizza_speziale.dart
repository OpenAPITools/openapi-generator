//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/pizza.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'pizza_speziale.g.dart';

/// PizzaSpeziale
///
/// Properties:
/// * [toppings] 
/// * [pizzasize] 
/// * [href] - Hyperlink reference
/// * [id] - unique identifier
/// * [atSchemalocation] - A URI to a JSON-Schema file that defines additional attributes and relationships
/// * [atBasetype] - When sub-classing, this defines the super-class
/// * [atType] - When sub-classing, this defines the sub-class Extensible name
@BuiltValue()
abstract class PizzaSpeziale implements Pizza, Built<PizzaSpeziale, PizzaSpezialeBuilder> {
  @BuiltValueField(wireName: r'toppings')
  String? get toppings;

  PizzaSpeziale._();

  factory PizzaSpeziale([void updates(PizzaSpezialeBuilder b)]) = _$PizzaSpeziale;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(PizzaSpezialeBuilder b) => b..atType=b.discriminatorValue;

  @BuiltValueSerializer(custom: true)
  static Serializer<PizzaSpeziale> get serializer => _$PizzaSpezialeSerializer();
}

class _$PizzaSpezialeSerializer implements PrimitiveSerializer<PizzaSpeziale> {
  @override
  final Iterable<Type> types = const [PizzaSpeziale, _$PizzaSpeziale];

  @override
  final String wireName = r'PizzaSpeziale';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    PizzaSpeziale object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.atSchemalocation != null) {
      yield r'@schemaLocation';
      yield serializers.serialize(
        object.atSchemalocation,
        specifiedType: const FullType(String),
      );
    }
    if (object.pizzasize != null) {
      yield r'pizzaSize';
      yield serializers.serialize(
        object.pizzasize,
        specifiedType: const FullType(num),
      );
    }
    if (object.toppings != null) {
      yield r'toppings';
      yield serializers.serialize(
        object.toppings,
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
    PizzaSpeziale object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required PizzaSpezialeBuilder result,
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
        case r'pizzaSize':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(num),
          ) as num;
          result.pizzasize = valueDes;
          break;
        case r'toppings':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.toppings = valueDes;
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
  PizzaSpeziale deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = PizzaSpezialeBuilder();
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

