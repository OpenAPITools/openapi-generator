//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/dog.dart';
import 'package:openapi/src/model/cat.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'animal.g.dart';

/// Animal
///
/// Properties:
/// * [className] 
/// * [color] 
@BuiltValue(instantiable: false)
abstract class Animal  {
  @BuiltValueField(wireName: r'className')
  String get className;

  @BuiltValueField(wireName: r'color')
  String? get color;

  static const String discriminatorFieldName = r'className';

  static const Map<String, Type> discriminatorMapping = {
    r'CAT': Cat,
    r'DOG': Dog,
  };

  @BuiltValueSerializer(custom: true)
  static Serializer<Animal> get serializer => _$AnimalSerializer();
}

extension AnimalDiscriminatorExt on Animal {
    String? get discriminatorValue {
        if (this is Cat) {
            return r'CAT';
        }
        if (this is Dog) {
            return r'DOG';
        }
        return null;
    }
}
extension AnimalBuilderDiscriminatorExt on AnimalBuilder {
    String? get discriminatorValue {
        if (this is CatBuilder) {
            return r'CAT';
        }
        if (this is DogBuilder) {
            return r'DOG';
        }
        return null;
    }
}

class _$AnimalSerializer implements PrimitiveSerializer<Animal> {
  @override
  final Iterable<Type> types = const [Animal];

  @override
  final String wireName = r'Animal';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Animal object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'className';
    yield serializers.serialize(
      object.className,
      specifiedType: const FullType(String),
    );
    if (object.color != null) {
      yield r'color';
      yield serializers.serialize(
        object.color,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    Animal object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    if (object is Cat) {
      return serializers.serialize(object, specifiedType: FullType(Cat))!;
    }
    if (object is Dog) {
      return serializers.serialize(object, specifiedType: FullType(Dog))!;
    }
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  @override
  Animal deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final serializedList = (serialized as Iterable<Object?>).toList();
    final discIndex = serializedList.indexOf(Animal.discriminatorFieldName) + 1;
    final discValue = serializers.deserialize(serializedList[discIndex], specifiedType: FullType(String)) as String;
    switch (discValue) {
      case r'CAT':
        return serializers.deserialize(serialized, specifiedType: FullType(Cat)) as Cat;
      case r'DOG':
        return serializers.deserialize(serialized, specifiedType: FullType(Dog)) as Dog;
      default:
        return serializers.deserialize(serialized, specifiedType: FullType($Animal)) as $Animal;
    }
  }
}

/// a concrete implementation of [Animal], since [Animal] is not instantiable
@BuiltValue(instantiable: true)
abstract class $Animal implements Animal, Built<$Animal, $AnimalBuilder> {
  $Animal._();

  factory $Animal([void Function($AnimalBuilder)? updates]) = _$$Animal;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults($AnimalBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<$Animal> get serializer => _$$AnimalSerializer();
}

class _$$AnimalSerializer implements PrimitiveSerializer<$Animal> {
  @override
  final Iterable<Type> types = const [$Animal, _$$Animal];

  @override
  final String wireName = r'$Animal';

  @override
  Object serialize(
    Serializers serializers,
    $Animal object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return serializers.serialize(object, specifiedType: FullType(Animal))!;
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required AnimalBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'className':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.className = valueDes;
          break;
        case r'color':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.color = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  $Animal deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = $AnimalBuilder();
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

