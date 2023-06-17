//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_type.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'parent.g.dart';

/// Parent
///
/// Properties:
/// * [fruitType]
@BuiltValue(instantiable: false)
abstract class Parent implements FruitType {
  @BuiltValueSerializer(custom: true)
  static Serializer<Parent> get serializer => _$ParentSerializer();
}

class _$ParentSerializer implements PrimitiveSerializer<Parent> {
  @override
  final Iterable<Type> types = const [Parent];

  @override
  final String wireName = r'Parent';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Parent object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'fruitType';
    yield serializers.serialize(
      object.fruitType,
      specifiedType: const FullType(String),
    );
  }

  @override
  Object serialize(
    Serializers serializers,
    Parent object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object,
            specifiedType: specifiedType)
        .toList();
  }

  @override
  Parent deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return serializers.deserialize(serialized, specifiedType: FullType($Parent))
        as $Parent;
  }
}

/// a concrete implementation of [Parent], since [Parent] is not instantiable
@BuiltValue(instantiable: true)
abstract class $Parent implements Parent, Built<$Parent, $ParentBuilder> {
  $Parent._();

  factory $Parent([void Function($ParentBuilder)? updates]) = _$$Parent;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults($ParentBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<$Parent> get serializer => _$$ParentSerializer();
}

class _$$ParentSerializer implements PrimitiveSerializer<$Parent> {
  @override
  final Iterable<Type> types = const [$Parent, _$$Parent];

  @override
  final String wireName = r'$Parent';

  @override
  Object serialize(
    Serializers serializers,
    $Parent object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return serializers.serialize(object, specifiedType: FullType(Parent))!;
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ParentBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'fruitType':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.fruitType = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  $Parent deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = $ParentBuilder();
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
