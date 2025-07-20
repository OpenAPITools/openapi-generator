//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/object_one.dart';
import 'package:openapi/src/model/object_two.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'object_three.g.dart';

/// ObjectThree
///
/// Properties:
/// * [objectone] 
/// * [objecttwo] 
@BuiltValue()
abstract class ObjectThree implements Built<ObjectThree, ObjectThreeBuilder> {
  @BuiltValueField(wireName: r'objectone')
  ObjectOne? get objectone;

  @BuiltValueField(wireName: r'objecttwo')
  ObjectTwo? get objecttwo;

  ObjectThree._();

  factory ObjectThree([void updates(ObjectThreeBuilder b)]) = _$ObjectThree;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ObjectThreeBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ObjectThree> get serializer => _$ObjectThreeSerializer();
}

class _$ObjectThreeSerializer implements PrimitiveSerializer<ObjectThree> {
  @override
  final Iterable<Type> types = const [ObjectThree, _$ObjectThree];

  @override
  final String wireName = r'ObjectThree';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ObjectThree object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.objectone != null) {
      yield r'objectone';
      yield serializers.serialize(
        object.objectone,
        specifiedType: const FullType(ObjectOne),
      );
    }
    if (object.objecttwo != null) {
      yield r'objecttwo';
      yield serializers.serialize(
        object.objecttwo,
        specifiedType: const FullType(ObjectTwo),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ObjectThree object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ObjectThreeBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'objectone':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(ObjectOne),
          ) as ObjectOne;
          result.objectone.replace(valueDes);
          break;
        case r'objecttwo':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(ObjectTwo),
          ) as ObjectTwo;
          result.objecttwo.replace(valueDes);
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  ObjectThree deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ObjectThreeBuilder();
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

