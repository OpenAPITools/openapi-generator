//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/object_with_duplicate_inline_enum.dart';
import 'package:openapi/src/model/object_with_inline_enum.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'object_that_references_objects_with_duplicate_inline_enums.g.dart';

/// ObjectThatReferencesObjectsWithDuplicateInlineEnums
///
/// Properties:
/// * [objectOne] 
/// * [objectTwo] 
@BuiltValue()
abstract class ObjectThatReferencesObjectsWithDuplicateInlineEnums implements Built<ObjectThatReferencesObjectsWithDuplicateInlineEnums, ObjectThatReferencesObjectsWithDuplicateInlineEnumsBuilder> {
  @BuiltValueField(wireName: r'object_one')
  ObjectWithInlineEnum? get objectOne;

  @BuiltValueField(wireName: r'object_two')
  ObjectWithDuplicateInlineEnum? get objectTwo;

  ObjectThatReferencesObjectsWithDuplicateInlineEnums._();

  factory ObjectThatReferencesObjectsWithDuplicateInlineEnums([void updates(ObjectThatReferencesObjectsWithDuplicateInlineEnumsBuilder b)]) = _$ObjectThatReferencesObjectsWithDuplicateInlineEnums;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ObjectThatReferencesObjectsWithDuplicateInlineEnumsBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ObjectThatReferencesObjectsWithDuplicateInlineEnums> get serializer => _$ObjectThatReferencesObjectsWithDuplicateInlineEnumsSerializer();
}

class _$ObjectThatReferencesObjectsWithDuplicateInlineEnumsSerializer implements PrimitiveSerializer<ObjectThatReferencesObjectsWithDuplicateInlineEnums> {
  @override
  final Iterable<Type> types = const [ObjectThatReferencesObjectsWithDuplicateInlineEnums, _$ObjectThatReferencesObjectsWithDuplicateInlineEnums];

  @override
  final String wireName = r'ObjectThatReferencesObjectsWithDuplicateInlineEnums';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ObjectThatReferencesObjectsWithDuplicateInlineEnums object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.objectOne != null) {
      yield r'object_one';
      yield serializers.serialize(
        object.objectOne,
        specifiedType: const FullType(ObjectWithInlineEnum),
      );
    }
    if (object.objectTwo != null) {
      yield r'object_two';
      yield serializers.serialize(
        object.objectTwo,
        specifiedType: const FullType(ObjectWithDuplicateInlineEnum),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ObjectThatReferencesObjectsWithDuplicateInlineEnums object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ObjectThatReferencesObjectsWithDuplicateInlineEnumsBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'object_one':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(ObjectWithInlineEnum),
          ) as ObjectWithInlineEnum;
          result.objectOne.replace(valueDes);
          break;
        case r'object_two':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(ObjectWithDuplicateInlineEnum),
          ) as ObjectWithDuplicateInlineEnum;
          result.objectTwo.replace(valueDes);
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  ObjectThatReferencesObjectsWithDuplicateInlineEnums deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ObjectThatReferencesObjectsWithDuplicateInlineEnumsBuilder();
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

