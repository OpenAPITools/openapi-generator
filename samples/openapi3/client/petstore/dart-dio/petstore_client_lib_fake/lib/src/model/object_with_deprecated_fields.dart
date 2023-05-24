//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:openapi/src/model/deprecated_object.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'object_with_deprecated_fields.g.dart';

/// ObjectWithDeprecatedFields
///
/// Properties:
/// * [uuid] 
/// * [id] 
/// * [deprecatedRef] 
/// * [bars] 
@BuiltValue()
abstract class ObjectWithDeprecatedFields implements Built<ObjectWithDeprecatedFields, ObjectWithDeprecatedFieldsBuilder> {
  @BuiltValueField(wireName: r'uuid')
  String? get uuid;

  @Deprecated('id has been deprecated')
  @BuiltValueField(wireName: r'id')
  num? get id;

  @Deprecated('deprecatedRef has been deprecated')
  @BuiltValueField(wireName: r'deprecatedRef')
  DeprecatedObject? get deprecatedRef;

  @Deprecated('bars has been deprecated')
  @BuiltValueField(wireName: r'bars')
  BuiltList<String>? get bars;

  ObjectWithDeprecatedFields._();

  factory ObjectWithDeprecatedFields([void updates(ObjectWithDeprecatedFieldsBuilder b)]) = _$ObjectWithDeprecatedFields;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ObjectWithDeprecatedFieldsBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ObjectWithDeprecatedFields> get serializer => _$ObjectWithDeprecatedFieldsSerializer();
}

class _$ObjectWithDeprecatedFieldsSerializer implements PrimitiveSerializer<ObjectWithDeprecatedFields> {
  @override
  final Iterable<Type> types = const [ObjectWithDeprecatedFields, _$ObjectWithDeprecatedFields];

  @override
  final String wireName = r'ObjectWithDeprecatedFields';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ObjectWithDeprecatedFields object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.uuid != null) {
      yield r'uuid';
      yield serializers.serialize(
        object.uuid,
        specifiedType: const FullType(String),
      );
    }
    if (object.id != null) {
      yield r'id';
      yield serializers.serialize(
        object.id,
        specifiedType: const FullType(num),
      );
    }
    if (object.deprecatedRef != null) {
      yield r'deprecatedRef';
      yield serializers.serialize(
        object.deprecatedRef,
        specifiedType: const FullType(DeprecatedObject),
      );
    }
    if (object.bars != null) {
      yield r'bars';
      yield serializers.serialize(
        object.bars,
        specifiedType: const FullType(BuiltList, [FullType(String)]),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ObjectWithDeprecatedFields object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ObjectWithDeprecatedFieldsBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'uuid':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.uuid = valueDes;
          break;
        case r'id':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(num),
          ) as num;
          result.id = valueDes;
          break;
        case r'deprecatedRef':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(DeprecatedObject),
          ) as DeprecatedObject;
          result.deprecatedRef.replace(valueDes);
          break;
        case r'bars':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltList, [FullType(String)]),
          ) as BuiltList<String>;
          result.bars.replace(valueDes);
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  ObjectWithDeprecatedFields deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ObjectWithDeprecatedFieldsBuilder();
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

