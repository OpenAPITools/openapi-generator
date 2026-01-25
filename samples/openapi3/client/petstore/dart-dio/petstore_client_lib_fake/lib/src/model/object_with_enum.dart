//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/test_enum.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'object_with_enum.g.dart';

/// ObjectWithEnum
///
/// Properties:
/// * [attribute] 
@BuiltValue()
abstract class ObjectWithEnum implements Built<ObjectWithEnum, ObjectWithEnumBuilder> {
  @BuiltValueField(wireName: r'attribute')
  TestEnum? get attribute;
  // enum attributeEnum {  ,  value_one,  value_two,  };

  ObjectWithEnum._();

  factory ObjectWithEnum([void updates(ObjectWithEnumBuilder b)]) = _$ObjectWithEnum;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ObjectWithEnumBuilder b) => b
      ..attribute = TestEnum.empty;

  @BuiltValueSerializer(custom: true)
  static Serializer<ObjectWithEnum> get serializer => _$ObjectWithEnumSerializer();
}

class _$ObjectWithEnumSerializer implements PrimitiveSerializer<ObjectWithEnum> {
  @override
  final Iterable<Type> types = const [ObjectWithEnum, _$ObjectWithEnum];

  @override
  final String wireName = r'ObjectWithEnum';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ObjectWithEnum object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.attribute != null) {
      yield r'attribute';
      yield serializers.serialize(
        object.attribute,
        specifiedType: const FullType(TestEnum),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ObjectWithEnum object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ObjectWithEnumBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'attribute':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(TestEnum),
          ) as TestEnum;
          result.attribute = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  ObjectWithEnum deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ObjectWithEnumBuilder();
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

