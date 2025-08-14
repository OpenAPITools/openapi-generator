//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'object_with_inline_enum.g.dart';

/// ObjectWithInlineEnum
///
/// Properties:
/// * [attribute] - Object one attribute enum
@BuiltValue()
abstract class ObjectWithInlineEnum implements Built<ObjectWithInlineEnum, ObjectWithInlineEnumBuilder> {
  /// Object one attribute enum
  @BuiltValueField(wireName: r'attribute')
  BuiltSet<ObjectWithDuplicateInlineEnumAttributeEnum>? get attribute;
  // enum attributeEnum {  value_one,  value_two,  };

  ObjectWithInlineEnum._();

  factory ObjectWithInlineEnum([void updates(ObjectWithInlineEnumBuilder b)]) = _$ObjectWithInlineEnum;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ObjectWithInlineEnumBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ObjectWithInlineEnum> get serializer => _$ObjectWithInlineEnumSerializer();
}

class _$ObjectWithInlineEnumSerializer implements PrimitiveSerializer<ObjectWithInlineEnum> {
  @override
  final Iterable<Type> types = const [ObjectWithInlineEnum, _$ObjectWithInlineEnum];

  @override
  final String wireName = r'ObjectWithInlineEnum';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ObjectWithInlineEnum object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.attribute != null) {
      yield r'attribute';
      yield serializers.serialize(
        object.attribute,
        specifiedType: const FullType(BuiltSet, [FullType(ObjectWithDuplicateInlineEnumAttributeEnum)]),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ObjectWithInlineEnum object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ObjectWithInlineEnumBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'attribute':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltSet, [FullType(ObjectWithDuplicateInlineEnumAttributeEnum)]),
          ) as BuiltSet<ObjectWithDuplicateInlineEnumAttributeEnum>;
          result.attribute.replace(valueDes);
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  ObjectWithInlineEnum deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ObjectWithInlineEnumBuilder();
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

class ObjectWithDuplicateInlineEnumAttributeEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'value_one')
  static const ObjectWithDuplicateInlineEnumAttributeEnum valueOne = _$objectWithDuplicateInlineEnumAttributeEnum_valueOne;
  @BuiltValueEnumConst(wireName: r'value_two')
  static const ObjectWithDuplicateInlineEnumAttributeEnum valueTwo = _$objectWithDuplicateInlineEnumAttributeEnum_valueTwo;
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const ObjectWithDuplicateInlineEnumAttributeEnum unknownDefaultOpenApi = _$objectWithDuplicateInlineEnumAttributeEnum_unknownDefaultOpenApi;

  static Serializer<ObjectWithDuplicateInlineEnumAttributeEnum> get serializer => _$objectWithDuplicateInlineEnumAttributeEnumSerializer;

  const ObjectWithDuplicateInlineEnumAttributeEnum._(String name): super(name);

  static BuiltSet<ObjectWithDuplicateInlineEnumAttributeEnum> get values => _$objectWithDuplicateInlineEnumAttributeEnumValues;
  static ObjectWithDuplicateInlineEnumAttributeEnum valueOf(String name) => _$objectWithDuplicateInlineEnumAttributeEnumValueOf(name);
}

