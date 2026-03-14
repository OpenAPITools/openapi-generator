//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'object_with_duplicate_inline_enum.g.dart';

/// ObjectWithDuplicateInlineEnum
///
/// Properties:
/// * [attribute] - Object two attribute enum
@BuiltValue()
abstract class ObjectWithDuplicateInlineEnum implements Built<ObjectWithDuplicateInlineEnum, ObjectWithDuplicateInlineEnumBuilder> {
  /// Object two attribute enum
  @BuiltValueField(wireName: r'attribute')
  BuiltSet<ObjectWithDuplicateInlineEnumAttributeEnum>? get attribute;
  // enum attributeEnum {  value_one,  value_two,  };

  ObjectWithDuplicateInlineEnum._();

  factory ObjectWithDuplicateInlineEnum([void updates(ObjectWithDuplicateInlineEnumBuilder b)]) = _$ObjectWithDuplicateInlineEnum;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ObjectWithDuplicateInlineEnumBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ObjectWithDuplicateInlineEnum> get serializer => _$ObjectWithDuplicateInlineEnumSerializer();
}

class _$ObjectWithDuplicateInlineEnumSerializer implements PrimitiveSerializer<ObjectWithDuplicateInlineEnum> {
  @override
  final Iterable<Type> types = const [ObjectWithDuplicateInlineEnum, _$ObjectWithDuplicateInlineEnum];

  @override
  final String wireName = r'ObjectWithDuplicateInlineEnum';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ObjectWithDuplicateInlineEnum object, {
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
    ObjectWithDuplicateInlineEnum object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ObjectWithDuplicateInlineEnumBuilder result,
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
  ObjectWithDuplicateInlineEnum deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ObjectWithDuplicateInlineEnumBuilder();
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

