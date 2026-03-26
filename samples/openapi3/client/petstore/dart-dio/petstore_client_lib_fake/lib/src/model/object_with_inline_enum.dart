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
  BuiltSet<ObjectWithInlineEnumAttributeEnum>? get attribute;
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
        specifiedType: const FullType(BuiltSet, [FullType(ObjectWithInlineEnumAttributeEnum)]),
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
            specifiedType: const FullType(BuiltSet, [FullType(ObjectWithInlineEnumAttributeEnum)]),
          ) as BuiltSet<ObjectWithInlineEnumAttributeEnum>;
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

class ObjectWithInlineEnumAttributeEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'value_one')
  static const ObjectWithInlineEnumAttributeEnum valueOne = _$objectWithInlineEnumAttributeEnum_valueOne;
  @BuiltValueEnumConst(wireName: r'value_two')
  static const ObjectWithInlineEnumAttributeEnum valueTwo = _$objectWithInlineEnumAttributeEnum_valueTwo;
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const ObjectWithInlineEnumAttributeEnum unknownDefaultOpenApi = _$objectWithInlineEnumAttributeEnum_unknownDefaultOpenApi;

  static Serializer<ObjectWithInlineEnumAttributeEnum> get serializer => _$objectWithInlineEnumAttributeEnumSerializer;

  const ObjectWithInlineEnumAttributeEnum._(String name): super(name);

  static BuiltSet<ObjectWithInlineEnumAttributeEnum> get values => _$objectWithInlineEnumAttributeEnumValues;
  static ObjectWithInlineEnumAttributeEnum valueOf(String name) => _$objectWithInlineEnumAttributeEnumValueOf(name);
}

