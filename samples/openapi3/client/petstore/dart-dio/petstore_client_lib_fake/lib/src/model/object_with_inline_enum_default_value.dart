//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'object_with_inline_enum_default_value.g.dart';

/// ObjectWithInlineEnumDefaultValue
///
/// Properties:
/// * [attribute] - Object one attribute enum with default value
@BuiltValue()
abstract class ObjectWithInlineEnumDefaultValue implements Built<ObjectWithInlineEnumDefaultValue, ObjectWithInlineEnumDefaultValueBuilder> {
  /// Object one attribute enum with default value
  @BuiltValueField(wireName: r'attribute')
  ObjectWithInlineEnumDefaultValueAttributeEnum? get attribute;
  // enum attributeEnum {  value_one,  value_two,  };

  ObjectWithInlineEnumDefaultValue._();

  factory ObjectWithInlineEnumDefaultValue([void updates(ObjectWithInlineEnumDefaultValueBuilder b)]) = _$ObjectWithInlineEnumDefaultValue;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ObjectWithInlineEnumDefaultValueBuilder b) => b
      ..attribute = ObjectWithInlineEnumDefaultValueAttributeEnum.valueOf('value_one');

  @BuiltValueSerializer(custom: true)
  static Serializer<ObjectWithInlineEnumDefaultValue> get serializer => _$ObjectWithInlineEnumDefaultValueSerializer();
}

class _$ObjectWithInlineEnumDefaultValueSerializer implements PrimitiveSerializer<ObjectWithInlineEnumDefaultValue> {
  @override
  final Iterable<Type> types = const [ObjectWithInlineEnumDefaultValue, _$ObjectWithInlineEnumDefaultValue];

  @override
  final String wireName = r'ObjectWithInlineEnumDefaultValue';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ObjectWithInlineEnumDefaultValue object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.attribute != null) {
      yield r'attribute';
      yield serializers.serialize(
        object.attribute,
        specifiedType: const FullType(ObjectWithInlineEnumDefaultValueAttributeEnum),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ObjectWithInlineEnumDefaultValue object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ObjectWithInlineEnumDefaultValueBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'attribute':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(ObjectWithInlineEnumDefaultValueAttributeEnum),
          ) as ObjectWithInlineEnumDefaultValueAttributeEnum;
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
  ObjectWithInlineEnumDefaultValue deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ObjectWithInlineEnumDefaultValueBuilder();
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

class ObjectWithInlineEnumDefaultValueAttributeEnum extends EnumClass {

  /// Object one attribute enum with default value
  @BuiltValueEnumConst(wireName: r'value_one')
  static const ObjectWithInlineEnumDefaultValueAttributeEnum valueOne = _$objectWithInlineEnumDefaultValueAttributeEnum_valueOne;
  /// Object one attribute enum with default value
  @BuiltValueEnumConst(wireName: r'value_two')
  static const ObjectWithInlineEnumDefaultValueAttributeEnum valueTwo = _$objectWithInlineEnumDefaultValueAttributeEnum_valueTwo;
  /// Object one attribute enum with default value
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const ObjectWithInlineEnumDefaultValueAttributeEnum unknownDefaultOpenApi = _$objectWithInlineEnumDefaultValueAttributeEnum_unknownDefaultOpenApi;

  static Serializer<ObjectWithInlineEnumDefaultValueAttributeEnum> get serializer => _$objectWithInlineEnumDefaultValueAttributeEnumSerializer;

  const ObjectWithInlineEnumDefaultValueAttributeEnum._(String name): super(name);

  static BuiltSet<ObjectWithInlineEnumDefaultValueAttributeEnum> get values => _$objectWithInlineEnumDefaultValueAttributeEnumValues;
  static ObjectWithInlineEnumDefaultValueAttributeEnum valueOf(String name) => _$objectWithInlineEnumDefaultValueAttributeEnumValueOf(name);
}

