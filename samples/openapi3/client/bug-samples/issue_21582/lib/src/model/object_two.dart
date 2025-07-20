//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'object_two.g.dart';

/// ObjectTwo
///
/// Properties:
/// * [attribute] - Object two attribute enum
@BuiltValue()
abstract class ObjectTwo implements Built<ObjectTwo, ObjectTwoBuilder> {
  /// Object two attribute enum
  @BuiltValueField(wireName: r'attribute')
  BuiltSet<ObjectTwoAttributeEnum>? get attribute;
  // enum attributeEnum {  valueone,  vauetwo,  };

  ObjectTwo._();

  factory ObjectTwo([void updates(ObjectTwoBuilder b)]) = _$ObjectTwo;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ObjectTwoBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ObjectTwo> get serializer => _$ObjectTwoSerializer();
}

class _$ObjectTwoSerializer implements PrimitiveSerializer<ObjectTwo> {
  @override
  final Iterable<Type> types = const [ObjectTwo, _$ObjectTwo];

  @override
  final String wireName = r'ObjectTwo';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ObjectTwo object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.attribute != null) {
      yield r'attribute';
      yield serializers.serialize(
        object.attribute,
        specifiedType: const FullType(BuiltSet, [FullType(ObjectTwoAttributeEnum)]),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ObjectTwo object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ObjectTwoBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'attribute':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltSet, [FullType(ObjectTwoAttributeEnum)]),
          ) as BuiltSet<ObjectTwoAttributeEnum>;
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
  ObjectTwo deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ObjectTwoBuilder();
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

class ObjectTwoAttributeEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'valueone')
  static const ObjectTwoAttributeEnum valueone = _$objectTwoAttributeEnum_valueone;
  @BuiltValueEnumConst(wireName: r'vauetwo')
  static const ObjectTwoAttributeEnum vauetwo = _$objectTwoAttributeEnum_vauetwo;

  static Serializer<ObjectTwoAttributeEnum> get serializer => _$objectTwoAttributeEnumSerializer;

  const ObjectTwoAttributeEnum._(String name): super(name);

  static BuiltSet<ObjectTwoAttributeEnum> get values => _$objectTwoAttributeEnumValues;
  static ObjectTwoAttributeEnum valueOf(String name) => _$objectTwoAttributeEnumValueOf(name);
}

