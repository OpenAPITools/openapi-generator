//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/outer_enum_integer.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'outer_object_with_enum_property.g.dart';

/// OuterObjectWithEnumProperty
///
/// Properties:
/// * [value] 
@BuiltValue()
abstract class OuterObjectWithEnumProperty implements Built<OuterObjectWithEnumProperty, OuterObjectWithEnumPropertyBuilder> {
  @BuiltValueField(wireName: r'value')
  OuterEnumInteger get value;
  // enum valueEnum {  0,  1,  2,  };

  OuterObjectWithEnumProperty._();

  factory OuterObjectWithEnumProperty([void updates(OuterObjectWithEnumPropertyBuilder b)]) = _$OuterObjectWithEnumProperty;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(OuterObjectWithEnumPropertyBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<OuterObjectWithEnumProperty> get serializer => _$OuterObjectWithEnumPropertySerializer();
}

class _$OuterObjectWithEnumPropertySerializer implements PrimitiveSerializer<OuterObjectWithEnumProperty> {
  @override
  final Iterable<Type> types = const [OuterObjectWithEnumProperty, _$OuterObjectWithEnumProperty];

  @override
  final String wireName = r'OuterObjectWithEnumProperty';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    OuterObjectWithEnumProperty object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'value';
    yield serializers.serialize(
      object.value,
      specifiedType: const FullType(OuterEnumInteger),
    );
  }

  @override
  Object serialize(
    Serializers serializers,
    OuterObjectWithEnumProperty object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required OuterObjectWithEnumPropertyBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'value':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(OuterEnumInteger),
          ) as OuterEnumInteger;
          result.value = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  OuterObjectWithEnumProperty deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = OuterObjectWithEnumPropertyBuilder();
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

