//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:openapi/src/model/parent_with_nullable.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'child_with_nullable.g.dart';

/// ChildWithNullable
///
/// Properties:
/// * [type] 
/// * [nullableProperty] 
/// * [otherProperty] 
@BuiltValue()
abstract class ChildWithNullable implements ParentWithNullable, Built<ChildWithNullable, ChildWithNullableBuilder> {
  @BuiltValueField(wireName: r'otherProperty')
  String? get otherProperty;

  ChildWithNullable._();

  factory ChildWithNullable([void updates(ChildWithNullableBuilder b)]) = _$ChildWithNullable;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ChildWithNullableBuilder b) => b..type=b.discriminatorValue;

  @BuiltValueSerializer(custom: true)
  static Serializer<ChildWithNullable> get serializer => _$ChildWithNullableSerializer();
}

class _$ChildWithNullableSerializer implements PrimitiveSerializer<ChildWithNullable> {
  @override
  final Iterable<Type> types = const [ChildWithNullable, _$ChildWithNullable];

  @override
  final String wireName = r'ChildWithNullable';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ChildWithNullable object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.type != null) {
      yield r'type';
      yield serializers.serialize(
        object.type,
        specifiedType: const FullType(ParentWithNullableTypeEnum),
      );
    }
    if (object.nullableProperty != null) {
      yield r'nullableProperty';
      yield serializers.serialize(
        object.nullableProperty,
        specifiedType: const FullType.nullable(String),
      );
    }
    if (object.otherProperty != null) {
      yield r'otherProperty';
      yield serializers.serialize(
        object.otherProperty,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ChildWithNullable object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ChildWithNullableBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'type':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(ParentWithNullableTypeEnum),
          ) as ParentWithNullableTypeEnum;
          result.type = valueDes;
          break;
        case r'nullableProperty':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType.nullable(String),
          ) as String?;
          if (valueDes == null) continue;
          result.nullableProperty = valueDes;
          break;
        case r'otherProperty':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.otherProperty = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  ChildWithNullable deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ChildWithNullableBuilder();
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

class ChildWithNullableTypeEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'ChildWithNullable')
  static const ChildWithNullableTypeEnum childWithNullable = _$childWithNullableTypeEnum_childWithNullable;
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const ChildWithNullableTypeEnum unknownDefaultOpenApi = _$childWithNullableTypeEnum_unknownDefaultOpenApi;

  static Serializer<ChildWithNullableTypeEnum> get serializer => _$childWithNullableTypeEnumSerializer;

  const ChildWithNullableTypeEnum._(String name): super(name);

  static BuiltSet<ChildWithNullableTypeEnum> get values => _$childWithNullableTypeEnumValues;
  static ChildWithNullableTypeEnum valueOf(String name) => _$childWithNullableTypeEnumValueOf(name);
}

