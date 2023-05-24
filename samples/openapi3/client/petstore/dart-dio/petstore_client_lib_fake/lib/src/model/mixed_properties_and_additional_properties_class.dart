//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:openapi/src/model/animal.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'mixed_properties_and_additional_properties_class.g.dart';

/// MixedPropertiesAndAdditionalPropertiesClass
///
/// Properties:
/// * [uuid] 
/// * [dateTime] 
/// * [map] 
@BuiltValue()
abstract class MixedPropertiesAndAdditionalPropertiesClass implements Built<MixedPropertiesAndAdditionalPropertiesClass, MixedPropertiesAndAdditionalPropertiesClassBuilder> {
  @BuiltValueField(wireName: r'uuid')
  String? get uuid;

  @BuiltValueField(wireName: r'dateTime')
  DateTime? get dateTime;

  @BuiltValueField(wireName: r'map')
  BuiltMap<String, Animal>? get map;

  MixedPropertiesAndAdditionalPropertiesClass._();

  factory MixedPropertiesAndAdditionalPropertiesClass([void updates(MixedPropertiesAndAdditionalPropertiesClassBuilder b)]) = _$MixedPropertiesAndAdditionalPropertiesClass;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(MixedPropertiesAndAdditionalPropertiesClassBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<MixedPropertiesAndAdditionalPropertiesClass> get serializer => _$MixedPropertiesAndAdditionalPropertiesClassSerializer();
}

class _$MixedPropertiesAndAdditionalPropertiesClassSerializer implements PrimitiveSerializer<MixedPropertiesAndAdditionalPropertiesClass> {
  @override
  final Iterable<Type> types = const [MixedPropertiesAndAdditionalPropertiesClass, _$MixedPropertiesAndAdditionalPropertiesClass];

  @override
  final String wireName = r'MixedPropertiesAndAdditionalPropertiesClass';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    MixedPropertiesAndAdditionalPropertiesClass object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.uuid != null) {
      yield r'uuid';
      yield serializers.serialize(
        object.uuid,
        specifiedType: const FullType(String),
      );
    }
    if (object.dateTime != null) {
      yield r'dateTime';
      yield serializers.serialize(
        object.dateTime,
        specifiedType: const FullType(DateTime),
      );
    }
    if (object.map != null) {
      yield r'map';
      yield serializers.serialize(
        object.map,
        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(Animal)]),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    MixedPropertiesAndAdditionalPropertiesClass object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required MixedPropertiesAndAdditionalPropertiesClassBuilder result,
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
        case r'dateTime':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(DateTime),
          ) as DateTime;
          result.dateTime = valueDes;
          break;
        case r'map':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltMap, [FullType(String), FullType(Animal)]),
          ) as BuiltMap<String, Animal>;
          result.map.replace(valueDes);
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  MixedPropertiesAndAdditionalPropertiesClass deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = MixedPropertiesAndAdditionalPropertiesClassBuilder();
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

