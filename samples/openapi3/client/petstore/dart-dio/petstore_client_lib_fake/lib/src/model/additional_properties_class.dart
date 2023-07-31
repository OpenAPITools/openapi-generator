//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'additional_properties_class.g.dart';

/// AdditionalPropertiesClass
///
/// Properties:
/// * [mapProperty] 
/// * [mapOfMapProperty] 
@BuiltValue()
abstract class AdditionalPropertiesClass implements Built<AdditionalPropertiesClass, AdditionalPropertiesClassBuilder> {
  @BuiltValueField(wireName: r'map_property')
  BuiltMap<String, String>? get mapProperty;

  @BuiltValueField(wireName: r'map_of_map_property')
  BuiltMap<String, BuiltMap<String, String>>? get mapOfMapProperty;

  AdditionalPropertiesClass._();

  factory AdditionalPropertiesClass([void updates(AdditionalPropertiesClassBuilder b)]) = _$AdditionalPropertiesClass;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(AdditionalPropertiesClassBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<AdditionalPropertiesClass> get serializer => _$AdditionalPropertiesClassSerializer();
}

class _$AdditionalPropertiesClassSerializer implements PrimitiveSerializer<AdditionalPropertiesClass> {
  @override
  final Iterable<Type> types = const [AdditionalPropertiesClass, _$AdditionalPropertiesClass];

  @override
  final String wireName = r'AdditionalPropertiesClass';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    AdditionalPropertiesClass object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.mapProperty != null) {
      yield r'map_property';
      yield serializers.serialize(
        object.mapProperty,
        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(String)]),
      );
    }
    if (object.mapOfMapProperty != null) {
      yield r'map_of_map_property';
      yield serializers.serialize(
        object.mapOfMapProperty,
        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(BuiltMap, [FullType(String), FullType(String)])]),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    AdditionalPropertiesClass object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required AdditionalPropertiesClassBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'map_property':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltMap, [FullType(String), FullType(String)]),
          ) as BuiltMap<String, String>;
          result.mapProperty.replace(valueDes);
          break;
        case r'map_of_map_property':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltMap, [FullType(String), FullType(BuiltMap, [FullType(String), FullType(String)])]),
          ) as BuiltMap<String, BuiltMap<String, String>>;
          result.mapOfMapProperty.replace(valueDes);
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  AdditionalPropertiesClass deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = AdditionalPropertiesClassBuilder();
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

