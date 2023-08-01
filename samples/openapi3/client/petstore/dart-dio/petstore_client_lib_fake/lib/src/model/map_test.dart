//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'map_test.g.dart';

/// MapTest
///
/// Properties:
/// * [mapMapOfString] 
/// * [mapOfEnumString] 
/// * [directMap] 
/// * [indirectMap] 
@BuiltValue()
abstract class MapTest implements Built<MapTest, MapTestBuilder> {
  @BuiltValueField(wireName: r'map_map_of_string')
  BuiltMap<String, BuiltMap<String, String>>? get mapMapOfString;

  @BuiltValueField(wireName: r'map_of_enum_string')
  BuiltMap<String, MapTestMapOfEnumStringEnum>? get mapOfEnumString;
  // enum mapOfEnumStringEnum {  UPPER,  lower,  };

  @BuiltValueField(wireName: r'direct_map')
  BuiltMap<String, bool>? get directMap;

  @BuiltValueField(wireName: r'indirect_map')
  BuiltMap<String, bool>? get indirectMap;

  MapTest._();

  factory MapTest([void updates(MapTestBuilder b)]) = _$MapTest;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(MapTestBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<MapTest> get serializer => _$MapTestSerializer();
}

class _$MapTestSerializer implements PrimitiveSerializer<MapTest> {
  @override
  final Iterable<Type> types = const [MapTest, _$MapTest];

  @override
  final String wireName = r'MapTest';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    MapTest object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.mapMapOfString != null) {
      yield r'map_map_of_string';
      yield serializers.serialize(
        object.mapMapOfString,
        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(BuiltMap, [FullType(String), FullType(String)])]),
      );
    }
    if (object.mapOfEnumString != null) {
      yield r'map_of_enum_string';
      yield serializers.serialize(
        object.mapOfEnumString,
        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(MapTestMapOfEnumStringEnum)]),
      );
    }
    if (object.directMap != null) {
      yield r'direct_map';
      yield serializers.serialize(
        object.directMap,
        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(bool)]),
      );
    }
    if (object.indirectMap != null) {
      yield r'indirect_map';
      yield serializers.serialize(
        object.indirectMap,
        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(bool)]),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    MapTest object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required MapTestBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'map_map_of_string':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltMap, [FullType(String), FullType(BuiltMap, [FullType(String), FullType(String)])]),
          ) as BuiltMap<String, BuiltMap<String, String>>;
          result.mapMapOfString.replace(valueDes);
          break;
        case r'map_of_enum_string':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltMap, [FullType(String), FullType(MapTestMapOfEnumStringEnum)]),
          ) as BuiltMap<String, MapTestMapOfEnumStringEnum>;
          result.mapOfEnumString.replace(valueDes);
          break;
        case r'direct_map':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltMap, [FullType(String), FullType(bool)]),
          ) as BuiltMap<String, bool>;
          result.directMap.replace(valueDes);
          break;
        case r'indirect_map':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltMap, [FullType(String), FullType(bool)]),
          ) as BuiltMap<String, bool>;
          result.indirectMap.replace(valueDes);
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  MapTest deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = MapTestBuilder();
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

class MapTestMapOfEnumStringEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'UPPER')
  static const MapTestMapOfEnumStringEnum UPPER = _$mapTestMapOfEnumStringEnum_UPPER;
  @BuiltValueEnumConst(wireName: r'lower')
  static const MapTestMapOfEnumStringEnum lower = _$mapTestMapOfEnumStringEnum_lower;
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const MapTestMapOfEnumStringEnum unknownDefaultOpenApi = _$mapTestMapOfEnumStringEnum_unknownDefaultOpenApi;

  static Serializer<MapTestMapOfEnumStringEnum> get serializer => _$mapTestMapOfEnumStringEnumSerializer;

  const MapTestMapOfEnumStringEnum._(String name): super(name);

  static BuiltSet<MapTestMapOfEnumStringEnum> get values => _$mapTestMapOfEnumStringEnumValues;
  static MapTestMapOfEnumStringEnum valueOf(String name) => _$mapTestMapOfEnumStringEnumValueOf(name);
}

