//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'map_test.g.dart';

abstract class MapTest implements Built<MapTest, MapTestBuilder> {

    @nullable
    @BuiltValueField(wireName: r'map_map_of_string')
    BuiltMap<String, BuiltMap<String, String>> get mapMapOfString;

    @nullable
    @BuiltValueField(wireName: r'map_of_enum_string')
    BuiltMap<String, MapTestMapOfEnumStringEnum> get mapOfEnumString;
    // enum mapOfEnumStringEnum {  UPPER,  lower,  };

    @nullable
    @BuiltValueField(wireName: r'direct_map')
    BuiltMap<String, bool> get directMap;

    @nullable
    @BuiltValueField(wireName: r'indirect_map')
    BuiltMap<String, bool> get indirectMap;

    MapTest._();

    static void _initializeBuilder(MapTestBuilder b) => b;

    factory MapTest([void updates(MapTestBuilder b)]) = _$MapTest;

    @BuiltValueSerializer(custom: true)
    static Serializer<MapTest> get serializer => _$MapTestSerializer();
}

class _$MapTestSerializer implements StructuredSerializer<MapTest> {

    @override
    final Iterable<Type> types = const [MapTest, _$MapTest];
    @override
    final String wireName = r'MapTest';

    @override
    Iterable<Object> serialize(Serializers serializers, MapTest object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.mapMapOfString != null) {
            result
                ..add(r'map_map_of_string')
                ..add(serializers.serialize(object.mapMapOfString,
                    specifiedType: const FullType(BuiltMap, [FullType(String), FullType(BuiltMap, [FullType(String), FullType(String)])])));
        }
        if (object.mapOfEnumString != null) {
            result
                ..add(r'map_of_enum_string')
                ..add(serializers.serialize(object.mapOfEnumString,
                    specifiedType: const FullType(BuiltMap, [FullType(String), FullType(MapTestMapOfEnumStringEnum)])));
        }
        if (object.directMap != null) {
            result
                ..add(r'direct_map')
                ..add(serializers.serialize(object.directMap,
                    specifiedType: const FullType(BuiltMap, [FullType(String), FullType(bool)])));
        }
        if (object.indirectMap != null) {
            result
                ..add(r'indirect_map')
                ..add(serializers.serialize(object.indirectMap,
                    specifiedType: const FullType(BuiltMap, [FullType(String), FullType(bool)])));
        }
        return result;
    }

    @override
    MapTest deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = MapTestBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'map_map_of_string':
                    result.mapMapOfString.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(BuiltMap, [FullType(String), FullType(String)])])) as BuiltMap<String, BuiltMap<String, String>>);
                    break;
                case r'map_of_enum_string':
                    result.mapOfEnumString.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(MapTestMapOfEnumStringEnum)])) as BuiltMap<String, MapTestMapOfEnumStringEnum>);
                    break;
                case r'direct_map':
                    result.directMap.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(bool)])) as BuiltMap<String, bool>);
                    break;
                case r'indirect_map':
                    result.indirectMap.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(bool)])) as BuiltMap<String, bool>);
                    break;
            }
        }
        return result.build();
    }
}

class MapTestMapOfEnumStringEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'UPPER')
  static const MapTestMapOfEnumStringEnum UPPER = _$mapTestMapOfEnumStringEnum_UPPER;
  @BuiltValueEnumConst(wireName: r'lower')
  static const MapTestMapOfEnumStringEnum lower = _$mapTestMapOfEnumStringEnum_lower;

  static Serializer<MapTestMapOfEnumStringEnum> get serializer => _$mapTestMapOfEnumStringEnumSerializer;

  const MapTestMapOfEnumStringEnum._(String name): super(name);

  static BuiltSet<MapTestMapOfEnumStringEnum> get values => _$mapTestMapOfEnumStringEnumValues;
  static MapTestMapOfEnumStringEnum valueOf(String name) => _$mapTestMapOfEnumStringEnumValueOf(name);
}

