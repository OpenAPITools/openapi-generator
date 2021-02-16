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

    // Boilerplate code needed to wire-up generated code
    MapTest._();

    static void _initializeBuilder(MapTestBuilder b) => b;

    factory MapTest([void updates(MapTestBuilder b)]) = _$MapTest;
    static Serializer<MapTest> get serializer => _$mapTestSerializer;
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

