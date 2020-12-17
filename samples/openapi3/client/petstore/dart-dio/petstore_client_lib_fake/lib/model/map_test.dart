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
    BuiltMap<String, MapTestInnerEnum> get mapOfEnumString;
    // enum mapOfEnumStringEnum {  UPPER,  lower,  };

    @nullable
    @BuiltValueField(wireName: r'direct_map')
    BuiltMap<String, bool> get directMap;

    @nullable
    @BuiltValueField(wireName: r'indirect_map')
    BuiltMap<String, bool> get indirectMap;

    // Boilerplate code needed to wire-up generated code
    MapTest._();

    factory MapTest([updates(MapTestBuilder b)]) = _$MapTest;
    static Serializer<MapTest> get serializer => _$mapTestSerializer;
}

class MapTestInnerEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'UPPER')
  static const MapTestInnerEnum UPPER = _$mapTestInnerEnum_UPPER;
  @BuiltValueEnumConst(wireName: r'lower')
  static const MapTestInnerEnum lower = _$mapTestInnerEnum_lower;

  static Serializer<MapTestInnerEnum> get serializer => _$mapTestInnerEnumSerializer;

  const MapTestInnerEnum._(String name): super(name);

  static BuiltSet<MapTestInnerEnum> get values => _$mapTestInnerEnumValues;
  static MapTestInnerEnum valueOf(String name) => _$mapTestInnerEnumValueOf(name);
}

