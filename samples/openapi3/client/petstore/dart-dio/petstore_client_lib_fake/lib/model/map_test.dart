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
    MapTestMapOfEnumString get mapOfEnumString;
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

class MapTestMapOfEnumString extends EnumClass {

  @BuiltValueEnumConst(wireName: 'UPPER')
  static const MapTestMapOfEnumString uPPER = _$uPPER;
  @BuiltValueEnumConst(wireName: 'lower')
  static const MapTestMapOfEnumString lower = _$lower;

  static Serializer<MapTestMapOfEnumString> get serializer => _$mapTestMapOfEnumStringSerializer;

  const MapTestMapOfEnumString._(String name): super(name);

  static BuiltSet<MapTestMapOfEnumString> get values => _$mapTestMapOfEnumStringValues;
  static MapTestMapOfEnumString valueOf(String name) => _$mapTestMapOfEnumStringValueOf(name);
}


