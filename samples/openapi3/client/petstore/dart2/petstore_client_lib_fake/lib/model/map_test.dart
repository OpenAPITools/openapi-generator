//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class MapTest {
  /// Returns a new [MapTest] instance.
  MapTest({
    this.mapMapOfString = const {},
    this.mapOfEnumString = const {},
    this.directMap = const {},
    this.indirectMap = const {},
  });

  Map<String, Map<String, String>> mapMapOfString;

  Map<String, MapTestInnerEnum> mapOfEnumString;

  Map<String, bool> directMap;

  Map<String, bool> indirectMap;

  @override
  bool operator ==(Object other) => identical(this, other) || other is MapTest &&
     other.mapMapOfString == mapMapOfString &&
     other.mapOfEnumString == mapOfEnumString &&
     other.directMap == directMap &&
     other.indirectMap == indirectMap;

  @override
  int get hashCode =>
    (mapMapOfString == null ? 0 : mapMapOfString.hashCode) +
    (mapOfEnumString == null ? 0 : mapOfEnumString.hashCode) +
    (directMap == null ? 0 : directMap.hashCode) +
    (indirectMap == null ? 0 : indirectMap.hashCode);

  @override
  String toString() => 'MapTest[mapMapOfString=$mapMapOfString, mapOfEnumString=$mapOfEnumString, directMap=$directMap, indirectMap=$indirectMap]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (mapMapOfString != null) {
      json[r'map_map_of_string'] = mapMapOfString;
    }
    if (mapOfEnumString != null) {
      json[r'map_of_enum_string'] = mapOfEnumString;
    }
    if (directMap != null) {
      json[r'direct_map'] = directMap;
    }
    if (indirectMap != null) {
      json[r'indirect_map'] = indirectMap;
    }
    return json;
  }

  /// Returns a new [MapTest] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static MapTest fromJson(Map<String, dynamic> json) => json == null
    ? null
    : MapTest(
        mapMapOfString: json[r'map_map_of_string'] == null
          ? null
          : Map.mapFromJson(json[r'map_map_of_string']),
        mapOfEnumString: json[r'map_of_enum_string'] == null ?
          null :
        (json[r'map_of_enum_string'] as Map<String, MapTestInnerEnum>),
        directMap: json[r'direct_map'] == null ?
          null :
        (json[r'direct_map'] as Map).cast<String, bool>(),
        indirectMap: json[r'indirect_map'] == null ?
          null :
        (json[r'indirect_map'] as Map).cast<String, bool>(),
    );

  static List<MapTest> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <MapTest>[]
      : json.map((v) => MapTest.fromJson(v)).toList(growable: true == growable);

  static Map<String, MapTest> mapFromJson(Map<String, dynamic> json) {
    final map = <String, MapTest>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = MapTest.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of MapTest-objects as value to a dart map
  static Map<String, List<MapTest>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<MapTest>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = MapTest.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}


enum MapTestInnerEnum {
        UPPER,
        lower,
}

const _$MapTestInnerEnum = <MapTestInnerEnum, dynamic>{
        MapTestInnerEnum.UPPER: 'UPPER',
        MapTestInnerEnum.lower: 'lower',
};


