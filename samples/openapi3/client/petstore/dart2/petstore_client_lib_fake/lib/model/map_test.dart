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
          (json[r'map_of_enum_string'] as Map).cast<String, String>(),
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


class MapTestInnerEnum {
  /// Instantiate a new enum with the provided [value].
  const MapTestInnerEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  bool operator ==(Object other) => identical(this, other) ||
      other is MapTestInnerEnum && other.value == value;

  @override
  int get hashCode => toString().hashCode;

  @override
  String toString() => value;

  String toJson() => value;

  static const UPPER = MapTestInnerEnum._(r'UPPER');
  static const lower = MapTestInnerEnum._(r'lower');

  /// List of all possible values in this [enum][MapTestInnerEnum].
  static const values = <MapTestInnerEnum>[
    UPPER,
    lower,
  ];

  static MapTestInnerEnum fromJson(dynamic value) =>
    MapTestInnerEnumTypeTransformer().decode(value);

  static List<MapTestInnerEnum> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <MapTestInnerEnum>[]
      : json
          .map((value) => MapTestInnerEnum.fromJson(value))
          .toList(growable: true == growable);
}

/// Transformation class that can [encode] an instance of [MapTestInnerEnum] to String,
/// and [decode] dynamic data back to [MapTestInnerEnum].
class MapTestInnerEnumTypeTransformer {
  const MapTestInnerEnumTypeTransformer._();

  factory MapTestInnerEnumTypeTransformer() => _instance ??= MapTestInnerEnumTypeTransformer._();

  String encode(MapTestInnerEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a MapTestInnerEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  MapTestInnerEnum decode(dynamic data, {bool allowNull}) {
    switch (data) {
      case r'UPPER': return MapTestInnerEnum.UPPER;
      case r'lower': return MapTestInnerEnum.lower;
      default:
        if (allowNull == false) {
          throw ArgumentError('Unknown enum value to decode: $data');
        }
    }
    return null;
  }

  /// Singleton [MapTestInnerEnumTypeTransformer] instance.
  static MapTestInnerEnumTypeTransformer _instance;
}

