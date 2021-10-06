//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
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


  Map<String, Map<String, String>>? mapMapOfString;

  Map<String, MapTestMapOfEnumStringEnum>? mapOfEnumString;

  Map<String, bool>? directMap;

  Map<String, bool>? indirectMap;

  @override
  bool operator ==(Object other) => identical(this, other) || other is MapTest &&
     other.mapMapOfString == mapMapOfString &&
     other.mapOfEnumString == mapOfEnumString &&
     other.directMap == directMap &&
     other.indirectMap == indirectMap;

  @override
  int get hashCode =>
    mapMapOfString.hashCode +
    mapOfEnumString.hashCode +
    directMap.hashCode +
    indirectMap.hashCode;

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
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static MapTest fromJson(Map<String, dynamic> json) => MapTest(
        mapMapOfString: json[r'map_map_of_string'] as Map<String, Map<String, String>>,
        mapOfEnumString: json[r'map_of_enum_string'] as Map<String, MapTestMapOfEnumStringEnum>,
        directMap: json[r'direct_map'] as Map<String, bool>,
        indirectMap: json[r'indirect_map'] as Map<String, bool>,
    );

  static List<MapTest> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<MapTest>((i) => MapTest.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <MapTest>[];

  static Map<String, MapTest> mapFromJson(dynamic json) {
    final map = <String, MapTest>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = MapTest.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of MapTest-objects as value to a dart map
  static Map<String, List<MapTest>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<MapTest>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = MapTest.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}


class MapTestMapOfEnumStringEnum {
  /// Instantiate a new enum with the provided [value].
  const MapTestMapOfEnumStringEnum._(this.value);

  /// The underlying value of this enum member.
  final String? value;

  @override
  String toString() => value ?? '';

  String? toJson() => value;

  static const UPPER = MapTestMapOfEnumStringEnum._(r'UPPER');
  static const lower = MapTestMapOfEnumStringEnum._(r'lower');

  /// List of all possible values in this [enum][MapTestMapOfEnumStringEnum].
  static const values = <MapTestMapOfEnumStringEnum>[
    UPPER,
    lower,
  ];

  static MapTestMapOfEnumStringEnum fromJson(dynamic value) =>
    MapTestMapOfEnumStringEnumTypeTransformer().decode(value);

  static List<MapTestMapOfEnumStringEnum> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<MapTestMapOfEnumStringEnum>((i) => MapTestMapOfEnumStringEnum.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <MapTestMapOfEnumStringEnum>[];
}

/// Transformation class that can [encode] an instance of [MapTestMapOfEnumStringEnum] to String,
/// and [decode] dynamic data back to [MapTestMapOfEnumStringEnum].
class MapTestMapOfEnumStringEnumTypeTransformer {
  factory MapTestMapOfEnumStringEnumTypeTransformer() => _instance ??= const MapTestMapOfEnumStringEnumTypeTransformer._();

  const MapTestMapOfEnumStringEnumTypeTransformer._();

  String? encode(MapTestMapOfEnumStringEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a MapTestMapOfEnumStringEnum.
  ///
  /// If the [dynamic value][data] cannot be decoded successfully, then an [UnimplementedError] is thrown.
  MapTestMapOfEnumStringEnum decode(dynamic data) {
    if (data == r'UPPER') {
      return MapTestMapOfEnumStringEnum.UPPER;
    }
    if (data == r'lower') {
      return MapTestMapOfEnumStringEnum.lower;
    }
    throw ArgumentError('Unknown enum value to decode: $data');
  }

  /// Singleton [MapTestMapOfEnumStringEnumTypeTransformer] instance.
  static MapTestMapOfEnumStringEnumTypeTransformer? _instance;
}


