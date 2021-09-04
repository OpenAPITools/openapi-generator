//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

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

  Map<String, Map<String, String>> mapMapOfString;

  Map<String, MapTestMapOfEnumStringEnum> mapOfEnumString;

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
  // ignore: unnecessary_parenthesis
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
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static MapTest fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return MapTest(
        mapMapOfString: mapCastOfType<String, dynamic>(json, r'map_map_of_string'),
        mapOfEnumString: mapCastOfType<String, String>(json, r'map_of_enum_string'),
        directMap: mapCastOfType<String, bool>(json, r'direct_map'),
        indirectMap: mapCastOfType<String, bool>(json, r'indirect_map'),
      );
    }
    return null;
  }

  static List<MapTest> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(MapTest.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <MapTest>[];

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
  static Map<String, List<MapTest>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<MapTest>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = MapTest.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
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
  final String value;

  @override
  String toString() => value ?? '';

  String toJson() => value;

  static const UPPER = MapTestMapOfEnumStringEnum._(r'UPPER');
  static const lower = MapTestMapOfEnumStringEnum._(r'lower');

  /// List of all possible values in this [enum][MapTestMapOfEnumStringEnum].
  static const values = <MapTestMapOfEnumStringEnum>[
    UPPER,
    lower,
  ];

  static MapTestMapOfEnumStringEnum fromJson(dynamic value) =>
    MapTestMapOfEnumStringEnumTypeTransformer().decode(value);

  static List<MapTestMapOfEnumStringEnum> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(MapTestMapOfEnumStringEnum.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <MapTestMapOfEnumStringEnum>[];
}

/// Transformation class that can [encode] an instance of [MapTestMapOfEnumStringEnum] to String,
/// and [decode] dynamic data back to [MapTestMapOfEnumStringEnum].
class MapTestMapOfEnumStringEnumTypeTransformer {
  factory MapTestMapOfEnumStringEnumTypeTransformer() => _instance ??= const MapTestMapOfEnumStringEnumTypeTransformer._();

  const MapTestMapOfEnumStringEnumTypeTransformer._();

  String encode(MapTestMapOfEnumStringEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a MapTestMapOfEnumStringEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  MapTestMapOfEnumStringEnum decode(dynamic data, {bool allowNull}) {
    if (data != null) {
      switch (data.toString()) {
        case r'UPPER': return MapTestMapOfEnumStringEnum.UPPER;
        case r'lower': return MapTestMapOfEnumStringEnum.lower;
        default:
          if (allowNull == false) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [MapTestMapOfEnumStringEnumTypeTransformer] instance.
  static MapTestMapOfEnumStringEnumTypeTransformer _instance;
}


