//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: true,
  explicitToJson: true,
)
class MapTest {
  /// Returns a new [MapTest] instance.
  MapTest({
    this.mapMapOfString = const {},
    this.mapOfEnumString = const {},
    this.directMap = const {},
    this.indirectMap = const {},
  });

  @JsonKey(
    defaultValue: const {},
    name: r'map_map_of_string',
    required: false,
  )
  Map<String, Map<String, String>> mapMapOfString;

  @JsonKey(
    defaultValue: const {},
    name: r'map_of_enum_string',
    required: false,
  )
  Map<String, MapTestMapOfEnumStringEnum> mapOfEnumString;

  @JsonKey(
    defaultValue: const {},
    name: r'direct_map',
    required: false,
  )
  Map<String, bool> directMap;

  @JsonKey(
    defaultValue: const {},
    name: r'indirect_map',
    required: false,
  )
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

  factory MapTest.fromJson(Map<String, dynamic> json) => _$MapTestFromJson(json);

  Map<String, dynamic> toJson() => _$MapTestToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}


enum MapTestMapOfEnumStringEnum {
  UPPER,
  lower,
}

