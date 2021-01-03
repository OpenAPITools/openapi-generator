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
  includeIfNull: false,
  disallowUnrecognizedKeys: true,
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
    name: r'mapMapOfString',
    
    defaultValue: const {},
    
  )
  Map<String, Map<String, String>> mapMapOfString;

  @JsonKey(
    name: r'mapOfEnumString',
    
    defaultValue: const {},
    
  )
  Map<String, MapTestInnerEnum> mapOfEnumString;

  @JsonKey(
    name: r'directMap',
    
    defaultValue: const {},
    
  )
  Map<String, bool> directMap;

  @JsonKey(
    name: r'indirectMap',
    
    defaultValue: const {},
    
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

  @override
  String toString() => toJson().toString();

  factory MapTest.fromJson(Map<String, dynamic> json) => _$MapTestFromJson(json);
  Map<String, dynamic> toJson() => _$MapTestToJson(this);
}


enum MapTestInnerEnum {

    @JsonValue(r'UPPER')
    
    UPPER,
    @JsonValue(r'lower')
    
    lower,

}


