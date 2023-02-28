//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'map_test.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class MapTest {
  /// Returns a new [MapTest] instance.
  MapTest({

     this.mapMapOfString,

     this.mapOfEnumString,

     this.directMap,

     this.indirectMap,
  });

  @JsonKey(
    
    name: r'map_map_of_string',
    required: false,
    includeIfNull: false
  )


  final Map<String, Map<String, String>>? mapMapOfString;



  @JsonKey(
    
    name: r'map_of_enum_string',
    required: false,
    includeIfNull: false
  )


  final Map<String, MapTestMapOfEnumStringEnum>? mapOfEnumString;



  @JsonKey(
    
    name: r'direct_map',
    required: false,
    includeIfNull: false
  )


  final Map<String, bool>? directMap;



  @JsonKey(
    
    name: r'indirect_map',
    required: false,
    includeIfNull: false
  )


  final Map<String, bool>? indirectMap;



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

  factory MapTest.fromJson(Map<String, dynamic> json) => _$MapTestFromJson(json);

  Map<String, dynamic> toJson() => _$MapTestToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}


enum MapTestMapOfEnumStringEnum {
  @JsonValue(r'UPPER')
  UPPER,
  @JsonValue(r'lower')
  lower,
  @JsonValue(r'unknown_default_open_api')
  unknownDefaultOpenApi,
}


