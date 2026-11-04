//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'map_test.freezed.dart';
part 'map_test.g.dart';


@freezed
abstract class MapTest with _$MapTest {
  const factory MapTest({
    @JsonKey(
      name: r'map_map_of_string',
      required: false,
      includeIfNull: false,
    )
    Map<String, Map<String, String>>? mapMapOfString,
    @JsonKey(
      name: r'map_of_enum_string',
      required: false,
      includeIfNull: false,
    )
    Map<String, MapTestMapOfEnumStringEnum>? mapOfEnumString,
    @JsonKey(
      name: r'direct_map',
      required: false,
      includeIfNull: false,
    )
    Map<String, bool>? directMap,
    @JsonKey(
      name: r'indirect_map',
      required: false,
      includeIfNull: false,
    )
    Map<String, bool>? indirectMap,
  }) = _MapTest;

  factory MapTest.fromJson(Map<String, dynamic> json) =>
      _$MapTestFromJson(json);
}

enum MapTestMapOfEnumStringEnum {
@JsonValue(r'UPPER')
UPPER(r'UPPER'),
@JsonValue(r'lower')
lower(r'lower'),
@JsonValue(r'unknown_default_open_api')
unknownDefaultOpenApi(r'unknown_default_open_api');

const MapTestMapOfEnumStringEnum(this.value);

final String value;

@override
String toString() => value;
}


