//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'enum_arrays.freezed.dart';
part 'enum_arrays.g.dart';


@freezed
abstract class EnumArrays with _$EnumArrays {
  const factory EnumArrays({
    @JsonKey(
      name: r'just_symbol',
      required: false,
      includeIfNull: false,
      unknownEnumValue: EnumArraysJustSymbolEnum.unknownDefaultOpenApi,
    )
    EnumArraysJustSymbolEnum? justSymbol,
    @JsonKey(
      name: r'array_enum',
      required: false,
      includeIfNull: false,
      unknownEnumValue: EnumArraysArrayEnumEnum.unknownDefaultOpenApi,
    )
    List<EnumArraysArrayEnumEnum>? arrayEnum,
  }) = _EnumArrays;

  factory EnumArrays.fromJson(Map<String, dynamic> json) =>
      _$EnumArraysFromJson(json);
}

enum EnumArraysJustSymbolEnum {
@JsonValue(r'>=')
greaterThanEqual(r'>='),
@JsonValue(r'$')
dollar(r'$'),
@JsonValue(r'unknown_default_open_api')
unknownDefaultOpenApi(r'unknown_default_open_api');

const EnumArraysJustSymbolEnum(this.value);

final String value;

@override
String toString() => value;
}


enum EnumArraysArrayEnumEnum {
@JsonValue(r'fish')
fish(r'fish'),
@JsonValue(r'crab')
crab(r'crab'),
@JsonValue(r'unknown_default_open_api')
unknownDefaultOpenApi(r'unknown_default_open_api');

const EnumArraysArrayEnumEnum(this.value);

final String value;

@override
String toString() => value;
}


