//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/outer_enum.dart';
import 'package:openapi/src/model/outer_enum_default_value.dart';
import 'package:openapi/src/model/outer_enum_integer.dart';
import 'package:openapi/src/model/outer_enum_integer_default_value.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'enum_test.freezed.dart';
part 'enum_test.g.dart';


@freezed
abstract class EnumTest with _$EnumTest {
  const factory EnumTest({
    @JsonKey(
      name: r'enum_string',
      required: false,
      includeIfNull: false,
      unknownEnumValue: EnumTestEnumStringEnum.unknownDefaultOpenApi,
    )
    EnumTestEnumStringEnum? enumString,
    @JsonKey(
      name: r'enum_string_required',
      required: true,
      includeIfNull: false,
      unknownEnumValue: EnumTestEnumStringRequiredEnum.unknownDefaultOpenApi,
    )
    required EnumTestEnumStringRequiredEnum enumStringRequired,
    @JsonKey(
      name: r'enum_integer',
      required: false,
      includeIfNull: false,
      unknownEnumValue: EnumTestEnumIntegerEnum.unknownDefaultOpenApi,
    )
    EnumTestEnumIntegerEnum? enumInteger,
    @JsonKey(
      name: r'enum_number',
      required: false,
      includeIfNull: false,
      unknownEnumValue: EnumTestEnumNumberEnum.unknownDefaultOpenApi,
    )
    EnumTestEnumNumberEnum? enumNumber,
    @JsonKey(
      name: r'outerEnum',
      required: false,
      includeIfNull: false,
      unknownEnumValue: OuterEnum.unknownDefaultOpenApi,
    )
    OuterEnum? outerEnum,
    @JsonKey(
      name: r'outerEnumInteger',
      required: false,
      includeIfNull: false,
      unknownEnumValue: OuterEnumInteger.unknownDefaultOpenApi,
    )
    OuterEnumInteger? outerEnumInteger,
    @JsonKey(
      name: r'outerEnumDefaultValue',
      required: false,
      includeIfNull: false,
      unknownEnumValue: OuterEnumDefaultValue.unknownDefaultOpenApi,
    )
    OuterEnumDefaultValue? outerEnumDefaultValue,
    @JsonKey(
      name: r'outerEnumIntegerDefaultValue',
      required: false,
      includeIfNull: false,
      unknownEnumValue: OuterEnumIntegerDefaultValue.unknownDefaultOpenApi,
    )
    OuterEnumIntegerDefaultValue? outerEnumIntegerDefaultValue,
  }) = _EnumTest;

  factory EnumTest.fromJson(Map<String, dynamic> json) =>
      _$EnumTestFromJson(json);
}

enum EnumTestEnumStringEnum {
@JsonValue(r'UPPER')
UPPER(r'UPPER'),
@JsonValue(r'lower')
lower(r'lower'),
@JsonValue(r'')
empty(r''),
@JsonValue(r'unknown_default_open_api')
unknownDefaultOpenApi(r'unknown_default_open_api');

const EnumTestEnumStringEnum(this.value);

final String value;

@override
String toString() => value;
}


enum EnumTestEnumStringRequiredEnum {
@JsonValue(r'UPPER')
UPPER(r'UPPER'),
@JsonValue(r'lower')
lower(r'lower'),
@JsonValue(r'')
empty(r''),
@JsonValue(r'unknown_default_open_api')
unknownDefaultOpenApi(r'unknown_default_open_api');

const EnumTestEnumStringRequiredEnum(this.value);

final String value;

@override
String toString() => value;
}


enum EnumTestEnumIntegerEnum {
@JsonValue(1)
number1('1'),
@JsonValue(-1)
numberNegative1('-1'),
@JsonValue(11184809)
unknownDefaultOpenApi('11184809');

const EnumTestEnumIntegerEnum(this.value);

final String value;

@override
String toString() => value;
}


enum EnumTestEnumNumberEnum {
@JsonValue('1.1')
number1Period1('1.1'),
@JsonValue('-1.2')
numberNegative1Period2('-1.2'),
@JsonValue('11184809')
unknownDefaultOpenApi('11184809');

const EnumTestEnumNumberEnum(this.value);

final String value;

@override
String toString() => value;
}


