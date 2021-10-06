//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: true,
  explicitToJson: true,
)
class EnumTest {
  /// Returns a new [EnumTest] instance.
  EnumTest({
    this.enumString,
    required this.enumStringRequired,
    this.enumInteger,
    this.enumNumber,
    this.outerEnum,
    this.outerEnumInteger,
    this.outerEnumDefaultValue,
    this.outerEnumIntegerDefaultValue,
  });


  @JsonKey(
    name: r'enum_string',
    required: false,
  )
  EnumTestEnumStringEnum? enumString;

  @JsonKey(
    name: r'enum_string_required',
    required: true,
  )
  EnumTestEnumStringRequiredEnum enumStringRequired;

  @JsonKey(
    name: r'enum_integer',
    required: false,
  )
  EnumTestEnumIntegerEnum? enumInteger;

  @JsonKey(
    name: r'enum_number',
    required: false,
  )
  EnumTestEnumNumberEnum? enumNumber;

  @JsonKey(
    name: r'outerEnum',
    required: false,
  )
  OuterEnum? outerEnum;

  @JsonKey(
    name: r'outerEnumInteger',
    required: false,
  )
  OuterEnumInteger? outerEnumInteger;

  @JsonKey(
    name: r'outerEnumDefaultValue',
    required: false,
  )
  OuterEnumDefaultValue? outerEnumDefaultValue;

  @JsonKey(
    name: r'outerEnumIntegerDefaultValue',
    required: false,
  )
  OuterEnumIntegerDefaultValue? outerEnumIntegerDefaultValue;

  @override
  bool operator ==(Object other) => identical(this, other) || other is EnumTest &&
     other.enumString == enumString &&
     other.enumStringRequired == enumStringRequired &&
     other.enumInteger == enumInteger &&
     other.enumNumber == enumNumber &&
     other.outerEnum == outerEnum &&
     other.outerEnumInteger == outerEnumInteger &&
     other.outerEnumDefaultValue == outerEnumDefaultValue &&
     other.outerEnumIntegerDefaultValue == outerEnumIntegerDefaultValue;

  @override
  int get hashCode =>
    enumString.hashCode +
    enumStringRequired.hashCode +
    enumInteger.hashCode +
    enumNumber.hashCode +
    outerEnum.hashCode +
    outerEnumInteger.hashCode +
    outerEnumDefaultValue.hashCode +
    outerEnumIntegerDefaultValue.hashCode;

  factory EnumTest.fromJson(Map<String, dynamic> json) => _$EnumTestFromJson(json);

  Map<String, dynamic> toJson() => _$EnumTestToJson(this);

  @override
  String toString() => toJson().toString();
}


enum EnumTestEnumStringEnum {
  UPPER,
  lower,
  empty,
}


enum EnumTestEnumStringRequiredEnum {
  UPPER,
  lower,
  empty,
}


enum EnumTestEnumIntegerEnum {
  number1,
  numberNegative1,
}


enum EnumTestEnumNumberEnum {
  number1Period1,
  numberNegative1Period2,
}

