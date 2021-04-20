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
class EnumTest {
  /// Returns a new [EnumTest] instance.
  EnumTest({
    this.enumString,
    @required this.enumStringRequired,
    this.enumInteger,
    this.enumNumber,
    this.outerEnum,
    this.outerEnumInteger,
    this.outerEnumDefaultValue,
    this.outerEnumIntegerDefaultValue,
  });

  @JsonKey(
    nullable: false,
    name: r'enum_string',
    required: false,
  )
  EnumTestEnumStringEnum enumString;

  @JsonKey(
    nullable: false,
    name: r'enum_string_required',
    required: true,
  )
  EnumTestEnumStringRequiredEnum enumStringRequired;

  @JsonKey(
    nullable: false,
    name: r'enum_integer',
    required: false,
  )
  EnumTestEnumIntegerEnum enumInteger;

  @JsonKey(
    nullable: false,
    name: r'enum_number',
    required: false,
  )
  EnumTestEnumNumberEnum enumNumber;

  @JsonKey(
    nullable: true,
    name: r'outerEnum',
    required: false,
  )
  OuterEnum outerEnum;

  @JsonKey(
    nullable: false,
    name: r'outerEnumInteger',
    required: false,
  )
  OuterEnumInteger outerEnumInteger;

  @JsonKey(
    nullable: false,
    name: r'outerEnumDefaultValue',
    required: false,
  )
  OuterEnumDefaultValue outerEnumDefaultValue;

  @JsonKey(
    nullable: false,
    name: r'outerEnumIntegerDefaultValue',
    required: false,
  )
  OuterEnumIntegerDefaultValue outerEnumIntegerDefaultValue;

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
    (enumString == null ? 0 : enumString.hashCode) +
    (enumStringRequired == null ? 0 : enumStringRequired.hashCode) +
    (enumInteger == null ? 0 : enumInteger.hashCode) +
    (enumNumber == null ? 0 : enumNumber.hashCode) +
    (outerEnum == null ? 0 : outerEnum.hashCode) +
    (outerEnumInteger == null ? 0 : outerEnumInteger.hashCode) +
    (outerEnumDefaultValue == null ? 0 : outerEnumDefaultValue.hashCode) +
    (outerEnumIntegerDefaultValue == null ? 0 : outerEnumIntegerDefaultValue.hashCode);

  factory EnumTest.fromJson(Map<String, dynamic> json) => _$EnumTestFromJson(json);

  Map<String, dynamic> toJson() => _$EnumTestToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

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

