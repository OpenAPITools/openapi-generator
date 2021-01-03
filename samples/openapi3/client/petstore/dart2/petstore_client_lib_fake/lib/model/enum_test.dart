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
    name: r'enumString',
    
    
    
  )
  EnumTestEnumStringEnum enumString;

  @JsonKey(
    name: r'enumStringRequired',
    required: true,
    
    
  )
  EnumTestEnumStringRequiredEnum enumStringRequired;

  @JsonKey(
    name: r'enumInteger',
    
    
    
  )
  EnumTestEnumIntegerEnum enumInteger;

  @JsonKey(
    name: r'enumNumber',
    
    
    
  )
  EnumTestEnumNumberEnum enumNumber;

  @JsonKey(
    name: r'outerEnum',
    
    
    
  )
  OuterEnum outerEnum;

  @JsonKey(
    name: r'outerEnumInteger',
    
    
    
  )
  OuterEnumInteger outerEnumInteger;

  @JsonKey(
    name: r'outerEnumDefaultValue',
    
    
    
  )
  OuterEnumDefaultValue outerEnumDefaultValue;

  @JsonKey(
    name: r'outerEnumIntegerDefaultValue',
    
    
    
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

  @override
  String toString() => toJson().toString();

  factory EnumTest.fromJson(Map<String, dynamic> json) => _$EnumTestFromJson(json);
  Map<String, dynamic> toJson() => _$EnumTestToJson(this);
}


enum EnumTestEnumStringEnum {

    @JsonValue(r'UPPER')
    
    UPPER,
    @JsonValue(r'lower')
    
    lower,
    @JsonValue(r'')
    
    empty,

}



enum EnumTestEnumStringRequiredEnum {

    @JsonValue(r'UPPER')
    
    UPPER,
    @JsonValue(r'lower')
    
    lower,
    @JsonValue(r'')
    
    empty,

}



enum EnumTestEnumIntegerEnum {

    
    @JsonValue(1)
    number1,
    
    @JsonValue(-1)
    numberNegative1,

}



enum EnumTestEnumNumberEnum {

    
    @JsonValue('1.1')
    number1Period1,
    
    @JsonValue('-1.2')
    numberNegative1Period2,

}


