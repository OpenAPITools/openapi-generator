//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/outer_enum.dart';
import 'package:openapi/src/model/outer_enum_default_value.dart';
import 'package:openapi/src/model/outer_enum_integer.dart';
import 'package:openapi/src/model/outer_enum_integer_default_value.dart';
import 'package:json_annotation/json_annotation.dart';

part 'enum_test.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class EnumTest {
  /// Returns a new [EnumTest] instance.
  EnumTest({

     this.enumString,

    required  this.enumStringRequired,

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
    includeIfNull: false
  )


  final EnumTestEnumStringEnum? enumString;



  @JsonKey(
    
    name: r'enum_string_required',
    required: true,
    includeIfNull: false
  )


  final EnumTestEnumStringRequiredEnum enumStringRequired;



  @JsonKey(
    
    name: r'enum_integer',
    required: false,
    includeIfNull: false
  )


  final EnumTestEnumIntegerEnum? enumInteger;



  @JsonKey(
    
    name: r'enum_number',
    required: false,
    includeIfNull: false
  )


  final EnumTestEnumNumberEnum? enumNumber;



  @JsonKey(
    
    name: r'outerEnum',
    required: false,
    includeIfNull: false
  )


  final OuterEnum? outerEnum;



  @JsonKey(
    
    name: r'outerEnumInteger',
    required: false,
    includeIfNull: false
  )


  final OuterEnumInteger? outerEnumInteger;



  @JsonKey(
    
    name: r'outerEnumDefaultValue',
    required: false,
    includeIfNull: false
  )


  final OuterEnumDefaultValue? outerEnumDefaultValue;



  @JsonKey(
    
    name: r'outerEnumIntegerDefaultValue',
    required: false,
    includeIfNull: false
  )


  final OuterEnumIntegerDefaultValue? outerEnumIntegerDefaultValue;



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
    (outerEnum == null ? 0 : outerEnum.hashCode) +
    outerEnumInteger.hashCode +
    outerEnumDefaultValue.hashCode +
    outerEnumIntegerDefaultValue.hashCode;

  factory EnumTest.fromJson(Map<String, dynamic> json) => _$EnumTestFromJson(json);

  Map<String, dynamic> toJson() => _$EnumTestToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}


enum EnumTestEnumStringEnum {
  @JsonValue(r'UPPER')
  UPPER,
  @JsonValue(r'lower')
  lower,
  @JsonValue(r'')
  empty,
  @JsonValue(r'unknown_default_open_api')
  unknownDefaultOpenApi,
}



enum EnumTestEnumStringRequiredEnum {
  @JsonValue(r'UPPER')
  UPPER,
  @JsonValue(r'lower')
  lower,
  @JsonValue(r'')
  empty,
  @JsonValue(r'unknown_default_open_api')
  unknownDefaultOpenApi,
}



enum EnumTestEnumIntegerEnum {
  @JsonValue(1)
  number1,
  @JsonValue(-1)
  numberNegative1,
  @JsonValue(11184809)
  unknownDefaultOpenApi,
}



enum EnumTestEnumNumberEnum {
  @JsonValue('1.1')
  number1Period1,
  @JsonValue('-1.2')
  numberNegative1Period2,
  @JsonValue('11184809')
  unknownDefaultOpenApi,
}


