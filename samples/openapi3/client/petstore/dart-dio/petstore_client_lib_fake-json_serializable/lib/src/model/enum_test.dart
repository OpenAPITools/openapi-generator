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
    includeIfNull: false,
  unknownEnumValue: EnumTestEnumStringEnum.unknownDefaultOpenApi,
  )


  final EnumTestEnumStringEnum? enumString;



  @JsonKey(
    
    name: r'enum_string_required',
    required: true,
    includeIfNull: false,
  unknownEnumValue: EnumTestEnumStringRequiredEnum.unknownDefaultOpenApi,
  )


  final EnumTestEnumStringRequiredEnum enumStringRequired;



  @JsonKey(
    
    name: r'enum_integer',
    required: false,
    includeIfNull: false,
  unknownEnumValue: EnumTestEnumIntegerEnum.unknownDefaultOpenApi,
  )


  final EnumTestEnumIntegerEnum? enumInteger;



  @JsonKey(
    
    name: r'enum_number',
    required: false,
    includeIfNull: false,
  unknownEnumValue: EnumTestEnumNumberEnum.unknownDefaultOpenApi,
  )


  final EnumTestEnumNumberEnum? enumNumber;



  @JsonKey(
    
    name: r'outerEnum',
    required: false,
    includeIfNull: false,
  unknownEnumValue: OuterEnum.unknownDefaultOpenApi,
  )


  final OuterEnum? outerEnum;



  @JsonKey(
    
    name: r'outerEnumInteger',
    required: false,
    includeIfNull: false,
  unknownEnumValue: OuterEnumInteger.unknownDefaultOpenApi,
  )


  final OuterEnumInteger? outerEnumInteger;



  @JsonKey(
    
    name: r'outerEnumDefaultValue',
    required: false,
    includeIfNull: false,
  unknownEnumValue: OuterEnumDefaultValue.unknownDefaultOpenApi,
  )


  final OuterEnumDefaultValue? outerEnumDefaultValue;



  @JsonKey(
    
    name: r'outerEnumIntegerDefaultValue',
    required: false,
    includeIfNull: false,
  unknownEnumValue: OuterEnumIntegerDefaultValue.unknownDefaultOpenApi,
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
number1Period1(''1.1''),
@JsonValue('-1.2')
numberNegative1Period2(''-1.2''),
@JsonValue('11184809')
unknownDefaultOpenApi(''11184809'');

const EnumTestEnumNumberEnum(this.value);

final String value;

@override
String toString() => value;
}


