//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'enum_arrays.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class EnumArrays {
  /// Returns a new [EnumArrays] instance.
  EnumArrays({

     this.justSymbol,

     this.arrayEnum,
  });

  @JsonKey(
    
    name: r'just_symbol',
    required: false,
    includeIfNull: false,
  unknownEnumValue: EnumArraysJustSymbolEnum.unknownDefaultOpenApi,
  )


  final EnumArraysJustSymbolEnum? justSymbol;



  @JsonKey(
    
    name: r'array_enum',
    required: false,
    includeIfNull: false,
  unknownEnumValue: List<EnumArraysArrayEnumEnum>.unknownDefaultOpenApi,
  )


  final List<EnumArraysArrayEnumEnum>? arrayEnum;





    @override
    bool operator ==(Object other) => identical(this, other) || other is EnumArrays &&
      other.justSymbol == justSymbol &&
      other.arrayEnum == arrayEnum;

    @override
    int get hashCode =>
        justSymbol.hashCode +
        arrayEnum.hashCode;

  factory EnumArrays.fromJson(Map<String, dynamic> json) => _$EnumArraysFromJson(json);

  Map<String, dynamic> toJson() => _$EnumArraysToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

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


