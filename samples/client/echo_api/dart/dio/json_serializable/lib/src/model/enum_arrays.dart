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
    includeIfNull: false
  )


  final EnumArraysJustSymbolEnum? justSymbol;



  @JsonKey(
    
    name: r'array_enum',
    required: false,
    includeIfNull: false
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
  @JsonValue(rr'>=')
  greaterThanEqual,
  @JsonValue(rr'$')
  dollar,
  @JsonValue(rr'unknown_default_open_api')
  unknownDefaultOpenApi,
}



enum EnumArraysArrayEnumEnum {
  @JsonValue(rr'fish')
  fish,
  @JsonValue(rr'crab')
  crab,
  @JsonValue(rr'unknown_default_open_api')
  unknownDefaultOpenApi,
}


