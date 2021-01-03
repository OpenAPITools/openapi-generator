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
class EnumArrays {
  /// Returns a new [EnumArrays] instance.
  EnumArrays({
    this.justSymbol,
    this.arrayEnum = const [],
  });

  @JsonKey(
    name: r'justSymbol',
    
    
    
  )
  EnumArraysJustSymbolEnum justSymbol;

  @JsonKey(
    name: r'arrayEnum',
    
    defaultValue: const [],
    
  )
  List<EnumArraysArrayEnumEnum> arrayEnum;

  @override
  bool operator ==(Object other) => identical(this, other) || other is EnumArrays &&
     other.justSymbol == justSymbol &&
     other.arrayEnum == arrayEnum;

  @override
  int get hashCode =>
    (justSymbol == null ? 0 : justSymbol.hashCode) +
    (arrayEnum == null ? 0 : arrayEnum.hashCode);

  @override
  String toString() => toJson().toString();

  factory EnumArrays.fromJson(Map<String, dynamic> json) => _$EnumArraysFromJson(json);
  Map<String, dynamic> toJson() => _$EnumArraysToJson(this);
}


enum EnumArraysJustSymbolEnum {

    @JsonValue(r'>=')
    
    greaterThanEqual,
    @JsonValue(r'$')
    
    dollar,

}



enum EnumArraysArrayEnumEnum {

    @JsonValue(r'fish')
    
    fish,
    @JsonValue(r'crab')
    
    crab,

}


