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
class EnumArrays {
  /// Returns a new [EnumArrays] instance.
  EnumArrays({
    this.justSymbol,
    this.arrayEnum = const [],
  });

  @JsonKey(
    nullable: false,
    name: r'just_symbol',
    required: false,
  )
  EnumArraysJustSymbolEnum justSymbol;

  @JsonKey(
    defaultValue: const [],
    name: r'array_enum',
    required: false,
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

  factory EnumArrays.fromJson(Map<String, dynamic> json) => _$EnumArraysFromJson(json);

  Map<String, dynamic> toJson() => _$EnumArraysToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}


enum EnumArraysJustSymbolEnum {
  greaterThanEqual,
  dollar,
}


enum EnumArraysArrayEnumEnum {
  fish,
  crab,
}

