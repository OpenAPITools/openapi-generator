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
class SpecialModelName {
  /// Returns a new [SpecialModelName] instance.
  SpecialModelName({
    this.dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket,
  });

  @JsonKey(
    name: r'dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket',
    
    
    
  )
  int dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket;

  @override
  bool operator ==(Object other) => identical(this, other) || other is SpecialModelName &&
     other.dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket == dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket;

  @override
  int get hashCode =>
    (dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket == null ? 0 : dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket.hashCode);

  @override
  String toString() => toJson().toString();

  factory SpecialModelName.fromJson(Map<String, dynamic> json) => _$SpecialModelNameFromJson(json);
  Map<String, dynamic> toJson() => _$SpecialModelNameToJson(this);
}

