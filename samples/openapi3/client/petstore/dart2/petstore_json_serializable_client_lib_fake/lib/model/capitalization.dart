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
class Capitalization {
  /// Returns a new [Capitalization] instance.
  Capitalization({
    this.smallCamel,
    this.capitalCamel,
    this.smallSnake,
    this.capitalSnake,
    this.sCAETHFlowPoints,
    this.ATT_NAME,
  });


  @JsonKey(
    name: r'smallCamel',
    required: false,
  )
  String? smallCamel;

  @JsonKey(
    name: r'CapitalCamel',
    required: false,
  )
  String? capitalCamel;

  @JsonKey(
    name: r'small_Snake',
    required: false,
  )
  String? smallSnake;

  @JsonKey(
    name: r'Capital_Snake',
    required: false,
  )
  String? capitalSnake;

  @JsonKey(
    name: r'SCA_ETH_Flow_Points',
    required: false,
  )
  String? sCAETHFlowPoints;

  /// Name of the pet 
  @JsonKey(
    name: r'ATT_NAME',
    required: false,
  )
  String? ATT_NAME;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Capitalization &&
     other.smallCamel == smallCamel &&
     other.capitalCamel == capitalCamel &&
     other.smallSnake == smallSnake &&
     other.capitalSnake == capitalSnake &&
     other.sCAETHFlowPoints == sCAETHFlowPoints &&
     other.ATT_NAME == ATT_NAME;

  @override
  int get hashCode =>
    smallCamel.hashCode +
    capitalCamel.hashCode +
    smallSnake.hashCode +
    capitalSnake.hashCode +
    sCAETHFlowPoints.hashCode +
    ATT_NAME.hashCode;

  factory Capitalization.fromJson(Map<String, dynamic> json) => _$CapitalizationFromJson(json);

  Map<String, dynamic> toJson() => _$CapitalizationToJson(this);

  @override
  String toString() => toJson().toString();
}

