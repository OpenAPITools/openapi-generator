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
    nullable: false,
    name: r'smallCamel',
    required: false,
  )
  String smallCamel;

  @JsonKey(
    nullable: false,
    name: r'CapitalCamel',
    required: false,
  )
  String capitalCamel;

  @JsonKey(
    nullable: false,
    name: r'small_Snake',
    required: false,
  )
  String smallSnake;

  @JsonKey(
    nullable: false,
    name: r'Capital_Snake',
    required: false,
  )
  String capitalSnake;

  @JsonKey(
    nullable: false,
    name: r'SCA_ETH_Flow_Points',
    required: false,
  )
  String sCAETHFlowPoints;

      /// Name of the pet 
  @JsonKey(
    nullable: false,
    name: r'ATT_NAME',
    required: false,
  )
  String ATT_NAME;

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
    (smallCamel == null ? 0 : smallCamel.hashCode) +
    (capitalCamel == null ? 0 : capitalCamel.hashCode) +
    (smallSnake == null ? 0 : smallSnake.hashCode) +
    (capitalSnake == null ? 0 : capitalSnake.hashCode) +
    (sCAETHFlowPoints == null ? 0 : sCAETHFlowPoints.hashCode) +
    (ATT_NAME == null ? 0 : ATT_NAME.hashCode);

  factory Capitalization.fromJson(Map<String, dynamic> json) => _$CapitalizationFromJson(json);

  Map<String, dynamic> toJson() => _$CapitalizationToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

