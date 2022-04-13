//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:json_annotation/json_annotation.dart';

part 'capitalization.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
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
    includeIfNull: false
  )


  final String? smallCamel;



  @JsonKey(
    
    name: r'CapitalCamel',
    required: false,
    includeIfNull: false
  )


  final String? capitalCamel;



  @JsonKey(
    
    name: r'small_Snake',
    required: false,
    includeIfNull: false
  )


  final String? smallSnake;



  @JsonKey(
    
    name: r'Capital_Snake',
    required: false,
    includeIfNull: false
  )


  final String? capitalSnake;



  @JsonKey(
    
    name: r'SCA_ETH_Flow_Points',
    required: false,
    includeIfNull: false
  )


  final String? sCAETHFlowPoints;



      /// Name of the pet 
  @JsonKey(
    
    name: r'ATT_NAME',
    required: false,
    includeIfNull: false
  )


  final String? ATT_NAME;



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

