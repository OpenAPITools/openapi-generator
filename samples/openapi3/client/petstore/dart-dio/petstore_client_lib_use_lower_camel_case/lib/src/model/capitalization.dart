//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
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

     this.smallcamel,

     this.capitalcamel,

     this.smallSnake,

     this.capitalSnake,

     this.scaEthFlowPoints,

     this.attName,
  });

  @JsonKey(
    
    name: r'smallCamel',
    required: false,
    includeIfNull: false,
  )


  final String? smallcamel;



  @JsonKey(
    
    name: r'CapitalCamel',
    required: false,
    includeIfNull: false,
  )


  final String? capitalcamel;



  @JsonKey(
    
    name: r'small_Snake',
    required: false,
    includeIfNull: false,
  )


  final String? smallSnake;



  @JsonKey(
    
    name: r'Capital_Snake',
    required: false,
    includeIfNull: false,
  )


  final String? capitalSnake;



  @JsonKey(
    
    name: r'SCA_ETH_Flow_Points',
    required: false,
    includeIfNull: false,
  )


  final String? scaEthFlowPoints;



      /// Name of the pet 
  @JsonKey(
    
    name: r'ATT_NAME',
    required: false,
    includeIfNull: false,
  )


  final String? attName;





    @override
    bool operator ==(Object other) => identical(this, other) || other is Capitalization &&
      other.smallcamel == smallcamel &&
      other.capitalcamel == capitalcamel &&
      other.smallSnake == smallSnake &&
      other.capitalSnake == capitalSnake &&
      other.scaEthFlowPoints == scaEthFlowPoints &&
      other.attName == attName;

    @override
    int get hashCode =>
        smallcamel.hashCode +
        capitalcamel.hashCode +
        smallSnake.hashCode +
        capitalSnake.hashCode +
        scaEthFlowPoints.hashCode +
        attName.hashCode;

  factory Capitalization.fromJson(Map<String, dynamic> json) => _$CapitalizationFromJson(json);

  Map<String, dynamic> toJson() => _$CapitalizationToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

