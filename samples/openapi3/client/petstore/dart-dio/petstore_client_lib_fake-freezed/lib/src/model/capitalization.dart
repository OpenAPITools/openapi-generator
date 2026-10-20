//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'capitalization.freezed.dart';
part 'capitalization.g.dart';


@freezed
abstract class Capitalization with _$Capitalization {
  const factory Capitalization({
    @JsonKey(
      name: r'smallCamel',
      required: false,
      includeIfNull: false,
    )
    String? smallCamel,
    @JsonKey(
      name: r'CapitalCamel',
      required: false,
      includeIfNull: false,
    )
    String? capitalCamel,
    @JsonKey(
      name: r'small_Snake',
      required: false,
      includeIfNull: false,
    )
    String? smallSnake,
    @JsonKey(
      name: r'Capital_Snake',
      required: false,
      includeIfNull: false,
    )
    String? capitalSnake,
    @JsonKey(
      name: r'SCA_ETH_Flow_Points',
      required: false,
      includeIfNull: false,
    )
    String? sCAETHFlowPoints,
    /// Name of the pet 
    @JsonKey(
      name: r'ATT_NAME',
      required: false,
      includeIfNull: false,
    )
    String? ATT_NAME,
  }) = _Capitalization;

  factory Capitalization.fromJson(Map<String, dynamic> json) =>
      _$CapitalizationFromJson(json);
}

