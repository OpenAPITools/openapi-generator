//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'special_model_name.freezed.dart';
part 'special_model_name.g.dart';


@freezed
abstract class SpecialModelName with _$SpecialModelName {
  const factory SpecialModelName({
    @JsonKey(
      name: r'$special[property.name]',
      required: false,
      includeIfNull: false,
    )
    int? dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket,
  }) = _SpecialModelName;

  factory SpecialModelName.fromJson(Map<String, dynamic> json) =>
      _$SpecialModelNameFromJson(json);
}

