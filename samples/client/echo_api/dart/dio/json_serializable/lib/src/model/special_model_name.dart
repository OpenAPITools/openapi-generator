//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'special_model_name.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class SpecialModelName {
  /// Returns a new [SpecialModelName] instance.
  SpecialModelName({

     this.dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket,
  });

  @JsonKey(
    
    name: r'$special[property.name]',
    required: false,
    includeIfNull: false
  )


  final int? dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket;



  @override
  bool operator ==(Object other) => identical(this, other) || other is SpecialModelName &&
     other.dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket == dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket;

  @override
  int get hashCode =>
    dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket.hashCode;

  factory SpecialModelName.fromJson(Map<String, dynamic> json) => _$SpecialModelNameFromJson(json);

  Map<String, dynamic> toJson() => _$SpecialModelNameToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

