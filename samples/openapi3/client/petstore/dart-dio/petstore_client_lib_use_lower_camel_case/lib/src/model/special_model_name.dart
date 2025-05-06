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

     this.dollarSpecialleftSquareBracketPropertyperiodNamerightSquareBracket,
  });

  @JsonKey(
    
    name: r'$special[property.name]',
    required: false,
    includeIfNull: false,
  )


  final int? dollarSpecialleftSquareBracketPropertyperiodNamerightSquareBracket;





    @override
    bool operator ==(Object other) => identical(this, other) || other is SpecialModelName &&
      other.dollarSpecialleftSquareBracketPropertyperiodNamerightSquareBracket == dollarSpecialleftSquareBracketPropertyperiodNamerightSquareBracket;

    @override
    int get hashCode =>
        dollarSpecialleftSquareBracketPropertyperiodNamerightSquareBracket.hashCode;

  factory SpecialModelName.fromJson(Map<String, dynamic> json) => _$SpecialModelNameFromJson(json);

  Map<String, dynamic> toJson() => _$SpecialModelNameToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

