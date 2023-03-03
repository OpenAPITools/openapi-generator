//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'special_cat_all_of.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class SpecialCatAllOf {
  /// Returns a new [SpecialCatAllOf] instance.
  SpecialCatAllOf({

     this.kind,
  });

  @JsonKey(
    
    name: r'kind',
    required: false,
    includeIfNull: false
  )


  final SpecialCatAllOfKindEnum? kind;



  @override
  bool operator ==(Object other) => identical(this, other) || other is SpecialCatAllOf &&
     other.kind == kind;

  @override
  int get hashCode =>
    kind.hashCode;

  factory SpecialCatAllOf.fromJson(Map<String, dynamic> json) => _$SpecialCatAllOfFromJson(json);

  Map<String, dynamic> toJson() => _$SpecialCatAllOfToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}


enum SpecialCatAllOfKindEnum {
  @JsonValue(r'lions')
  lions,
  @JsonValue(r'tigers')
  tigers,
  @JsonValue(r'leopards')
  leopards,
  @JsonValue(r'jaguars')
  jaguars,
  @JsonValue(r'unknown_default_open_api')
  unknownDefaultOpenApi,
}


