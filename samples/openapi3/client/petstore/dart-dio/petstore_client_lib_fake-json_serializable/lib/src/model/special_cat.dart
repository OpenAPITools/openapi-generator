//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/cat.dart';
import 'package:json_annotation/json_annotation.dart';

part 'special_cat.g.dart';

// ignore_for_file: unused_import


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class SpecialCat {
  /// Returns a new [SpecialCat] instance.
  SpecialCat({

    required  this.className,

     this.color = 'red',

     this.declawed,

     this.kind,
  });

  @JsonKey(
    
    name: r'className',
    required: true,
    includeIfNull: false
  )


  final String className;



  @JsonKey(
    defaultValue: 'red',
    name: r'color',
    required: false,
    includeIfNull: false
  )


  final String? color;



  @JsonKey(
    
    name: r'declawed',
    required: false,
    includeIfNull: false
  )


  final bool? declawed;



  @JsonKey(
    
    name: r'kind',
    required: false,
    includeIfNull: false
  )


  final SpecialCatKindEnum? kind;



  @override
  bool operator ==(Object other) => identical(this, other) || other is SpecialCat &&
     other.className == className &&
     other.color == color &&
     other.declawed == declawed &&
     other.kind == kind;

  @override
  int get hashCode =>
    className.hashCode +
    color.hashCode +
    declawed.hashCode +
    kind.hashCode;

  factory SpecialCat.fromJson(Map<String, dynamic> json) => _$SpecialCatFromJson(json);

  Map<String, dynamic> toJson() => _$SpecialCatToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}


enum SpecialCatKindEnum {
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


