//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'name.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Name {
  /// Returns a new [Name] instance.
  Name({

    required  this.name,

     this.snakeCase,

     this.property,

     this.n123number,
  });

  @JsonKey(
    
    name: r'name',
    required: true,
    includeIfNull: false
  )


  final int name;



  @JsonKey(
    
    name: r'snake_case',
    required: false,
    includeIfNull: false
  )


  final int? snakeCase;



  @JsonKey(
    
    name: r'property',
    required: false,
    includeIfNull: false
  )


  final String? property;



  @JsonKey(
    
    name: r'123Number',
    required: false,
    includeIfNull: false
  )


  final int? n123number;



  @override
  bool operator ==(Object other) => identical(this, other) || other is Name &&
     other.name == name &&
     other.snakeCase == snakeCase &&
     other.property == property &&
     other.n123number == n123number;

  @override
  int get hashCode =>
    name.hashCode +
    snakeCase.hashCode +
    property.hashCode +
    n123number.hashCode;

  factory Name.fromJson(Map<String, dynamic> json) => _$NameFromJson(json);

  Map<String, dynamic> toJson() => _$NameToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

