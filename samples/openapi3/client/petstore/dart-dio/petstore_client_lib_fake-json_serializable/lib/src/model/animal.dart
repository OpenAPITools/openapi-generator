//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'animal.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Animal {
  /// Returns a new [Animal] instance.
  Animal({

    required  this.className,

     this.color = 'red',
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



  @override
  bool operator ==(Object other) => identical(this, other) || other is Animal &&
     other.className == className &&
     other.color == color;

  @override
  int get hashCode =>
    className.hashCode +
    color.hashCode;

  factory Animal.fromJson(Map<String, dynamic> json) => _$AnimalFromJson(json);

  Map<String, dynamic> toJson() => _$AnimalToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

