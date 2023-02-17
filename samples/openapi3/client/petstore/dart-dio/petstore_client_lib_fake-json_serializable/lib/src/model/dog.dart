//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/animal.dart';
import 'package:json_annotation/json_annotation.dart';

part 'dog.g.dart';

// ignore_for_file: unused_import


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Dog {
  /// Returns a new [Dog] instance.
  Dog({

    required  this.type,

     this.color = 'red',

     this.breed,
  });

  @JsonKey(
    
    name: r'type',
    required: true,
    includeIfNull: false
  )


  final String type;



  @JsonKey(
    defaultValue: 'red',
    name: r'color',
    required: false,
    includeIfNull: false
  )


  final String? color;



  @JsonKey(
    
    name: r'breed',
    required: false,
    includeIfNull: false
  )


  final String? breed;



  @override
  bool operator ==(Object other) => identical(this, other) || other is Dog &&
     other.type == type &&
     other.color == color &&
     other.breed == breed;

  @override
  int get hashCode =>
    type.hashCode +
    color.hashCode +
    breed.hashCode;

  factory Dog.fromJson(Map<String, dynamic> json) => _$DogFromJson(json);

  Map<String, dynamic> toJson() => _$DogToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

