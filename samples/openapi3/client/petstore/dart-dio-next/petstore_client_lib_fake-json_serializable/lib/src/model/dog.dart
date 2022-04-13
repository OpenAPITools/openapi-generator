//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:openapi/src/model/dog_all_of.dart';
import 'package:openapi/src/model/animal.dart';
import 'package:json_annotation/json_annotation.dart';

part 'dog.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Dog {
  /// Returns a new [Dog] instance.
  Dog({

    required  this.className,

     this.color = 'red',

     this.breed,
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
    
    name: r'breed',
    required: false,
    includeIfNull: false
  )


  final String? breed;



  @override
  bool operator ==(Object other) => identical(this, other) || other is Dog &&
     other.className == className &&
     other.color == color &&
     other.breed == breed;

  @override
  int get hashCode =>
    (className == null ? 0 : className.hashCode) +
    (color == null ? 0 : color.hashCode) +
    (breed == null ? 0 : breed.hashCode);

  factory Dog.fromJson(Map<String, dynamic> json) => _$DogFromJson(json);

  Map<String, dynamic> toJson() => _$DogToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

