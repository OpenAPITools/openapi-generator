//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/animal.dart';
import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'dog.g.dart';

// ignore_for_file: unused_import


@CopyWith()
@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Dog extends Animal {
/// Returns a new [Dog] instance.
  Dog({
     this.breed,
    required  super.className,
     super.color = 'red',
  });

  @JsonKey(
    
    name: r'breed',
    required: false,
    includeIfNull: false,
  )


  final String? breed;





    @override
    bool operator ==(Object other) => identical(this, other) || other is Dog &&
    runtimeType == other.runtimeType &&
      other.className == className &&
      other.color == color &&
      other.breed == breed;

    @override
    int get hashCode =>
        className.hashCode +
        color.hashCode +
        breed.hashCode;

  factory Dog.fromJson(Map<String, dynamic> json) => _$DogFromJson(json);

  Map<String, dynamic> toJson() => _$DogToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

