//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'dog_all_of.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class DogAllOf {
  /// Returns a new [DogAllOf] instance.
  DogAllOf({

     this.breed,
  });

  @JsonKey(
    
    name: r'breed',
    required: false,
    includeIfNull: false
  )


  final String? breed;



  @override
  bool operator ==(Object other) => identical(this, other) || other is DogAllOf &&
     other.breed == breed;

  @override
  int get hashCode =>
    breed.hashCode;

  factory DogAllOf.fromJson(Map<String, dynamic> json) => _$DogAllOfFromJson(json);

  Map<String, dynamic> toJson() => _$DogAllOfToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

