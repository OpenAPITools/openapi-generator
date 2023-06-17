//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'disc_optional_type_incorrect.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class DiscOptionalTypeIncorrect {
  /// Returns a new [DiscOptionalTypeIncorrect] instance.
  DiscOptionalTypeIncorrect({

     this.fruitType,
  });

  @JsonKey(
    
    name: r'fruitType',
    required: false,
    includeIfNull: false
  )


  final int? fruitType;



  @override
  bool operator ==(Object other) => identical(this, other) || other is DiscOptionalTypeIncorrect &&
     other.fruitType == fruitType;

  @override
  int get hashCode =>
    fruitType.hashCode;

  factory DiscOptionalTypeIncorrect.fromJson(Map<String, dynamic> json) => _$DiscOptionalTypeIncorrectFromJson(json);

  Map<String, dynamic> toJson() => _$DiscOptionalTypeIncorrectToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

