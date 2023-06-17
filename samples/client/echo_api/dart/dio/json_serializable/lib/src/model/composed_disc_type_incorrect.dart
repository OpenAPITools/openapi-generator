//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/disc_type_incorrect.dart';
import 'package:json_annotation/json_annotation.dart';

part 'composed_disc_type_incorrect.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ComposedDiscTypeIncorrect {
  /// Returns a new [ComposedDiscTypeIncorrect] instance.
  ComposedDiscTypeIncorrect({

    required  this.fruitType,
  });

  @JsonKey(
    
    name: r'fruitType',
    required: true,
    includeIfNull: false
  )


  final int fruitType;



  @override
  bool operator ==(Object other) => identical(this, other) || other is ComposedDiscTypeIncorrect &&
     other.fruitType == fruitType;

  @override
  int get hashCode =>
    fruitType.hashCode;

  factory ComposedDiscTypeIncorrect.fromJson(Map<String, dynamic> json) => _$ComposedDiscTypeIncorrectFromJson(json);

  Map<String, dynamic> toJson() => _$ComposedDiscTypeIncorrectToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

