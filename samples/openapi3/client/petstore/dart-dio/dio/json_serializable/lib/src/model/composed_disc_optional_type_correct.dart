//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/disc_optional_type_correct.dart';
import 'package:json_annotation/json_annotation.dart';

part 'composed_disc_optional_type_correct.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ComposedDiscOptionalTypeCorrect {
  /// Returns a new [ComposedDiscOptionalTypeCorrect] instance.
  ComposedDiscOptionalTypeCorrect({

     this.fruitType,
  });

  @JsonKey(
    
    name: r'fruitType',
    required: false,
    includeIfNull: false
  )


  final String? fruitType;



  @override
  bool operator ==(Object other) => identical(this, other) || other is ComposedDiscOptionalTypeCorrect &&
     other.fruitType == fruitType;

  @override
  int get hashCode =>
    fruitType.hashCode;

  factory ComposedDiscOptionalTypeCorrect.fromJson(Map<String, dynamic> json) => _$ComposedDiscOptionalTypeCorrectFromJson(json);

  Map<String, dynamic> toJson() => _$ComposedDiscOptionalTypeCorrectToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

