//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/disc_optional_type_incorrect.dart';
import 'package:json_annotation/json_annotation.dart';

part 'composed_disc_optional_type_incorrect.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ComposedDiscOptionalTypeIncorrect {
  /// Returns a new [ComposedDiscOptionalTypeIncorrect] instance.
  ComposedDiscOptionalTypeIncorrect({
    this.fruitType,
  });

  @JsonKey(name: r'fruitType', required: false, includeIfNull: false)
  final int? fruitType;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is ComposedDiscOptionalTypeIncorrect &&
          other.fruitType == fruitType;

  @override
  int get hashCode => fruitType.hashCode;

  factory ComposedDiscOptionalTypeIncorrect.fromJson(
          Map<String, dynamic> json) =>
      _$ComposedDiscOptionalTypeIncorrectFromJson(json);

  Map<String, dynamic> toJson() =>
      _$ComposedDiscOptionalTypeIncorrectToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
