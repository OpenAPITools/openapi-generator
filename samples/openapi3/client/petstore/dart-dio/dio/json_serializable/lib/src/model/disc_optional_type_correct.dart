//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'disc_optional_type_correct.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class DiscOptionalTypeCorrect {
  /// Returns a new [DiscOptionalTypeCorrect] instance.
  DiscOptionalTypeCorrect({
    this.fruitType,
  });

  @JsonKey(name: r'fruitType', required: false, includeIfNull: false)
  final String? fruitType;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is DiscOptionalTypeCorrect && other.fruitType == fruitType;

  @override
  int get hashCode => fruitType.hashCode;

  factory DiscOptionalTypeCorrect.fromJson(Map<String, dynamic> json) =>
      _$DiscOptionalTypeCorrectFromJson(json);

  Map<String, dynamic> toJson() => _$DiscOptionalTypeCorrectToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
