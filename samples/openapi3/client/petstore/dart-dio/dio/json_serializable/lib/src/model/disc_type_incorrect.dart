//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'disc_type_incorrect.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class DiscTypeIncorrect {
  /// Returns a new [DiscTypeIncorrect] instance.
  DiscTypeIncorrect({
    required this.fruitType,
  });

  @JsonKey(name: r'fruitType', required: true, includeIfNull: false)
  final int fruitType;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is DiscTypeIncorrect && other.fruitType == fruitType;

  @override
  int get hashCode => fruitType.hashCode;

  factory DiscTypeIncorrect.fromJson(Map<String, dynamic> json) =>
      _$DiscTypeIncorrectFromJson(json);

  Map<String, dynamic> toJson() => _$DiscTypeIncorrectToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
