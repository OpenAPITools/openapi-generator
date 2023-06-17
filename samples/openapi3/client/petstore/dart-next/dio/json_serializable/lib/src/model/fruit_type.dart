//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'fruit_type.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FruitType {
  /// Returns a new [FruitType] instance.
  FruitType({
    required this.fruitType,
  });

  @JsonKey(name: r'fruitType', required: true, includeIfNull: false)
  final String fruitType;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is FruitType && other.fruitType == fruitType;

  @override
  int get hashCode => fruitType.hashCode;

  factory FruitType.fromJson(Map<String, dynamic> json) =>
      _$FruitTypeFromJson(json);

  Map<String, dynamic> toJson() => _$FruitTypeToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
