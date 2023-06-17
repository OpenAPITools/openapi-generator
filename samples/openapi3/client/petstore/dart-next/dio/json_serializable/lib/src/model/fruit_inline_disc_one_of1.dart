//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'fruit_inline_disc_one_of1.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FruitInlineDiscOneOf1 {
  /// Returns a new [FruitInlineDiscOneOf1] instance.
  FruitInlineDiscOneOf1({
    required this.length,
    required this.fruitType,
  });

  @JsonKey(name: r'length', required: true, includeIfNull: false)
  final int length;

  @JsonKey(name: r'fruitType', required: true, includeIfNull: false)
  final String fruitType;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is FruitInlineDiscOneOf1 &&
          other.length == length &&
          other.fruitType == fruitType;

  @override
  int get hashCode => length.hashCode + fruitType.hashCode;

  factory FruitInlineDiscOneOf1.fromJson(Map<String, dynamic> json) =>
      _$FruitInlineDiscOneOf1FromJson(json);

  Map<String, dynamic> toJson() => _$FruitInlineDiscOneOf1ToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
