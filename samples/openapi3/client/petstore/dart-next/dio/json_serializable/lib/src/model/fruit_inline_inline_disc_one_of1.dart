//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_inline_inline_disc_one_of_one_of.dart';
import 'package:json_annotation/json_annotation.dart';

part 'fruit_inline_inline_disc_one_of1.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FruitInlineInlineDiscOneOf1 {
  /// Returns a new [FruitInlineInlineDiscOneOf1] instance.
  FruitInlineInlineDiscOneOf1({
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
      other is FruitInlineInlineDiscOneOf1 &&
          other.length == length &&
          other.fruitType == fruitType;

  @override
  int get hashCode => length.hashCode + fruitType.hashCode;

  factory FruitInlineInlineDiscOneOf1.fromJson(Map<String, dynamic> json) =>
      _$FruitInlineInlineDiscOneOf1FromJson(json);

  Map<String, dynamic> toJson() => _$FruitInlineInlineDiscOneOf1ToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
