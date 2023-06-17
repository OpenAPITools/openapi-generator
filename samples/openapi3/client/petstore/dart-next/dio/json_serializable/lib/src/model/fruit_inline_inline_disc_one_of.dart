//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_inline_inline_disc_one_of_one_of.dart';
import 'package:json_annotation/json_annotation.dart';

part 'fruit_inline_inline_disc_one_of.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FruitInlineInlineDiscOneOf {
  /// Returns a new [FruitInlineInlineDiscOneOf] instance.
  FruitInlineInlineDiscOneOf({
    required this.seeds,
    required this.fruitType,
  });

  @JsonKey(name: r'seeds', required: true, includeIfNull: false)
  final int seeds;

  @JsonKey(name: r'fruitType', required: true, includeIfNull: false)
  final String fruitType;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is FruitInlineInlineDiscOneOf &&
          other.seeds == seeds &&
          other.fruitType == fruitType;

  @override
  int get hashCode => seeds.hashCode + fruitType.hashCode;

  factory FruitInlineInlineDiscOneOf.fromJson(Map<String, dynamic> json) =>
      _$FruitInlineInlineDiscOneOfFromJson(json);

  Map<String, dynamic> toJson() => _$FruitInlineInlineDiscOneOfToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
