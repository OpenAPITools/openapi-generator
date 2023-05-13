//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'fruit_inline_disc_one_of.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FruitInlineDiscOneOf {
  /// Returns a new [FruitInlineDiscOneOf] instance.
  FruitInlineDiscOneOf({
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
      other is FruitInlineDiscOneOf &&
          other.seeds == seeds &&
          other.fruitType == fruitType;

  @override
  int get hashCode => seeds.hashCode + fruitType.hashCode;

  factory FruitInlineDiscOneOf.fromJson(Map<String, dynamic> json) =>
      _$FruitInlineDiscOneOfFromJson(json);

  Map<String, dynamic> toJson() => _$FruitInlineDiscOneOfToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
