//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/apple_variant1.dart';
import 'package:openapi/src/model/grape_variant1.dart';
import 'package:json_annotation/json_annotation.dart';

part 'fruit_variant1.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FruitVariant1 {
  /// Returns a new [FruitVariant1] instance.
  FruitVariant1({
    this.color,
    this.kind,
  });

  @JsonKey(name: r'color', required: false, includeIfNull: false)
  final String? color;

  @JsonKey(name: r'kind', required: false, includeIfNull: false)
  final String? kind;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is FruitVariant1 && other.color == color && other.kind == kind;

  @override
  int get hashCode => color.hashCode + kind.hashCode;

  factory FruitVariant1.fromJson(Map<String, dynamic> json) =>
      _$FruitVariant1FromJson(json);

  Map<String, dynamic> toJson() => _$FruitVariant1ToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
