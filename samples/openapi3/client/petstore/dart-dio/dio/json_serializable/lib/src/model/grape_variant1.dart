//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'grape_variant1.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class GrapeVariant1 {
  /// Returns a new [GrapeVariant1] instance.
  GrapeVariant1({
    this.color,
  });

  @JsonKey(name: r'color', required: false, includeIfNull: false)
  final String? color;

  @override
  bool operator ==(Object other) =>
      identical(this, other) || other is GrapeVariant1 && other.color == color;

  @override
  int get hashCode => color.hashCode;

  factory GrapeVariant1.fromJson(Map<String, dynamic> json) =>
      _$GrapeVariant1FromJson(json);

  Map<String, dynamic> toJson() => _$GrapeVariant1ToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
