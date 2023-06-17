//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/banana_one_of_disc.dart';
import 'package:openapi/src/model/apple_one_of_disc.dart';
import 'package:json_annotation/json_annotation.dart';

part 'fruit_one_of_disc.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FruitOneOfDisc {
  /// Returns a new [FruitOneOfDisc] instance.
  FruitOneOfDisc({
    required this.fruitType,
  });

  @JsonKey(name: r'fruitType', required: true, includeIfNull: false)
  final String fruitType;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is FruitOneOfDisc && other.fruitType == fruitType;

  @override
  int get hashCode => fruitType.hashCode;

  factory FruitOneOfDisc.fromJson(Map<String, dynamic> json) =>
      _$FruitOneOfDiscFromJson(json);

  Map<String, dynamic> toJson() => _$FruitOneOfDiscToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
