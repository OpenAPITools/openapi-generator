//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_type.dart';
import 'package:json_annotation/json_annotation.dart';

part 'apple_one_of_disc.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class AppleOneOfDisc {
  /// Returns a new [AppleOneOfDisc] instance.
  AppleOneOfDisc({
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
      other is AppleOneOfDisc &&
          other.seeds == seeds &&
          other.fruitType == fruitType;

  @override
  int get hashCode => seeds.hashCode + fruitType.hashCode;

  factory AppleOneOfDisc.fromJson(Map<String, dynamic> json) =>
      _$AppleOneOfDiscFromJson(json);

  Map<String, dynamic> toJson() => _$AppleOneOfDiscToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
