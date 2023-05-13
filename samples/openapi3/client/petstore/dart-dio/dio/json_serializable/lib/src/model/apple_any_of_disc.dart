//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_type.dart';
import 'package:json_annotation/json_annotation.dart';

part 'apple_any_of_disc.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class AppleAnyOfDisc {
  /// Returns a new [AppleAnyOfDisc] instance.
  AppleAnyOfDisc({
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
      other is AppleAnyOfDisc &&
          other.seeds == seeds &&
          other.fruitType == fruitType;

  @override
  int get hashCode => seeds.hashCode + fruitType.hashCode;

  factory AppleAnyOfDisc.fromJson(Map<String, dynamic> json) =>
      _$AppleAnyOfDiscFromJson(json);

  Map<String, dynamic> toJson() => _$AppleAnyOfDiscToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
