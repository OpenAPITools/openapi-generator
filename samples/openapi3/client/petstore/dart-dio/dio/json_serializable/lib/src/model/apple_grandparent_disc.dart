//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'apple_grandparent_disc.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class AppleGrandparentDisc {
  /// Returns a new [AppleGrandparentDisc] instance.
  AppleGrandparentDisc({
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
      other is AppleGrandparentDisc &&
          other.seeds == seeds &&
          other.fruitType == fruitType;

  @override
  int get hashCode => seeds.hashCode + fruitType.hashCode;

  factory AppleGrandparentDisc.fromJson(Map<String, dynamic> json) =>
      _$AppleGrandparentDiscFromJson(json);

  Map<String, dynamic> toJson() => _$AppleGrandparentDiscToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
