//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_type.dart';
import 'package:json_annotation/json_annotation.dart';

part 'banana_one_of_disc.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class BananaOneOfDisc {
  /// Returns a new [BananaOneOfDisc] instance.
  BananaOneOfDisc({
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
      other is BananaOneOfDisc &&
          other.length == length &&
          other.fruitType == fruitType;

  @override
  int get hashCode => length.hashCode + fruitType.hashCode;

  factory BananaOneOfDisc.fromJson(Map<String, dynamic> json) =>
      _$BananaOneOfDiscFromJson(json);

  Map<String, dynamic> toJson() => _$BananaOneOfDiscToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
