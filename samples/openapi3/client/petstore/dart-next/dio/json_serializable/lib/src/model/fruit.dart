//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/apple.dart';
import 'package:openapi/src/model/banana.dart';
import 'package:json_annotation/json_annotation.dart';

part 'fruit.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Fruit {
  /// Returns a new [Fruit] instance.
  Fruit({
    this.color,
    this.kind,
    this.count,
  });

  @JsonKey(name: r'color', required: false, includeIfNull: false)
  final String? color;

  @JsonKey(name: r'kind', required: false, includeIfNull: false)
  final String? kind;

  @JsonKey(name: r'count', required: false, includeIfNull: false)
  final num? count;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Fruit &&
          other.color == color &&
          other.kind == kind &&
          other.count == count;

  @override
  int get hashCode => color.hashCode + kind.hashCode + count.hashCode;

  factory Fruit.fromJson(Map<String, dynamic> json) => _$FruitFromJson(json);

  Map<String, dynamic> toJson() => _$FruitToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
