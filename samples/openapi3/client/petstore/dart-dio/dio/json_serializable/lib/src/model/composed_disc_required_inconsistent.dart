//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_type.dart';
import 'package:openapi/src/model/disc_optional_type_correct.dart';
import 'package:json_annotation/json_annotation.dart';

part 'composed_disc_required_inconsistent.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ComposedDiscRequiredInconsistent {
  /// Returns a new [ComposedDiscRequiredInconsistent] instance.
  ComposedDiscRequiredInconsistent({
    required this.fruitType,
  });

  @JsonKey(name: r'fruitType', required: true, includeIfNull: false)
  final String fruitType;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is ComposedDiscRequiredInconsistent && other.fruitType == fruitType;

  @override
  int get hashCode => fruitType.hashCode;

  factory ComposedDiscRequiredInconsistent.fromJson(
          Map<String, dynamic> json) =>
      _$ComposedDiscRequiredInconsistentFromJson(json);

  Map<String, dynamic> toJson() =>
      _$ComposedDiscRequiredInconsistentToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
