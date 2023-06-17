//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/disc_missing_from_properties.dart';
import 'package:json_annotation/json_annotation.dart';

part 'composed_disc_missing_from_properties.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ComposedDiscMissingFromProperties {
  /// Returns a new [ComposedDiscMissingFromProperties] instance.
  ComposedDiscMissingFromProperties({
    this.length,
  });

  @JsonKey(name: r'length', required: false, includeIfNull: false)
  final int? length;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is ComposedDiscMissingFromProperties && other.length == length;

  @override
  int get hashCode => length.hashCode;

  factory ComposedDiscMissingFromProperties.fromJson(
          Map<String, dynamic> json) =>
      _$ComposedDiscMissingFromPropertiesFromJson(json);

  Map<String, dynamic> toJson() =>
      _$ComposedDiscMissingFromPropertiesToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
