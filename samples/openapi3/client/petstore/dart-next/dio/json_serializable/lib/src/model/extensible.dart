//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'extensible.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Extensible {
  /// Returns a new [Extensible] instance.
  Extensible({
    this.atSchemaLocation,
    this.atBaseType,
    required this.atType,
  });

  /// A URI to a JSON-Schema file that defines additional attributes and relationships
  @JsonKey(name: r'@schemaLocation', required: false, includeIfNull: false)
  final String? atSchemaLocation;

  /// When sub-classing, this defines the super-class
  @JsonKey(name: r'@baseType', required: false, includeIfNull: false)
  final String? atBaseType;

  /// When sub-classing, this defines the sub-class Extensible name
  @JsonKey(name: r'@type', required: true, includeIfNull: false)
  final String atType;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Extensible &&
          other.atSchemaLocation == atSchemaLocation &&
          other.atBaseType == atBaseType &&
          other.atType == atType;

  @override
  int get hashCode =>
      atSchemaLocation.hashCode + atBaseType.hashCode + atType.hashCode;

  factory Extensible.fromJson(Map<String, dynamic> json) =>
      _$ExtensibleFromJson(json);

  Map<String, dynamic> toJson() => _$ExtensibleToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
