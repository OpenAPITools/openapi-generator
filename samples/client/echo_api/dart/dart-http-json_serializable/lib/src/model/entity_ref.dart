//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'entity_ref.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class EntityRef {
  /// Returns a new [EntityRef] instance.
  EntityRef({
    this.name,
    this.atReferredType,
    this.href,
    this.id,
    this.atSchemaLocation,
    this.atBaseType,
    required this.atType,
  });

  /// Name of the related entity.
  @JsonKey(name: r'name', required: false, includeIfNull: false)
  final String? name;

  /// The actual type of the target instance when needed for disambiguation.
  @JsonKey(name: r'@referredType', required: false, includeIfNull: false)
  final String? atReferredType;

  /// Hyperlink reference
  @JsonKey(name: r'href', required: false, includeIfNull: false)
  final String? href;

  /// unique identifier
  @JsonKey(name: r'id', required: false, includeIfNull: false)
  final String? id;

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
      other is EntityRef &&
          other.name == name &&
          other.atReferredType == atReferredType &&
          other.href == href &&
          other.id == id &&
          other.atSchemaLocation == atSchemaLocation &&
          other.atBaseType == atBaseType &&
          other.atType == atType;

  @override
  int get hashCode =>
      name.hashCode +
      atReferredType.hashCode +
      href.hashCode +
      id.hashCode +
      atSchemaLocation.hashCode +
      atBaseType.hashCode +
      atType.hashCode;

  factory EntityRef.fromJson(Map<String, dynamic> json) =>
      _$EntityRefFromJson(json);

  Map<String, dynamic> toJson() => _$EntityRefToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
