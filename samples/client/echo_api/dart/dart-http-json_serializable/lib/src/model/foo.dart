//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/entity.dart';
import 'package:json_annotation/json_annotation.dart';

part 'foo.g.dart';

// ignore_for_file: unused_import

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Foo {
  /// Returns a new [Foo] instance.
  Foo({
    this.fooPropA,
    this.fooPropB,
    this.href,
    this.id,
    this.atSchemaLocation,
    this.atBaseType,
    required this.atType,
  });

  @JsonKey(name: r'fooPropA', required: false, includeIfNull: false)
  final String? fooPropA;

  @JsonKey(name: r'fooPropB', required: false, includeIfNull: false)
  final String? fooPropB;

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
      other is Foo &&
          other.fooPropA == fooPropA &&
          other.fooPropB == fooPropB &&
          other.href == href &&
          other.id == id &&
          other.atSchemaLocation == atSchemaLocation &&
          other.atBaseType == atBaseType &&
          other.atType == atType;

  @override
  int get hashCode =>
      fooPropA.hashCode +
      fooPropB.hashCode +
      href.hashCode +
      id.hashCode +
      atSchemaLocation.hashCode +
      atBaseType.hashCode +
      atType.hashCode;

  factory Foo.fromJson(Map<String, dynamic> json) => _$FooFromJson(json);

  Map<String, dynamic> toJson() => _$FooToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
