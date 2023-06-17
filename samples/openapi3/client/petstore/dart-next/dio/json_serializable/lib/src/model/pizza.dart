//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/entity.dart';
import 'package:json_annotation/json_annotation.dart';

part 'pizza.g.dart';

// ignore_for_file: unused_import

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Pizza {
  /// Returns a new [Pizza] instance.
  Pizza({
    this.pizzaSize,
    this.href,
    this.id,
    this.atSchemaLocation,
    this.atBaseType,
    required this.atType,
  });

  @JsonKey(name: r'pizzaSize', required: false, includeIfNull: false)
  final num? pizzaSize;

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
      other is Pizza &&
          other.pizzaSize == pizzaSize &&
          other.href == href &&
          other.id == id &&
          other.atSchemaLocation == atSchemaLocation &&
          other.atBaseType == atBaseType &&
          other.atType == atType;

  @override
  int get hashCode =>
      pizzaSize.hashCode +
      href.hashCode +
      id.hashCode +
      atSchemaLocation.hashCode +
      atBaseType.hashCode +
      atType.hashCode;

  factory Pizza.fromJson(Map<String, dynamic> json) => _$PizzaFromJson(json);

  Map<String, dynamic> toJson() => _$PizzaToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
