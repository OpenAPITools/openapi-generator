//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/entity.dart';
import 'package:json_annotation/json_annotation.dart';

part 'pasta.g.dart';

// ignore_for_file: unused_import


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Pasta {
  /// Returns a new [Pasta] instance.
  Pasta({

     this.vendor,

     this.href,

     this.id,

     this.atSchemaLocation,

     this.atBaseType,

    required  this.atType,
  });

  @JsonKey(
    
    name: r'vendor',
    required: false,
    includeIfNull: false
  )


  final String? vendor;



      /// Hyperlink reference
  @JsonKey(
    
    name: r'href',
    required: false,
    includeIfNull: false
  )


  final String? href;



      /// unique identifier
  @JsonKey(
    
    name: r'id',
    required: false,
    includeIfNull: false
  )


  final String? id;



      /// A URI to a JSON-Schema file that defines additional attributes and relationships
  @JsonKey(
    
    name: r'@schemaLocation',
    required: false,
    includeIfNull: false
  )


  final String? atSchemaLocation;



      /// When sub-classing, this defines the super-class
  @JsonKey(
    
    name: r'@baseType',
    required: false,
    includeIfNull: false
  )


  final String? atBaseType;



      /// When sub-classing, this defines the sub-class Extensible name
  @JsonKey(
    
    name: r'@type',
    required: true,
    includeIfNull: false
  )


  final String atType;



  @override
  bool operator ==(Object other) => identical(this, other) || other is Pasta &&
     other.vendor == vendor &&
     other.href == href &&
     other.id == id &&
     other.atSchemaLocation == atSchemaLocation &&
     other.atBaseType == atBaseType &&
     other.atType == atType;

  @override
  int get hashCode =>
    vendor.hashCode +
    href.hashCode +
    id.hashCode +
    atSchemaLocation.hashCode +
    atBaseType.hashCode +
    atType.hashCode;

  factory Pasta.fromJson(Map<String, dynamic> json) => _$PastaFromJson(json);

  Map<String, dynamic> toJson() => _$PastaToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

