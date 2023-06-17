//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/entity_ref.dart';
import 'package:json_annotation/json_annotation.dart';

part 'foo_ref.g.dart';

// ignore_for_file: unused_import


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FooRef {
  /// Returns a new [FooRef] instance.
  FooRef({

     this.foorefPropA,

     this.href,

     this.id,

     this.atSchemaLocation,

     this.atBaseType,

    required  this.atType,
  });

  @JsonKey(
    
    name: r'foorefPropA',
    required: false,
    includeIfNull: false
  )


  final String? foorefPropA;



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
  bool operator ==(Object other) => identical(this, other) || other is FooRef &&
     other.foorefPropA == foorefPropA &&
     other.href == href &&
     other.id == id &&
     other.atSchemaLocation == atSchemaLocation &&
     other.atBaseType == atBaseType &&
     other.atType == atType;

  @override
  int get hashCode =>
    foorefPropA.hashCode +
    href.hashCode +
    id.hashCode +
    atSchemaLocation.hashCode +
    atBaseType.hashCode +
    atType.hashCode;

  factory FooRef.fromJson(Map<String, dynamic> json) => _$FooRefFromJson(json);

  Map<String, dynamic> toJson() => _$FooRefToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

