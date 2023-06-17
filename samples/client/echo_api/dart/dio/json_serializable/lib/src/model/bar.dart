//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/foo_ref_or_value.dart';
import 'package:openapi/src/model/entity.dart';
import 'package:json_annotation/json_annotation.dart';

part 'bar.g.dart';

// ignore_for_file: unused_import


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Bar {
  /// Returns a new [Bar] instance.
  Bar({

    required  this.id,

     this.barPropA,

     this.fooPropB,

     this.foo,

     this.href,

     this.atSchemaLocation,

     this.atBaseType,

    required  this.atType,
  });

  @JsonKey(
    
    name: r'id',
    required: true,
    includeIfNull: false
  )


  final String id;



  @JsonKey(
    
    name: r'barPropA',
    required: false,
    includeIfNull: false
  )


  final String? barPropA;



  @JsonKey(
    
    name: r'fooPropB',
    required: false,
    includeIfNull: false
  )


  final String? fooPropB;



  @JsonKey(
    
    name: r'foo',
    required: false,
    includeIfNull: false
  )


  final FooRefOrValue? foo;



      /// Hyperlink reference
  @JsonKey(
    
    name: r'href',
    required: false,
    includeIfNull: false
  )


  final String? href;



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
  bool operator ==(Object other) => identical(this, other) || other is Bar &&
     other.id == id &&
     other.barPropA == barPropA &&
     other.fooPropB == fooPropB &&
     other.foo == foo &&
     other.href == href &&
     other.atSchemaLocation == atSchemaLocation &&
     other.atBaseType == atBaseType &&
     other.atType == atType;

  @override
  int get hashCode =>
    id.hashCode +
    barPropA.hashCode +
    fooPropB.hashCode +
    foo.hashCode +
    href.hashCode +
    atSchemaLocation.hashCode +
    atBaseType.hashCode +
    atType.hashCode;

  factory Bar.fromJson(Map<String, dynamic> json) => _$BarFromJson(json);

  Map<String, dynamic> toJson() => _$BarToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

