//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/pizza.dart';
import 'package:json_annotation/json_annotation.dart';

part 'pizza_speziale.g.dart';

// ignore_for_file: unused_import


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class PizzaSpeziale {
  /// Returns a new [PizzaSpeziale] instance.
  PizzaSpeziale({

     this.toppings,

     this.href,

     this.id,

     this.atSchemaLocation,

     this.atBaseType,

    required  this.atType,
  });

  @JsonKey(
    
    name: r'toppings',
    required: false,
    includeIfNull: false
  )


  final String? toppings;



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
  bool operator ==(Object other) => identical(this, other) || other is PizzaSpeziale &&
     other.toppings == toppings &&
     other.href == href &&
     other.id == id &&
     other.atSchemaLocation == atSchemaLocation &&
     other.atBaseType == atBaseType &&
     other.atType == atType;

  @override
  int get hashCode =>
    toppings.hashCode +
    href.hashCode +
    id.hashCode +
    atSchemaLocation.hashCode +
    atBaseType.hashCode +
    atType.hashCode;

  factory PizzaSpeziale.fromJson(Map<String, dynamic> json) => _$PizzaSpezialeFromJson(json);

  Map<String, dynamic> toJson() => _$PizzaSpezialeToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

