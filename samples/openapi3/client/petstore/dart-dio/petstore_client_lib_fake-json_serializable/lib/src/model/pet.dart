//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/category.dart';
import 'package:openapi/src/model/tag.dart';
import 'package:json_annotation/json_annotation.dart';

part 'pet.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Pet {
  /// Returns a new [Pet] instance.
  Pet({

     this.id,

     this.category,

    required  this.name,

    required  this.photoUrls,

     this.tags,

     this.status,
  });

  @JsonKey(
    
    name: r'id',
    required: false,
    includeIfNull: false
  )


  final int? id;



  @JsonKey(
    
    name: r'category',
    required: false,
    includeIfNull: false
  )


  final Category? category;



  @JsonKey(
    
    name: r'name',
    required: true,
    includeIfNull: false
  )


  final String name;



  @JsonKey(
    
    name: r'photoUrls',
    required: true,
    includeIfNull: false
  )


  final Set<String> photoUrls;



  @JsonKey(
    
    name: r'tags',
    required: false,
    includeIfNull: false
  )


  final List<Tag>? tags;



      /// pet status in the store
  @JsonKey(
    
    name: r'status',
    required: false,
    includeIfNull: false
  )


  final PetStatusEnum? status;



  @override
  bool operator ==(Object other) => identical(this, other) || other is Pet &&
     other.id == id &&
     other.category == category &&
     other.name == name &&
     other.photoUrls == photoUrls &&
     other.tags == tags &&
     other.status == status;

  @override
  int get hashCode =>
    id.hashCode +
    category.hashCode +
    name.hashCode +
    photoUrls.hashCode +
    tags.hashCode +
    status.hashCode;

  factory Pet.fromJson(Map<String, dynamic> json) => _$PetFromJson(json);

  Map<String, dynamic> toJson() => _$PetToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

/// pet status in the store
enum PetStatusEnum {
  @JsonValue(r'available')
  available,
  @JsonValue(r'pending')
  pending,
  @JsonValue(r'sold')
  sold,
  @JsonValue(r'unknown_default_open_api')
  unknownDefaultOpenApi,
}


